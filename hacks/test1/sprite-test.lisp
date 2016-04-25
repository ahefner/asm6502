
(defpackage :sprite-test-1
  (:use :common-lisp
        :6502
        :6502-modes
        :asm6502
        :asm6502-utility
        :asm6502-nes))

(in-package :sprite-test-1)

(defvar *path* #.*compile-file-pathname*)
(defun asset-path (filename) (merge-pathnames filename *path*))

;; I'm going to try this slightly differently this time..
(defparameter *global-context* (make-instance 'basic-context :address #x8000))
(setf *context* *global-context*)       ; yuck.

;;;; Globals

(defparameter status-update-vector (wordvar #xE0)
  "Status bar updates by END-FRAME are done by setting this code pointer.")

(defparameter *sprite-x* (zp #xF0))
(defparameter *sprite-y* (zp #xF1))
(defparameter *shadow-cr1* (zp #xF2))
(defparameter *shadow-cr2* (zp #xF3))
(defparameter *shadow-scroll-x* (zp #xF4))
(defparameter *shadow-scroll-y* (zp #xF5))

(defparameter *frame-counter* (zp #xFD)
  "Incremented each frame by NMI handler")

;;(defparameter *sprite-count* (zp #xFD))
(defparameter *oamidx* (zp #xFE))
(defparameter vblank-flag (zp #xFF))

(defparameter *oam-shadow* #x0200)


;;;; Code

(defun oam-flag-value (flag)
  (let ((tmp (assoc flag '((:fliph . 64)
                           (:flipv . 128)
                           (:fg . 0)
                           (:bg . 32)))))
    (when (null tmp)
      (error "Unknown flag ~A" flag))
    (cdr tmp)))

(defmacro defsprite (name &body body)
  `(procedure ,name ;;',(list :sprite name)
     (ldy *oamidx*)
     (mapcar
      (lambda (spec)
        (destructuring-bind (x y tile palette &rest flags) spec
          ;; Write Y coordinate
          (lda *sprite-y*)
          (unless (zerop y)
            (clc)
            (adc (imm y)))
          (sta (aby *oam-shadow*))
          (iny)
          ;; Write tile index
          (lda (imm tile))
          (sta (aby *oam-shadow*))
          (iny)
          ;; Write flags
          (lda (imm (logior palette (reduce #'+ (mapcar #'oam-flag-value flags)))))
          (sta (aby *oam-shadow*))
          (iny)
          ;; Write X coordinate
          (lda *sprite-x*)
          (unless (zerop x)
            (clc)
            (adc (imm x)))
          (sta (aby *oam-shadow*))
          (iny)))
      ',body)
     ;; Experiment for sprite muxing:
     (tya)
;;     (clc)
     (adc (imm (ash (- 11 1) 2)))        ; Subtract one, because we already incremented..
     (sta *oamidx*)
     ;; ..instead of..
;;     (sty *oamidx*)
     (rts)))

(defsprite (sage f 0)
  (0 0 #x00 1)
  (8 0 #x01 1)
  (0 8 #x10 0)
  (8 8 #x11 0))

(defsprite (sage f 1)
  (0 0 #x00 1)
  (8 0 #x01 1)
  (8 8 #x10 0 :fliph)
  (0 8 #x11 0 :fliph))

(defsprite (wiz f 0)
  (0 0 #x02 0)
  (8 0 #x03 0)
  (0 8 #x12 0)
  (8 8 #x13 0))

(procedure reset
  (sei)
  (cld)
  (poke #b00010000 +ppu-cr1+)         ; NMI off during init.
  (poke #b00000000 +ppu-cr2+)         ; Do turn the screen off too..
  (poke #x40 +papu-irq-ctrl+)
  (ldx (imm #xFF))                    ; Set stack pointer
  (txs)

  ;; Init sound hardware
  (poke 0 #x4015)                     ; Silence all channels
  (poke #x40 #x4017)                  ; Disable IRQ

  (bita (mem +ppu-status+))           ; clear vblank flag (reset glitch)
  (as/until :negative (bita (mem +ppu-status+))) ; PPU warmup interval
  (as/until :negative (bita (mem +ppu-status+))) ; (two frames)

  (lda (mem +ppu-status+))            ; Clear vblank flag before enabling NMI!
  (poke #b10001000 +ppu-cr1+)         ; Enable NMI

  ;; Reset background
  (jsr 'wait-for-vblank)                ; Rendering is off but just to be safe.. (?..)
  (jsr 'reset-background)

  ;; Palette init
  (jsr 'wait-for-vblank)
  (ppuaddr #x3F00)
  (let ((bg #x1B))
    (dolist (color (list bg #x1D #x3D #x30  bg #x03 #x13 #x23
                         bg #x2D #x3D #x30  bg #x05 #x15 #x25
                         bg #x1d #x15 #x37  bg #x1d #x13 #x37))
     (poke color +vram-io+)))

  (jsr 'reset-frame-vectors)

  ;; Main loop - wait for vblank, reset PPU registers, do sprite DMA.
  (with-label :loop
    ;; FIXME: sprite chr table address behaving backward vs what I expect..
    (poke #b10010000 *shadow-cr1*)
    (poke #b00011110 *shadow-cr2*)
    (poke 0 *shadow-scroll-x*)
    (poke 0 *shadow-scroll-y*)

    (jsr 'reset-sprites)
    (poke 40 *sprite-y*)

    (poke 20 *sprite-x*)
    (jsr '(wiz f 0))

    (poke 40 *sprite-x*)
    (jsr '(sage f 0))

    (poke 80 *sprite-x*)
    (jsr '(wiz f 0))

    (poke 100 *sprite-x*)
    (jsr '(sage f 0))


    (poke 60 *sprite-y*)

    (poke 20 *sprite-x*)
    (jsr '(wiz f 0))

    (poke 40 *sprite-x*)
    (jsr '(sage f 0))

    (poke 80 *sprite-x*)
    (jsr '(wiz f 0))

    (poke 100 *sprite-x*)
    (lda (imm 16))
    (bita *frame-counter*)
    (asif :zero
     (jsr '(sage f 0))
     :else
     (jsr '(sage f 1)))

    (pokeword (label 'test-status-update) status-update-vector)

    (jsr 'end-frame)

    (jmp (mem :loop))))

(procedure reset-background
  ;; Fill first name table screen
  (lda (mem +ppu-status+))            ; Reset address latch
  (ppuaddr #x2000)
  (lda (imm 0))
  (ldy (imm 4))
  (as/until :zero
    (ldx (imm 0))
    (as/until :zero
      (sta (mem +vram-io+))
      (dex))
    (dey))
  (ppuaddr #x23C0)                      ; Attribute table
  (lda (imm 0))
  (ldx (imm 64))
  (as/until :zero
    (sta (mem +vram-io+))
    (dex))
  ;; Fill second screen
  (ppuaddr #x2400)
  (lda (imm 3))                         ; Distinguish color since it should be hidden now
  (ldy (imm 4))
  (as/until :zero
    (ldx (imm 0))
    (as/until :zero
      (sta (mem +vram-io+))
      (dex))
    (dey))
  (ppuaddr #x27C0)                      ; Attribute table
  (lda (imm 0))
  (ldx (imm 64))
  (as/until :zero
    (sta (mem +vram-io+))
    (dex))
  (jsr 'redraw-borders)
  (rts))

(procedure redraw-borders
  (flet ((draw (x y repeat value)
           (assert (<= 0 x 31))
           (assert (<= 0 y 29))
           (assert (< 0 repeat 256))
           (ppuxy x y)
           (cond
             ((= 1 repeat)
              (poke value +vram-io+))
             (t
              (lda (imm value))
              (ldx (imm repeat))
              (as/until :zero
                (sta (mem +vram-io+))
                (dex))))))
    (poke #x00 +ppu-cr1+)               ; Horizontal write mode
    (lda (mem +ppu-status+))
    ;; Status bar
    (draw 0 0 32 1)
    (draw 0 1 32 5)
    (draw 0 3 32 4)

    (poke #x84 +ppu-cr1+)               ; Vertical write mode
    (draw 0 0 30 1)
    (draw 1 0 30 1)
    (draw 30 0 30 1)
    (draw 31 0 30 1)

    ;;(draw 1 4 26 #x10)

    (poke #x80 +ppu-cr1+)               ; Return to horizontal write mode

    ;; Bevel edges of status bar
    (draw 1 1 1 11)
    (draw 1 2 1 8)
    (draw 1 3 1 7)
    (draw 30 1 1 12)
    (draw 30 2 1 9)
    (draw 30 3 1 10)
    )
  (rts))

(procedure end-frame
  "Complete processing for one frame. Move sprites, do pending VRAM writes, etc."
  (jsr 'wait-for-vblank)
  ;; If sprites are enabled, do DMA transfer
  (lda (imm #b00010000))                ; Bit 4 - sprite enable bit
  (bita *shadow-cr2*)
  (asif :not-zero
         (poke 0 +spr-addr+)
         (poke (msb *oam-shadow*) +sprite-dma+))
  (poke *shadow-cr1* +ppu-cr1+)
  (poke *shadow-cr2* +ppu-cr2+)
  (lda (mem +ppu-status+))                  ; Reset address latch

  (jmp status-update-vector)
  (set-label :continue-from-status-update) ; Default (idle) handler jumps here to try next vector


  (set-label :finish-vram-updates)      ; If we update VRAM, handler jumps here (we may be out of cycles for additional updates)
  (lda (mem +ppu-status+))                  ; Reset address latch
  (poke *shadow-scroll-x* +vram-scroll+)
  (poke *shadow-scroll-y* +vram-scroll+)
  (rts)

  (set-label 'no-status-update *global-context*)
  (jmp (mem :continue-from-status-update))

  (set-label 'test-status-update *global-context*)
  (ppuxy 2 2)
  (loop for code across (map-string "Hi there.") do (poke code +vram-io+))
  (pokeword (label 'no-status-update) status-update-vector)
  (jmp (mem :continue-from-status-update)))

(procedure reset-frame-vectors
  "Call this before first call to END-FRAME, to initialize shadows and vectors."
  (pokeword (label 'no-status-update) status-update-vector)
  (rts))

(procedure reset-sprites
  (ldx (imm 0))
  (lda (imm 255))
  (as/until :zero
    (sta (abx *oam-shadow*))
    (dex)))

(procedure wait-for-vblank
  (poke 0 vblank-flag)
  (as/until :not-zero (lda vblank-flag))
  (rts))

(procedure brk-handler (rti))

(procedure vblank-handler
  (php)
  (inc vblank-flag)
  (inc *frame-counter*)
  (plp)
  (rti))

(defparameter *character-mapping*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()`~-_+=[]{};:'\",.<>/?\\| ")

(defun map-string (string)
  (map 'vector (lambda (char) (+ #xA0 (position char *character-mapping*))) string))



;;;; End of program

(print (list :program-end *origin*
             :space-used (- *origin* #x8000)
             :space-remaining (- #xFFFA *origin*)))

;;;; Interrupt vectors

(advance-to +nmi-vector+)
(dw (label 'vblank-handler))
(dw (label 'reset))
(dw (label 'brk-handler))

;;;; Write ROM image

(write-ines "/tmp/sprite1.nes"
            (link *context*)
            :mirror-mode :vertical
            :chr (concatenate 'vector
                              (ichr:encode-chr (ichr:read-gif (asset-path "bg0.gif")))
                              (ichr:encode-chr (ichr:read-gif (asset-path "spr0.gif")))))
