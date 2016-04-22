
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



(defparameter *sprite-x* (zp #xF0))
(defparameter *sprite-y* (zp #xF1))
(defparameter *shadow-cr1* (zp #xF2))
(defparameter *shadow-cr2* (zp #xF3))
(defparameter *shadow-scroll-x* (zp #xF4))
(defparameter *shadow-scroll-y* (zp #xF5))

(defparameter *frame-counter* (zp #xFD)
  "Incremented each frame by NMI handler")
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
     (sty *oamidx*)
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
  (ldx (imm #xFF))                    ; Set stack pointer
  (txs)

  ;; Init sound hardware
  (poke 0 #x4015)                     ; Silence all channels
  (poke #x40 #x4017)                  ; Disable IRQ

  (as/until :negative (bita (mem +ppu-status+))) ; PPU warmup interval
  (as/until :negative (bita (mem +ppu-status+))) ; (two frames)

  (poke 0 vblank-flag)
  (lda (mem +ppu-status+))            ; Clear vblank flag before enabling NMI!
  (poke #b10001000 +ppu-cr1+)         ; Enable NMI

  ;; Reset background
  (jsr 'wait-for-vblank)
  ;; Fill first name table screen
  (lda (mem +ppu-status+))            ; Reset address latch
  (ppuaddr #x2000)
  (lda (imm 4))
  (ldy (imm 8))
  (as/until :zero
    (ldx (imm 0))
    (as/until :zero
      (sta (mem +vram-io+))
      (dex))
    (dey))
  ;; Fill second screen
  (ppuaddr #x2400)
  (lda (imm 5))
  (ldy (imm 8))
  (as/until :zero
    (ldx (imm 0))
    (as/until :zero
      (sta (mem +vram-io+))
      (dex))
    (dey))

  ;; Palette init
  (jsr 'wait-for-vblank)
  (ppuaddr #x3F00)
  (let ((bg #x1B))
    (dolist (color (list bg #x2D #x3D #x30  bg #x03 #x13 #x23
                         bg #x2D #x3D #x30  bg #x05 #x15 #x25
                         bg #x1d #x15 #x37  bg #x1d #x13 #x37))
     (poke color +vram-io+)))

  ;; Main loop - wait for vblank, reset PPU registers, do sprite DMA.
  (with-label :loop
    ;; FIXME: sprite chr table address behaving backward vs what I expect..
    (poke #b10010000 *shadow-cr1*)
    (poke #b00011100 *shadow-cr2*)
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

    ;; (poke 120 *sprite-x*)
    ;; (jsr '(wiz f 0))



    ;; (poke 80 *sprite-y*)

    ;; (poke 20 *sprite-x*)
    ;; (jsr '(wiz f 0))

    ;; (poke 40 *sprite-x*)
    ;; (jsr '(sage f 0))

    ;; (poke 80 *sprite-x*)
    ;; (jsr '(wiz f 0))

    ;; (poke 100 *sprite-x*)
    ;; (jsr '(sage f 0))

    ;; (poke 120 *sprite-x*)
    ;; (jsr '(wiz f 0))

    ;; (poke 140 *sprite-x*)
    ;; (jsr '(wiz f 0))

    ;; (poke 160 *sprite-x*)
    ;; (jsr '(wiz f 0))

    ;; (poke 180 *sprite-x*)
    ;; (jsr '(wiz f 0))

    (jsr 'end-frame)

    (jmp (mem :loop))))

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
  (poke *shadow-scroll-x* +vram-scroll+)
  (poke *shadow-scroll-y* +vram-scroll+)
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
