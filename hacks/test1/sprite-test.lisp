
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

(defparameter vblank-flag (zp #xFF))

;;;; Code

(defun oam-flag-value (flag)
  (let ((tmp (assoc flag '((:fliph . 64)
                           (:flipv . 128)
                           (:fg . 0)
                           (:bg . 32)))))
    (when (null tmp)
      (error "Unknown flag ~A" flag))
    (cdr tmp)))

(defun sprite (x y tile palette &rest flags)
  (db y)
  (db tile)
  (db (logior palette (reduce #'+ (mapcar #'oam-flag-value flags))))
  (db x))

(align 256)
(set-label :oamdata1)
(let ((x 40)
      (y 80))
  (sprite (+ x 0) (+ y 0) #x00 1)
  (sprite (+ x 8) (+ y 0) #x01 1)
  (sprite (+ x 0) (+ y 8) #x10 0)
  (sprite (+ x 8) (+ y 8) #x11 0))
(align 256)

(align 256)
(set-label :oamdata2)
(let ((x 40)
      (y 80))
  (sprite (+ x 8) (+ y 0) #x00 1 :fliph)
  (sprite (+ x 0) (+ y 0) #x01 1 :fliph)
  (sprite (+ x 8) (+ y 8) #x10 0 :fliph)
  (sprite (+ x 0) (+ y 8) #x11 0 :fliph))
(align 256)

(procedure reset
  (sei)
  (cld)
  (poke #b00010000 +ppu-cr1+)         ; NMI off during init.
  (poke #b00000000 +ppu-cr2+)         ; Do turn the screen off too..
  (ldx (imm #xFF))                    ; Set stack pointer
  (txs)

  ;; Init sound hardware
  (poke 0 #x4015)                     ; Silence all channels
  (poke #x40 #x4017)                  ; Disable IRQ !!

  (as/until :negative (bita (mem +ppu-status+))) ; PPU warmup interval
  (as/until :negative (bita (mem +ppu-status+))) ; (two frames)

  (poke 0 vblank-flag)
  (poke #b10001000 +ppu-cr1+)         ; Enable NMI

  ;; Palette init
  (jsr 'wait-for-vblank)
  (jsr 'wait-for-vblank)
  (jsr 'wait-for-vblank)
  (ppuaddr #x3F00)
  (let ((bg #x1B))
    (dolist (color (list bg #x2D #x3D #x30  bg #x03 #x13 #x23
                         bg #x2D #x3D #x30  bg #x05 #x15 #x25
                         bg #x1d #x26 #x38  bg #x1d #x13 #x38))
     (poke color +vram-io+)))

  ;; Main loop - wait for vblank, reset PPU registers, do sprite DMA.
  (with-label :loop
    (ldx (imm 10))
    (as/until :zero
      (jsr 'wait-for-vblank)
      (poke 0 +spr-addr+)
      (poke (msb (label :oamdata1)) +sprite-dma+)
      (poke #b10001000 +ppu-cr1+)
      (poke #b00010100 +ppu-cr2+)
      (dex))
    (ldx (imm 10))
    (as/until :zero
      (jsr 'wait-for-vblank)
      (poke 0 +spr-addr+)
      (poke (msb (label :oamdata2)) +sprite-dma+)
      (poke #b10001000 +ppu-cr1+)
      (poke #b00010100 +ppu-cr2+)
      (dex))
    (jmp (mem :loop))))

(procedure wait-for-vblank
  (lda (imm 0))
  (sta vblank-flag)
  (as/until :not-zero (lda vblank-flag))
  (lda (imm 0))
  (sta vblank-flag)
  (rts))

(procedure brk-handler (rti))

(procedure vblank-handler
  (inc vblank-flag)
  (rti))

;;;; Interrupt vectors

(advance-to +nmi-vector+)
(dw (label 'vblank-handler))
(dw (label 'reset))
(dw (label 'brk-handler))

;;;; Write ROM image

(write-ines "/tmp/sprite1.nes"
            (link *context*)
            :chr (concatenate 'vector
                              (ichr:encode-chr (ichr:read-gif (asset-path "spr0.gif")))
                              (ichr:encode-chr (ichr:read-gif (asset-path "spr0.gif")))))
