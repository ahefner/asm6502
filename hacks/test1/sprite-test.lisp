
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

(defun sprite (x y tile)
  (db y)
  (db tile)
  (db 0)
  (db x))

(align 256)
(set-label :oamdata)
(let ((x 40)
      (y 80))
  (sprite (+ x  0) (+ y  0) #x00)
  (sprite (+ x  8) (+ y  0) #x01)
  (sprite (+ x 16) (+ y  0) #x02)
  (sprite (+ x 24) (+ y  0) #x03)
  (sprite (+ x  0) (+ y  8) #x10)
  (sprite (+ x  8) (+ y  8) #x11))
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
  (dolist (color '(#x38 #x2D #x3D #x30  #x38 #x03 #x13 #x23
                   #x38 #x2D #x3D #x30  #x38 #x05 #x15 #x25
                   #x38 #x3F #x27 #x37))
    (poke color +vram-io+))

  ;; Main loop - wait for vblank, reset PPU registers, do sprite DMA.
  (with-label :loop
    (jsr 'wait-for-vblank)
    (poke 0 +spr-addr+)
    (poke (msb (label :oamdata)) +sprite-dma+)
    (poke #b10001000 +ppu-cr1+)
    (poke #b00010100 +ppu-cr2+)
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
