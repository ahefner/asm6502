;;;; Another NES test. This one displays a page from the character ROM
;;;; and several gradient bars, with the intent of judging how
;;;; convincingly you can use interlacing to blend between the four
;;;; available palette colors. Turns out that blending between
;;;; adjacent hues almost always looks good, but blending between
;;;; luminances always looks somewhat flickery (depending on hue).
;;;; Pressing the direction pad changes the base hue. Tends to flicker
;;;; badly on emulators that aren't locked to the display refresh.

(defpackage :nes-test-2
  (:use :common-lisp :asm6502 :6502 :6502-modes :asm6502-nes :asm6502-utility))

(in-package :nes-test-2)

(defmacro emitting-program ((filename &rest rom-args) &body body)
  `(write-ines
    ,filename
    (link
     (let ((*context* (make-instance 'basic-context :address #x8000)))
       ,@body
       *context*))
    ,@rom-args))

(defvar *path* #.*compile-file-pathname*)

(defparameter *ticker* (zp 0))
(defparameter *color* (zp 1))
(defparameter *lastj* (zp 3))
(defparameter *jtmp* (zp 2))

(emitting-program ("/tmp/nes-test-2.nes" :chr (loadbin (merge-pathnames "test2.chr" *path*)))
  (set-label 'reset)
  (sei)
  (ldx (imm #xFF))
  (txs)
  (poke 0 *color*)
  (sta *lastj*)

  ;; Enable VBI and let PPU warm up
  (poke #b10010000 +ppu-cr1+)
  (poke #b00000000 +ppu-cr2+)
  (jsr (mem (label 'wait-for-vblank)))
  (jsr (mem (label 'wait-for-vblank)))
  (poke #b00010000 +ppu-cr1+)

  ;; Clear name tables
  (ppuaddr #x2000)
  (lda (imm #xff))
  (ldy (imm 8))
  (set-label :outer)
  (ldx (imm 0))
  (set-label :loop)
  (sta (mem +vram-io+))
  (inx)
  (bne (rel :loop))
  (dey)
  (bne (rel :outer))

  ;; Zero attribute table
  (ppuaddr #x23C0)
  (lda (imm 0))
  (ldx (imm 60))
  (set-label :loop)
  (sta (mem +vram-io+))
  (dex)
  (bne (rel :loop))

  ;; Display something
  (ppuaddr #x2063)
  (ldx (imm 10))
  (set-label :loop)
  (txa)
  (clc)
  (adc (imm #xCF))
  (sta (mem +vram-io+))
  (dex)
  (bne (rel :loop))

  ;; Show the character rom
  (lda (mem +ppu-status+))
  (ldx (imm 0))
  (set-label :loop)
  (txa)
  (anda (imm 15))
  (asif :zero
    (txa)
    (asl)
    (lda (imm 0))
    (adc (imm #x21))
    (sta (mem +vram-addr+))
    (txa)
    (asl)
    (clc)
    (adc (imm 8))
    (sta (mem +vram-addr+)))
  (stx (mem +vram-io+))
  (inx)
  (bne (rel :loop))

  ;; Draw gradient stripes
  (poke #b00000100 +ppu-cr1+)           ; vertical write mode
  (ldy (imm 6))
  (clc)
  (set-label :loop)
  (poke #x20 +vram-addr+)
  (tya)
  (adc (imm #x59))
  (sta (mem +vram-addr+))
  (jsr (mem (label 'write-stripe)))
  (dey)
  (bne (rel :loop))
  (poke #b00000000 +ppu-cr1+)           ; horizontal write mode
  (ppuaddr (+ #x23c0 0 6))              ; alternating attributes
  (poke #b01000000 +vram-io+)
  (poke #b00100000 +vram-io+)
  (ppuaddr (+ #x23c0 8 6))              ; alternating attributes
  (poke #b01000100 +vram-io+)
  (poke #b00101110 +vram-io+)
  (ppuaddr (+ #x23c0 16 6))
  (poke #b01000100 +vram-io+)
  (poke #b00101110 +vram-io+)
  (ppuaddr (+ #x23c0 24 6))
  (poke #b01000100 +vram-io+)
  (poke #b00001110 +vram-io+)

  ;; Select character bank, reset scroll, turn on the display
  ;;(poke 0 #x8000)
  ;;(poke 20 #x8001)
  ;;(poke 1 #x8000)
  ;;(poke 21 #x8001)

  (lda (imm 0))
  (pha)

  (set-label :mainloop)
  (jsr (mem (label 'configure-ppu)))
  (jsr (mem (label 'wait-for-vblank)))
  (jsr (mem (label 'process-input)))
  (jsr (mem (label 'program-palette)))

  (pla)
  (eor (imm 1))
  (pha)

  (asif :zero
    (clc)
    (ppuaddr #x3F0D)
    (poke *color* +vram-io+)
    (adc (imm 16))
    (sta (mem +vram-io+))
    (adc (imm 16))
    (sta (mem +vram-io+))

    (ppuaddr #x3F09)
    (lda *color*)
    (adc (imm 1))
    (anda (imm 63))
    (jsr (mem (label 'ramp3)))

    :else
    (ppuaddr #x3F0D)
    (lda *color*)
    (jsr (mem (label 'ramp3)))
    (ppuaddr #x3F09)
    (lda *color*)
    (jsr (mem (label 'ramp3))))

  (jmp (mem (label :mainloop)))

  ;;;; ----------------------------------------------------------------

  ;;; Read joypad
  (set-label 'process-input)
  (poke 1 +joypad-1+)
  (poke 0 +joypad-1+)
  (loop repeat 8 do (lda (mem +joypad-1+)) (lsr) (rol *jtmp*))
  (lda *jtmp*)
  (tax)
  (and 15)
  (sta *color*)
  (inc *color*)
  (eor *lastj*)
  ;; Test A Button
  (asl)
  (tay)
  (asif :carry
    (lda *lastj*)
    (asif :positive
      (brk) (db 1)))
  ;; Test B Button
  (tya)
  (asl)
  (asif :carry
    (lda *lastj*)
    (asl)
    (asif :positive
      (brk) (db 2)))
  ;; Almost done.
  (stx *lastj*)
  (rts)

  ;;; Program palette
  (set-label 'program-palette)
  (lda (mem +ppu-status+))
  (ppuaddr #x3F00)
  (clc)
  (lda *color*)
  (jsr (mem (label 'ramp)))
  (adc (imm 17))
  (anda (imm 63))                       ; tail call.
  (set-label 'ramp)
  (sta (mem +vram-io+))
  (set-label 'ramp3)
  (adc (imm 16))
  (sta (mem +vram-io+))
  (adc (imm 16))
  (sta (mem +vram-io+))
  (adc (imm 16))
  (sta (mem +vram-io+))
  (rts)

  ;;; Reset PPU state at end of vblank
  (set-label 'configure-ppu)
  (lda (mem +ppu-status+))
  (lda (imm 0))
  (sta (mem +vram-addr+))
  (sta (mem +vram-addr+))
  (sta (mem +vram-scroll+))
  (sta (mem +vram-scroll+))
  (poke #b10000000 +ppu-cr1+)           ; enable VBI, horizontal write mode
  (poke #b00001110 +ppu-cr2+)
  (rts)

  (set-label 'write-stripe)
  (clc)
  (ldx (imm 3))
  (set-label :loop)
  (txa)
  (adc (imm #xFB))
  (sta (mem +vram-io+))
  (sta (mem +vram-io+))
  (sta (mem +vram-io+))
  (sta (mem +vram-io+))
  (dex)
  (bne (rel :loop))
  (rts)

  ;;; VBI handler
  (set-label 'vblank)
  (pha)
  (poke 0 *ticker*)
  (pla)
  (rti)

  ;;; Wait for vertical blank
  (set-label 'wait-for-vblank)
  (poke 1 *ticker*)
  (set-label 'wait-for-vblank-1)
  (lda *ticker*)
  (bne (rel 'wait-for-vblank-1))
  (rts)

  (set-label 'irq)
  (rti)

  ;;; Interrupt Vectors
  (format t "~&Program size: ~D bytes~%" (- *origin* #x8000))
  (advance-to +nmi-vector+)
  (dw (label 'vblank))
  (dw (label 'reset))
  (dw (label 'irq)))
