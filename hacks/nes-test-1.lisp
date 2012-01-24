;;;; This is a simple test of the assembler, targetting the NES, which
;;;; cycles the background color through each palette entry.

;;; This was the first test of the assembler, and doesn't use any of
;;; the fancy gimmicks added later (nor does it need to).

(defpackage :nes-test-1
  (:use :common-lisp :asm6502 :6502 :6502-modes :asm6502-nes :asm6502-utility))

(in-package :nes-test-1)

(write-ines
 "/tmp/nes-test-1.nes"
 (let ((*context* (make-instance 'basic-context :address #x8000))
       (color (zp 0)))

   ;; Program Entry Point
   (set-label 'entry-point)
   (sei)                                ; Disable interrupts
   (cld)
   (ldx (imm #xFF))                     ; Init stack pointer
   (txs)
   (lda (imm 3))
   (sta color)

   ;; Configure PPU
   (lda (imm #b10000000))               ; Enable VBlank NMI
   (sta (mem +ppu-cr1+))
   (lda (imm #b00000000))               ; Display off
   (sta (mem +ppu-cr2+))
   (jmp (mem *origin*))                 ; Spin.

   ;; VBlank Handler
   (set-label 'vblank-handler)
   (lda (mem +ppu-status+))             ; Clear NMI, reset high/low state
   (lda (imm #x3F))                     ; Program address #x3F00
   (sta (mem +vram-addr+))              ; ..write MSB
   (lda (imm #x00))
   (sta (mem +vram-addr+))              ; ..write LSB

   (inc color)                          ; Increment and load color
   (lda color)

   (lsr)                                ; Shift right two bits, so each
   (lsr)                                ; color appears for four frames.
   (sta (mem +vram-io+))                ; Write color to palete.

   (poke #x3F +vram-addr+)     ; Reset address due to palette latching.
   (poke #x00 +vram-addr+)
   (rti)

   ;; IRQ/Break Handler
   (set-label 'brk-handler)
   (rti)

   ;; Interrupt Vectors
   (advance-to +nmi-vector+)
   (dw (label 'vblank-handler)) ;; NMI
   (dw (label 'entry-point))    ;; RESET
   (dw (label 'brk-handler))    ;; BRK/IRQ

   (link *context*)))
