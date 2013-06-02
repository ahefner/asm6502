
(defpackage :death-star
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :death-star)

(defvar *path* #.*compile-file-pathname*)

(let ((*context* (make-instance 'basic-context :address #x8000))
      (vblank-flag (zp 16)))

  ;; Entry point, machine init
  (set-label 'reset)
  (sei)
  (cld)
  (poke #x40 +papu-irq-ctrl+)           ; Disable APU frame IRQ
  (ldx (imm #xFF))                      ; Init stack pointer
  (txs)
  (poke #b00000000 +ppu-cr1+)           ; Disable NMI
  (poke #b00000000 +ppu-cr2+)           ; Disable rendering
  (poke #b00000000 +dmc-control+)

  (bita (mem +ppu-status+))             ; Clear vblank flag
  (as/until :negative                   ; Loop until high (vblank) bit set
    (bita (mem +ppu-status+)))

  ;; Build empty sprite table at $0200
  (lda (imm #xFF))
  (ldx (imm 0))
  (as/until :zero
    (sta (abx #x0200))
    (inx))

  ;; Kill time while PPU warms up..
  (ldy (imm 128))
  (ldx (imm 0))
  (as/until :zero
    (as/until :zero
      (dex))
    (dey))

  ;;; -- PPU should be ready now.. build the screen contents --

  (bita (mem +ppu-status+))              ; Wait for vblank again
  (as/until :negative
    (bita (mem +ppu-status+)))


  ;; Program palette
  (ppuaddr #x3F00)
  (loop repeat 4 do
        (poke #x0F +vram-io+)
        (poke #x2D +vram-io+)
        (poke #x00 +vram-io+)
        (poke #x3D +vram-io+))

  ;; Clear nametable $2000
  (ppuaddr #x2000)
  (lda (imm 255))                       ;tile # to clear nametable to
  (ldy (imm 30))                        ; Y counts down 30 rows
  (as/until :zero
    (ldx (imm 32))                      ; X counts down 32 columns
    (as/until :zero
      (sta (mem +vram-io+))
      (dex))
    (dey))
  ;; Clear attribute table
  (ldx (imm 64))
  (lda (imm 0))                         ; First BG palette
  (as/until :zero
    (sta (mem +vram-io+)))

  ;; Display character rom
  (ldx (imm 0))                         ; X counts char # from 0 upto 255
  (as/until :zero
    (txa)                               ; If lower 4-bits of char # are zero,
    (anda (imm #x0F))                   ; set PPU addr to new line
    (asif :zero                         ; ..
      (txa)                             ; upper-left is #x2108
      (asl)                             ; A = 32*line (line = top 4 bits of X)
      (lda (imm 0))                     ; (doing 16-bit addition of line*32 + #x2108)
      (adc (imm #x21))                  ; carry into MSB
      (sta (mem +vram-addr+))
      (txa)                             ; Compute line*32 again..
      (asl)                             ;
      (clc)                             ;
      (adc (imm 8))                     ; Compute LSB of final address
      (sta (mem +vram-addr+)))
    (stx (mem +vram-io+))
    (inx))

  ;; Turn the screen back on
  (poke #b10000000 +ppu-cr1+)           ; Enable NMI
  (jsr 'wait-vblank)
  (jsr 'wait-vblank)

  (poke 0 +vram-scroll+)
  (sta (mem +vram-scroll+))
  (ppuaddr #x2000)
  (poke #xE8 +ppu-cr2+)                  ; BG visible, SPR off, darken screen


  (with-label :loop
    ;; Even frames:
    (jsr 'wait-vblank)
    (poke #b10000000 +ppu-cr1+)         ; Background pattern table $0000

    ;; Odd frames:
    (jsr 'wait-vblank)
    (poke #b10010000 +ppu-cr1+)         ; Background pattern table $1000

    (jmp (mem :loop)))

  (jmp (mem *origin*))

  (procedure wait-vblank
    (lda (imm 0))
    (sta vblank-flag)
    (as/until :not-zero
      (lda vblank-flag))
    (rts))

  ;; Interrupt handlers
  (procedure vblank-handler
    (inc vblank-flag)
    (rti))

  (procedure brk-handler
    (rti))

  ;; Interrupt vectors
  (advance-to +nmi-vector+)
  (dw (label 'vblank-handler))
  (dw (label 'reset))
  (dw (label 'brk-handler))

  ;; Generate output file:
  (write-ines "/tmp/deathstar.nes"
              (link *context*)
              :chr (concatenate 'vector
                                (ichr:encode-chr
                                 (ichr:read-gif
                                  (merge-pathnames "chr1.gif" *path*)))
                                (ichr:encode-chr
                                 (ichr:read-gif
                                  (merge-pathnames "chr2.gif" *path*))))))