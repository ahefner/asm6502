
(defpackage :death-star
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :death-star)

(defvar *path* #.(truename *compile-file-pathname*))

(defparameter *mmc3-bank-config* 0
  "Used for upper bits of writes to $8000 via MMC3-BANK function")

(defun mmc3-bank (bank value)
  (poke (logior *mmc3-bank-config* bank) #x8000)
  (poke value #x8001))

(let* ((*context* (make-instance 'basic-context :address #x8000))
       (*default-pathname-defaults* *path*)
       (vblank-flag (zp 16)))

  ;; --- ENTRY POINT (assemble in last PRG bank) ---
  (advance-to #xE000)
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

  ;;; MMC3 init
  (mmc3-bank 0 0)                       ; PPU $0000
  (mmc3-bank 1 2)                       ; PPU $0800
  (mmc3-bank 2 15)                      ; PPU $1000
  (mmc3-bank 3 15)                      ; PPU $1400
  (mmc3-bank 4 15)                      ; PPU $1800
  (mmc3-bank 5 15)                      ; PPU $1C00
  (mmc3-bank 6 0)                       ; CPU $8000
  (mmc3-bank 7 1)                       ; CPU $A000
  (poke 0 #xE000)                       ; Disable IRQ

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
  (poke #b10001000 +ppu-cr1+)         ; BG CHR $0000, SPR CHR $1000
  (jsr 'wait-vblank)
  (jsr 'wait-vblank)

  (poke 0 +vram-scroll+)
  (sta (mem +vram-scroll+))
  (ppuaddr #x2000)
  (poke #xE8 +ppu-cr2+)                  ; BG visible, SPR off, darken screen

  (with-label :loop

    ;; Even frames:
    (jsr 'wait-vblank)
    (mmc3-bank 0 0)                       ; PPU $0000
    (mmc3-bank 1 2)                       ; PPU $0800
    (poke #b10001000 +ppu-cr1+)           ; BG CHR $0000, SPR CHR $1000

    ;; Odd frames:
    (jsr 'wait-vblank)
    (mmc3-bank 0 4)                       ; PPU $0000
    (mmc3-bank 1 6)                       ; PPU $0800
    (poke #b10001000 +ppu-cr1+)           ; BG CHR $0000, SPR CHR $1000

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

  (procedure irq-handler
    (poke 0 #xE000)                       ; ACK / Disable IRQ
    (rti))

  ;; Interrupt vectors
  (advance-to +nmi-vector+)
  (dw (label 'vblank-handler))
  (dw (label 'reset))
  (dw (label 'irq-handler))

  ;; Generate output file (TNROM, 32K PRG / 16K CHR)
  (write-ines "/tmp/deathstar.nes"
              (link *context*)
              :mapper 4
              :chr (concatenate 'vector
                                (ichr:encode-gif "chr1.gif")
                                (ichr:encode-gif "chr2.gif")
                                (ichr:encode-gif "chr1.gif")
                                (ichr:encode-gif "chr2.gif"))))
