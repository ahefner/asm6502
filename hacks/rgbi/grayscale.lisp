
(defpackage :rgbi
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :rgbi)

(defvar *path* #.*compile-file-pathname*)

(defparameter *mmc3-bank-config* 0
  "Used for upper bits of writes to $8000 via MMC3-BANK function")

(defun mmc3-bank (bank value)
  (poke (logior *mmc3-bank-config* bank) #x8000)
  (poke value #x8001))

(let* ((*context* (make-instance 'basic-context :address #x8000))
       (*default-pathname-defaults* *path*)
       (vblank-flag (zp 16))
       (bg-bank     (zp 17)))

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
  (ppuxy 0 0)
  (ldy (imm 4))
  (as/until :zero
    (ldx (imm 0))
    (as/until :zero
      (txa)
      (sta (mem +vram-io+))
      (inx))
    (dey))

  ;; Turn the screen back on
  (poke #b10001000 +ppu-cr1+)         ; BG CHR $0000, SPR CHR $1000
  (jsr 'wait-vblank)
  (jsr 'wait-vblank)

  (poke 0 +vram-scroll+)
  (sta (mem +vram-scroll+))
  (ppuaddr #x2000)

  (jsr 'wait-vblank)
  (cli)

  (with-label :loop

    (poke #x00 bg-bank)
    (jsr 'frame)

    (poke #x10 bg-bank)
    (jsr 'frame)

    (jmp (mem :loop)))


  (procedure frame
    (jsr 'next-bg-chr-bank)
    (poke #b10001000 +ppu-cr1+)   ; BG CHR $0000, SPR CHR $1000
    (poke #b00001000 +ppu-cr2+)   ; BG visible, SPR off, darken screen
    (jsr 'frame-irq-init)
    (jsr 'wait-vblank)
    (rts))

  (procedure wait-vblank
    (lda (imm 0))
    (sta vblank-flag)
    (as/until :not-zero
      (lda vblank-flag))
    (rts))

  ;; Program MMC IRQ counter
  (procedure frame-irq-init
    (poke 0 #xE000)                     ; Disable/ACK MMC3 IRQ
    (poke 60 #xC000)                     ; IRQ latch (countdown)
    (sta (mem #xC001))                  ; Transfer IRQ latch to counter
    (sta (mem #xE001))                  ; Enable scanline IRQ
    (rts))

  ;; Interrupt handlers
  (procedure vblank-handler
    (inc vblank-flag)
    (rti))

  ;; Program and increment background character bank
  (procedure next-bg-chr-bank
    (mmc3-bank 0 bg-bank)
    (inc bg-bank)
    (inc bg-bank)
    (emit-delay 300)
    (mmc3-bank 1 bg-bank)
    (inc bg-bank)
    (inc bg-bank)
    (rts))

  ;; Scanline IRQ
  (procedure irq-handler
    (poke 0 #xE000)                     ; ACK / Disable IRQ
;    (emit-delay 50)
    (jsr 'next-bg-chr-bank)
    ;;(poke #x88 +ppu-cr2+)               ; debug
    (poke 60 #xC000)                    ; IRQ latch (countdown)
    (sta (mem #xC001))                  ; Transfer IRQ latch to counter
    (poke 0 #xE001)                     ; Enable IRQ
    (rti))

  ;; Interrupt vectors
  (advance-to +nmi-vector+)
  (dw (label 'vblank-handler))
  (dw (label 'reset))
  (dw (label 'irq-handler))

  ;; Generate output file (TNROM, 32K PRG / 32K CHR)
  (write-ines "/tmp/rgbi.nes"
              (link *context*)
              :mapper 4
              :chr (concatenate 'vector
                                (ichr:encode-gif "grayscale-1.gif")
                                (ichr:encode-gif "grayscale-2.gif"))))
