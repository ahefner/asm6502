;;;; This is a very simple graphics demo. It scrolls horizontally a
;;;; portion of some artwork by Mark Ryden, in various colors.  This
;;;; is converted from a GIF file to an 8 KB character ROM during
;;;; assembly. The image is large enough that we need some timed code
;;;; to switch pattern table banks mid-frame.  It also incorporates an
;;;; NSF file to provide some simple music.

;;;; The music source (noisejam.mml) was compiled to an NSF externally
;;;; using MCK and NESASM. In theory you could swap other NSFs in
;;;; place of this one, provided you fix the player addresses
;;;; (music-init and music-step) to match the file header, and no
;;;; variables clash in the zero page.

;;;; It targets a basic NROM board (32KB program, 8KB character). I
;;;; haven't had a chance to test on real hardware yet.

;;;; Somewhat embarrassingly, this demo reveals a bug in my own NES
;;;; emulator which I've yet to fix, where the frame timing is
;;;; apparently off by one scanline. Oops.

(defpackage :ryden-demo
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :ryden-demo)

(defvar *path* #.*compile-file-pathname*)

(let* ((global (make-instance 'basic-context :address #x8000))
       (*context* global)
       ;; Music:
       (music (binary-file (merge-pathnames "noisejam.nsf" *path*)))
       (music-init (mem #x8080))        ; Hardcoding these..
       (music-step (mem #x8084))
       ;; Variables:
       (fill-count  (zp #x91))
       (scroll-x    (zp #x92))
       (phase       (zp #x93))
       (ntaddr      (zp #x95))
       (vblank-flag (zp #x96)))         ; Set by NMI handler.

  (emit music)

  (procedure reset
    (sei)
    (cld)
    (poke #b00010000 +ppu-cr1+)         ; NMI off during init.
    (ldx (imm #xFF))                    ; Set stack pointer
    (txs)

    ;; Initialize music player.
    (lda (imm 0))                       ; song 0
    (ldx (imm 0))                       ; NTSC
    (jsr music-init)

    (as/until :negative (bita (mem +ppu-status+))) ; PPU warmup interval
    (as/until :negative (bita (mem +ppu-status+))) ; (two frames)

    ;; Fill nametable.
    (ppuaddr #x2000)
    (jsr 'fill-nametable)
    (ppuaddr #x2400)
    (jsr 'fill-nametable)

    ;; Fill attribute table.
    (ppuaddr #x23C0)
    (ldy (imm 0))
    (lda (imm #b01010101))
    (jsr 'fill-attributes)
    (ppuaddr #x27C0)
    (ldy (imm #b10101010))
    (lda (imm #b11111111))
    (jsr 'fill-attributes)

    ;; Program palette.
    (ppuaddr #x3F00)
    (dolist (color '(#x3F #x2D #x3D #x30  #x3F #x05 #x15 #x25
                     #x3F #x2D #x3D #x30  #x3F #x03 #x13 #x23))
      (poke color +vram-io+))

    (lda (imm 64))                      ; Initialize scroll variables
    (sta scroll-x)
    (lda (imm 0))
    (sta ntaddr)
    (lda (imm 160))
    (sta phase)

    (align 256 #xEA)                    ; Pad with NOPs to next page
    (poke #b10001000 +ppu-cr1+)         ; Enable NMI

    (with-label mainloop
      (jsr 'wait-for-vblank)

      (lda (mem +ppu-status+))          ; Reset address latch

      (ldx phase)                       ; 'phase' steps through rate-pattern
      (inx)
      (stx phase)
      (lda scroll-x)                    ; Scroll horizontally..
      (clc)                             ; Update scroll-x by rate-pattern
      (adc (abx 'rate-pattern))         ; Add. Use the carry-out below!
      (sta scroll-x)
      (sta (mem +vram-scroll+))         ; Set scroll registers
      (lda ntaddr)                      ; Carry into ntaddr
      (adc (imm 0))                     ; ** Carry in from ADC above. **
      (anda (imm 1))                    ; Carry toggles $2000/$2400
      (sta ntaddr)
      (ora (imm #b10001000))            ; NMI on, BG Pattern table $0000
      (sta (mem +ppu-cr1+))
      (poke #b00001110 +ppu-cr2+)       ; BG on, sprites off.
      (lda (imm 0))
      (sta (mem +vram-scroll+))
      ;; Switch pattern tables mid-frame:
      (emit-delay (+ (* 114 146) 178))
      (lda ntaddr)
      (ora (imm #b10011000))            ; NMI on, BG Pattern table $1000
      (sta (mem +ppu-cr1+))

      (jsr music-step)
      (jmp (mem 'mainloop))))

  ;; This table controls the scrolling.
  (with-label rate-pattern
    (let* ((tr '(1 0 1 0 1 0 1 0 0 1 0 0 1 0 0 0 1))
           (pattern (subseq
                     (append (loop repeat (- 128 (reduce '+ tr))
                                   collect 1)
                             tr
                             (loop repeat 256 collect 0))
                     0 256)))
      (assert (= 256 (length pattern)))
      (assert (= 128 (reduce #'+ pattern)))
      (map nil 'db pattern)))

  (procedure fill-nametable
    (lda (imm 30))
    (sta fill-count)
    (ldx (imm 0))
    (as/until :zero
      (txa)
      (ldy (imm 16))
      (as/until :zero
        (stx (mem +vram-io+))
        (inx)
        (dey))
      (tax)                             ; ..and once more.
      (ldy (imm 16))
      (as/until :zero
        (stx (mem +vram-io+))
        (inx)
        (dey))
      (dec fill-count)))

  ;; Wasteful, but the ROM is mostly empty anyway.
  (procedure fill-attributes
    (dotimes (y 8)
      (dotimes (x 4) (sty (mem +vram-io+)))
      (dotimes (x 4) (sta (mem +vram-io+))))
    (rts))

  (procedure wait-for-vblank
    (as/until :not-zero (lda vblank-flag))
    (lda (imm 0))
    (sta vblank-flag)
    (rts))

  (procedure brk-handler (rti))

  (procedure vblank-handler
    (inc vblank-flag)
    (rti))

  ;; Interrupt vectors:
  (advance-to +nmi-vector+)
  (dw (label 'vblank-handler))
  (dw (label 'reset))
  (dw (label 'brk-handler))

  ;; Create output file:
  (write-ines "/tmp/ryden.nes"
              (link global)
              :mirror-mode :vertical
              :chr (ichr:encode-chr
                    (ichr:read-gif
                     (merge-pathnames "ryden.gif" *path*)))))
