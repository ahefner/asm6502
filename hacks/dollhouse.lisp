;;;; This is a graphics demo. It horizontally scrolls a portion of
;;;; some artwork by Mark Ryden, in various colors, with some sinebobs
;;;; (?) and a wavy raster effect on top.  The art is converted from a
;;;; GIF file to an 8 KB character ROM during assembly. The image is
;;;; large enough that we need some timed code to switch pattern table
;;;; banks mid-frame.  It also incorporates an NSF file to provide
;;;; some crude music.

;;;; I'm no demo coder. Expect no 6502 wizardry here.

;;;; It targets a basic NROM board (32KB program, 8KB character). I
;;;; haven't had a chance to test on real hardware yet.

;;;; Somewhat embarrassingly, this demo reveals a bug in my own NES
;;;; emulator which I've yet to fix, where the frame timing is
;;;; apparently off by one scanline. Oops.

(defpackage :dollhouse-demo
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :dollhouse-demo)

(defvar *path* #.*compile-file-pathname*)

(let* ((global (make-instance 'basic-context :address #x8000))
       (*context* global)
       (num-wavy-lines (* 3 26))        ; Must be multiple of 3 !
       (log2-wavy-period 6)
       ;; Music:
       (music (binary-file (merge-pathnames "noisejam.nsf" *path*)))
       (music-init (mem #x8080))        ; Hardcoding these..
       (music-step (mem #x8084))
       ;; Variables:
       (sprite-table #x0300)
       (table-x      #x0200)
       (table-y      #x0240)
       (countdown   (zp #x58))
       (wave        (zp #x59))
       (wt-lsb      (zp #x60))
       (wt-msb      (zp #x61))
       (wt-get      (indi #x60))
       (fill-count  (zp #x91))
       (scroll-x    (zp #x92))
       (phase       (zp #x93))
       (ntaddr      (zp #x95))
       (top-ntaddr  (zp #x97))
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
    (dolist (color '(#x3F #x2D #x3D #x30  #x3F #x03 #x13 #x23
                     #x3F #x2D #x3D #x30  #x3F #x05 #x15 #x25
                     #x3F #x3F #x27 #x37))
      (poke color +vram-io+))

    (lda (imm 64))                      ; Initialize scroll variables
    (sta scroll-x)
    (lda (imm 0))
    (sta ntaddr)
    (sta wave)
    (lda (imm 159))
    (sta phase)

    (jsr 'initialize-sprites)

    (jsr 'update-sprites)

    (align 256 #xEA)                    ; Pad with NOPs to next page
    (poke #b10001000 +ppu-cr1+)         ; Enable NMI

    (with-label mainloop
      (macrolet ((repeat (times &body body)
                   `(progn
                      (lda countdown)
                      (pha)
                      (lda (imm ,(if (eql times 256)
                                     0
                                     times)))
                      (sta countdown)
                      (as/until :zero
                        ,@(if (and (= 1 (length body))
                                   (symbolp (first body)))
                              `((jsr ',(first body)))
                              body)
                        (dec countdown))
                      (pla)
                      (sta countdown)))
                 (scroll-panels ()
                   `(poke 0 phase))
                 (stop-panels ()
                   `(poke 159 phase)))

        ;; Show them the circle.
        (repeat 192 linearly)
        ;; Introduce the split, gently.
        (repeat 3
          (scroll-panels)
          (repeat 128 split-by-4)
          (repeat 128 linearly))
        ;; Compare/contrast:
        (repeat 2
         (scroll-panels)
         (repeat 128
           (jsr 'spin-apart)
           (jsr 'linearly))
         (repeat 128 linearly)
         (scroll-panels)
         (repeat 128 split-by-4)
         (repeat 128 linearly))
        (repeat 64 split-by-4)
        (repeat 96 linearly)
        ;; Change it up.
        (repeat 256 spin-apart)
        (repeat 96 linearly)
        ;; A little more intense..
        (repeat 5
         (repeat 24 split-by-4)
         (repeat 96 linearly))
        (repeat 88 split-by-4)
        ;; Slow down..
        (repeat 256
          (scroll-panels)
          (jsr 'split-by-4)
          (jsr 'spin-apart))
        (stop-panels)
        ;; Rest and repeat. Come a little unglued.
        (repeat 256 spin-apart)
        (repeat 253 spin-apart)

      (jmp (mem 'mainloop)))))

  (align 256)
  (procedure framestep
    (jsr 'wait-for-vblank)
    (lda (mem +ppu-status+))          ; Reset PPU address latch.
    (lda (imm (msb sprite-table)))    ; Sprite DMA transfer.
    (sta (mem +sprite-dma+))

    (lda (mem +ppu-status+))          ; Reset address latch, to be safe.
    (ldx phase)                       ; 'phase' steps through rate-pattern
    (lda scroll-x)                    ; Scroll horizontally..
    (clc)                             ; Update scroll-x by rate-pattern
    (adc (abx 'rate-pattern))         ; Add. Use the carry-out below!
    (sta scroll-x)
    (lda (abx 'rate-transition))      ; Update phase via transition function.
    (sta phase)
    (lda ntaddr)                      ; Carry into ntaddr
    (adc (imm 0))                     ; ** Carry in from ADC above. **
    (anda (imm #b00000001))           ; Carry toggles $2000/$2400
    ;; (*) Subtle: I had to bum an instruction out of the scanline kernel, so
    ;; I store this with bit 4 inverted, as the bottom half of the screen
    ;; requires.
    (eor (imm #b10011000))            ; NMI on, invert BG Pattern address! (*)
    (sta ntaddr)

    ;; This got a little messy. Arithmetic for the bottom half of the screen
    ;; is biased by 128, because it's easier than screwing with the overflow
    ;; flag (the wave offsets are signed). Must adjust the top half to match.
    ;; There's also the issue of the gap between the screen split and the wave
    ;; effect, which also needs the correct value from here ("top-ntaddr").
    (lda scroll-x)
    (clc)
    (adc (imm 128))
    (sta (mem +vram-scroll+))
    (lda ntaddr)                        ; Carry into NT address.
    (adc (imm 0))                       ; (okay if it carries again)
    (eor (imm #b00010000))              ; Flip pattern table address.
    (sta top-ntaddr)                    ; Reuse this after the split.
    (sta (mem +ppu-cr1+))

    (poke #b00011110 +ppu-cr2+)       ; BG and sprites on.
    (lda (imm 0))
    (sta (mem +vram-scroll+))

    (jsr 'update-sprites)

    ;; Switch pattern tables mid-frame:
    (emit-delay (+ (* 114 107) 30))
    (lda top-ntaddr)
    (eor (imm #b10010000))            ; Invert pattern bank.
    (sta (mem +ppu-cr1+))

    ;; In theory, the music player will run here.
    (emit-delay (round (* 113.66 14)))

    ;; Wavy effect
    (flet ((kernel ()
             (poke #b00011110 +ppu-cr2+)
             (clc)
             (lda wt-get)               ; Get wave offset
             (adc scroll-x)             ; Add to BG scroll position
             (sta (mem +vram-scroll+))
             (lda (imm 0))
             (sta (mem +vram-scroll+))
             (lda ntaddr)               ; Carry in. Effectively, scroll-x and
             (adc (imm 0))              ; ntaddr.0 form a 9-bit field.
             (sta (mem +ppu-cr1+))
             (poke #b00011110 +ppu-cr2+)))

      ;; Setup for wave effect.. load table pointer into zero page.
      (lda (imm (lsb (label 'wave-offsets))))
      (sta wt-lsb)
      (ldx wave)                        ; Increment wave counter..
      (inx)
      (stx wave)
      (txa)
      (anda (imm (1- (expt 2 log2-wavy-period)))) ; Modulo cycle length..
      (clc)
      (adc (imm (msb (label 'wave-offsets))))  ; Step MSB each frame..
      (sta wt-msb)

      (emit-delay 54)                   ; Realign with hblank

      (ldy (imm num-wavy-lines))

      (loop repeat (/ num-wavy-lines 3) do
            (print (list :kernel-cycles (counting-cycles (kernel) (dey))))
            (loop repeat 14 do (inc (zp 0)))
            (print (list :kernel-cycles (counting-cycles (kernel) (dey))))
            (nop)
            (loop repeat 14 do (inc (zp 0)))
            (print (list :kernel-cycles (counting-cycles (kernel) (dey))))
            (loop repeat 14 do (inc (zp 0))))

      ;; This version works okay, but unrolling it as above works better.
      #+(or)
      (with-label loop
        (timed-section (114) (kernel) (dey))
        (timed-section (114) (kernel) (dey))
        (timed-section (108) (kernel))
        ;; Fun fact: timed-section is a piece of crap.
        (cmp (zp 0))                    ; Burn 3 cycles.

        (dey)
        (bne 'loop)))

    ;; Shut the screen off. Need a few extra scanlines here to update
    ;; the sprite tables, and if I didn't shut it off, you'd notice
    ;; the background stopped waving.
    (loop repeat 10 do (nop))           ; Realign with hblank.. again.
    (poke 0 +ppu-cr2+)

    (rts))

  ;; This table controls the scrolling.
  (with-label rate-pattern
;    (loop repeat 256 do (db 0))

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
  (with-label rate-transition
    (loop for i from 0 below 160 do (db (1+ i)))
    (db 160))

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

  (procedure initialize-sprites
    ;; Initialize OAM mirror.
    (ldx (imm 0))
    (as/until :zero
      (poke 96 (abx sprite-table))      ; Y coordinate
      (inx)
      (txa)                             ; Alternate character #s
      (lsr)
      (lsr)                             ;
      (ora (imm #xFE))
      (sta (abx sprite-table))          ; Sprite index
      (inx)
      (poke 0 (abx sprite-table))       ; Attirubtes
      (inx)
      (poke 64 (abx sprite-table))      ; X coordinate
      (inx))
    ;; Initialize X/Y phase variables
    (ldx (imm 63))
    (lda (imm 0))
    (as/until :negative
      (sta (abx table-x))
      (sta (abx table-y))
      (clc)
      (adc (imm 4))
      (dex))
    (rts))

  (procedure linearly
    (jsr 'framestep)
    (ldx (imm 63))
    (as/until :negative
      (print
       (counting-cycles
        (inc (abx table-x))
        (inc (abx table-y))
        (dotimes (i 7) (nop))
        (dex))))
    (rts))

  (procedure spin-apart
    (jsr 'framestep)
    (ldx (imm 63))
    (as/until :negative
      (print
       (counting-cycles
        (inc (abx table-x))
        (txa)
        (anda (imm 1))
        (clc)
        (adc (abx table-x))
        (sta (abx table-x))
        (dotimes (i 3) (nop))
        (dex))))
    (rts))

  (procedure split-by-4
    (jsr 'framestep)
    (ldx (imm 63))
    (as/until :negative
      (print
       (counting-cycles
         (inc (abx table-y))
         (txa)
         (anda (imm 16))
         (lsr)
         (lsr)
         (lsr)
         (clc)
         (adc (abx table-x))
         (sta (abx table-x))
         (dex))))
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

  (align 256)
  (procedure update-sprites
    (ldx (imm 63))
    (as/until :negative
      (ldy (abx table-x))               ; Push sin[table_x[X]]
      (lda (aby 'sine-table))
      (pha)

      (lda (abx table-y))               ; Push sin[table_x[X]+64]
      (clc)
      (adc (imm 64))
      (tay)
      (lda (aby 'sine-table))
      (pha)

      (txa)                             ; A = X * 4
      (asl)
      (asl)
      (tay)                             ; Y = X * 4
      (pla)                             ; Pop cosine
      (clc)
      (adc (imm -10))                   ; Offset Y coordinate
      (sta (aby sprite-table))          ; Store in sprite Y coordinate
      (pla)                             ; Pop sine
      (sta (aby (+ 3 sprite-table)))    ; Store in sprite X coordinate
      (dex))                            ; Decrememnt sprite index
    (rts))

  (align 256)
  (with-label sine-table
    (loop for i from 0 below 256
          do (db (round (+ 124 (* 99 (sin (* 2 pi i 1/256))))))))

  (with-label wave-offsets
    (loop with nframes = (expt 2 log2-wavy-period)
          for frame from (1- nframes) downto 0 do
      (align 256)
      (emit
       ;; The wave loop counts from num-wavy-lines to 1. So, one extra here.
       (loop for line from num-wavy-lines downto 0
             with amp = 3
             collect (mod (round ( + 128 ; bias to fix carry
                                     (* amp (/ line 80)
                                        (sin (* 2 pi (/ (+ (* 0.3 (expt line 1.54)) frame)
                                                        nframes))))))
                          256)))))

  ;; Interrupt vectors:
  (advance-to +nmi-vector+)
  (dw (label 'vblank-handler))
  (dw (label 'reset))
  (dw (label 'brk-handler))

  ;; Create output file:
  (write-ines "/tmp/dollhouse.nes"
              (link global)
              :mirror-mode :vertical
              :chr (ichr:encode-chr
                    (ichr:read-gif
                     (merge-pathnames "ryden.gif" *path*)))))
