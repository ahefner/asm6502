;;;; This is a graphics demo. It horizontally scrolls a portion of
;;;; some artwork by Mark Ryden, in various colors, with some sinebobs
;;;; (?) and a wavy raster effect on top.  The art is converted from a
;;;; GIF file to an 8 KB character ROM during assembly. The image is
;;;; large enough that we need some timed code to switch pattern table
;;;; banks mid-frame.  It also incorporates a simple music player,
;;;; with the music described in a simple embedded DSL that compiles
;;;; down to a sequence of register writes to drive the 2A03 sound
;;;; hardware.

;;;; It targets a basic NROM board (32KB program, 8KB character,
;;;; vertical mirroring). I've tested it on an EPROM cart made from a
;;;; Gyromite board.

;;;; I'm neither a demo coder nor particularly experienced at 6502
;;;; assembly language, so expect nothing clever here.

;;;; (Somewhat embarrassingly, this demo reveals a bug in my own NES
;;;; emulator which I've yet to fix, where the frame timing is
;;;; apparently off by one scanline. Oops.)

(defpackage :dollhouse-demo
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes :nesmus))

(in-package :dollhouse-demo)

(defvar *path* #.*compile-file-pathname*)

;;;; *********************************************************************
;;;;  ( You are now about to witness the strength of street knowledge. ;)

(let* ((global (make-instance 'basic-context :address #x8000))
       (*context* global)
       (num-wavy-lines (* 3 26))        ; Must be multiple of 3 !
       (log2-wavy-period 5)

       ;; Music player:
       (mfr-addr #x40)
       (mfr-get (indi mfr-addr))
       (mptr #x42)
       (mptr-msb  (zp (1+ mptr)))
       (mptr-lsb  (zp mptr))
       (log2-song-length 4)             ; Base 2 log of song length.
       (write-patterns (make-hash-table :test 'equal))
       (music-sequence '())

       ;; Variables (graphics):
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

  ;; Step music playback. Advances MPTR.
  (procedure player-step

    ;; Transfer *MPTR to MFR and play this frame.
    (ldy (imm 0))                       ; LSB of new music frame pointer
    (lda (indi mptr))
    (sta (zp mfr-addr))
    (iny)                               ; MSB of new music frame pointer
    (lda (indi mptr))
    (sta (zp (1+ mfr-addr)))
    (jsr 'player-write)                 ; Play frame from MFR.

    ;; Advance music pointer
    (lda mptr-lsb)                       ; 16-bit addition: MPTR = (MPTR+2) mod song_len
    (clc)
    (adc (imm 2))
    (sta mptr-lsb)
    (lda mptr-msb)
    (adc (imm
          (delay nil ((offset (msb (label 'music-start))))
            (- 256 offset))))
    (anda (imm (1- (expt 2 log2-song-length))))
    (clc)
    (adc (imm (delay nil ((offset (msb (label 'music-start))))
                offset)))
    (sta mptr-msb)

    (rts))

  ;; Reset sound hardware and initialize music player.
  (procedure player-init
    (poke 0 #x4015)                     ; Silence all channels.
    (poke #x40 #x4017)                  ; IRQ off, 4-step.
    (ldx (imm #xF))                     ; Zero the registers
    (lda (imm 0))
    (as/until :negative
      (sta (abx #x4000))
      (dex))
    (poke 0 #x4011)                     ; Hit the DMC DAC, for good measure.
    (poke #x0F #x4015)                  ; Enable square, triangle, noise.

    (pokeword (label 'music-start) mptr) ; Set initial playback pointer.
    (rts))

  (procedure reset                      ; ------------------------------------
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

    ;; Paranoid initialization. Had some intermittent quicks on real
    ;; hardware, as though the nametables weren't fully initialized,
    ;; so I've inserted a bunch of paranoid vblank syncs, and call the
    ;; init routine multiple times just to be sure.
    (lda (imm 3))
    (sta countdown)
    (as/until :zero
      (jsr 'init-nametable)
      (jsr 'init-attr)
      (jsr 'init-palette)
      (dec countdown))

    (jsr 'player-init)                  ; Initialize music player

    (lda (imm 64))                      ; Initialize scroll variables
    (sta scroll-x)
    (lda (imm 0))
    (sta ntaddr)
    (sta wave)
    (lda (imm 159))
    (sta phase)

    (jsr 'initialize-sprites)

    (jsr 'update-sprites)

    (lda (imm 0))                       ; Don't forget to reset this.
    (sta vblank-flag)

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

  ;;; Do register writes for this frame of music. Set MFR to the
  ;;; set of writes for this frame (16*2 bytes).
  (procedure player-write
    (ldy (imm #x1F))
    (as/until :negative
     (lda mfr-get)
     (tax)
     (dey)
     (lda mfr-get)
     (sta (abx #x4000))
     (dey))
    (rts))

  (procedure init-nametable
    ;; Fill nametable.
    (as/until :negative (bita (mem +ppu-status+)))
    (ppuaddr #x2000)
    (jsr 'fill-nametable)
    (as/until :negative (bita (mem +ppu-status+)))
    (ppuaddr #x2400)
    (jsr 'fill-nametable)
    (rts))

  (procedure init-attr
    ;; Fill attribute table.
    (as/until :negative (bita (mem +ppu-status+)))
    (ppuaddr #x23C0)
    (ldy (imm 0))
    (lda (imm #b01010101))
    (jsr 'fill-attributes)
    (as/until :negative (bita (mem +ppu-status+)))
    (ppuaddr #x27C0)
    (ldy (imm #b10101010))
    (lda (imm #b11111111))
    (jsr 'fill-attributes)
    (rts))

  (procedure init-palette
    ;; Program palette.
    (ppuaddr #x3F00)
    (dolist (color '(#x3F #x2D #x3D #x30  #x3F #x03 #x13 #x23
                     #x3F #x2D #x3D #x30  #x3F #x05 #x15 #x25
                     #x3F #x3F #x27 #x37))
      (poke color +vram-io+))
    (rts))

  (align 256)
  (procedure framestep
    (jsr 'wait-for-vblank)
    (lda (mem +ppu-status+))          ; Reset PPU address latch.
    (poke 0 +spr-addr+)               ; Reset sprite address!
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
    (emit-delay (+ (* 114 107) 22))
    (lda top-ntaddr)
    (eor (imm #b10010000))            ; Invert pattern bank.
    (sta (mem +ppu-cr1+))

    ;; Run the music
    (jsr 'player-step)

    ;; Leftover scanlines before wave effect starts
    (emit-delay (round (+ (* 113.66 9) 110)))

    ;; Wavy effect
    (flet ((kernel ()
             ;; Assumes Y counts down each line..
             (poke #b00011110 +ppu-cr2+) ; To visually calibrate timing.
             (clc)
             (lda wt-get)               ; Get wave offset
             (adc scroll-x)             ; Add to BG scroll position
             (ldx (imm 0))
             (sta (mem +vram-scroll+))
             (stx (mem +vram-scroll+))
             (lda ntaddr)               ; Carry in. Effectively, scroll-x and
             (adc (imm 0))              ; ntaddr.0 form a 9-bit field.
             (sta (mem +ppu-cr1+))
             (poke #b00011110 +ppu-cr2+)))

      ;; Setup for wave effect.. load table pointer into zero page.
      (inc wave)                        ; Increment wave phase
      (lda wave)
      (anda (imm (1- (expt 2 log2-wavy-period)))) ; Modulo cycle length..
      (lsr)
      (clc)
      (adc (imm (msb (label 'wave-offsets)))) ; Calculate table MSB
      (sta wt-msb)
      (lda wave)
      (lsr)
      (lda (imm 0))                     ; Rotate into MSB of LSB (...)
      (ror)                             ; (each table is 128 bytes)
      (adc (imm (lsb (label 'wave-offsets))))
      (sta wt-lsb)

      (emit-delay 47)                   ; Realign with hblank
      (bita (zp 0))
      (nop)

      (macrolet ((reporting-timing (&body body)
                   `(print (list :kernel-cycles
                                 (counting-cycles ,@body)))))

        (ldy (imm num-wavy-lines))      ; Decremented within scanline kernel.

        (loop repeat (/ num-wavy-lines 3) do
              (reporting-timing (kernel) (dey))
              (loop repeat 14 do (inc (zp 0)))
              (reporting-timing (kernel) (dey))
              (nop)
              (loop repeat 14 do (inc (zp 0)))
              (reporting-timing (kernel) (dey))
              (loop repeat 14 do (inc (zp 0))))))

    ;; Shut the screen off. Need a few extra scanlines here to update
    ;; the sprite tables, and if I didn't shut it off, you'd notice
    ;; the background stopped waving.
    (loop repeat 10 do (nop))           ; Realign with hblank.. again.
    ;; Actually, leave the sprites on, otherwise they'll be disabled
    ;; during the DMA transfer, which is unwise.
    (poke #x10 +ppu-cr2+)

    (rts))

  ;; This table controls the scrolling.
  (align 256)
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

  (procedure fill-attributes
    (ldx (imm 8))
    (as/until :zero
      (dotimes (x 4) (sty (mem +vram-io+)))
      (dotimes (x 4) (sta (mem +vram-io+)))
      (dex))
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
      (adc (imm -14))                   ; Offset Y coordinate
      (sta (aby sprite-table))          ; Store in sprite Y coordinate
      (pla)                             ; Pop sine
      (sta (aby (+ 3 sprite-table)))    ; Store in sprite X coordinate
      (dex))                            ; Decrement sprite index
    (rts))

  (align 256)
  (with-label sine-table
    (loop for i from 0 below 256
          do (db (round (+ 124 (* 99 (sin (* 2 pi i 1/256))))))))

  (align 256)
  (with-label wave-offsets
    (loop with nframes = (expt 2 log2-wavy-period)
          for frame from (1- nframes) downto 0 do
      (align 128)
      (emit
       ;; The wave loop counts from num-wavy-lines to 1. So, one extra here.
       (loop for line from num-wavy-lines downto 0
             with amp = 2
             collect (mod (round ( + 128 ; bias to fix carry
                                     (* amp (/ (expt line 1.1) 80)
                                        (sin (* 2 pi (/ (+ (* 0.07 (expt line 1.75)) frame)
                                                        nframes))))))
                          256)))))

  (format t "~&Code ends at at ~X~%" *origin*)

  ;; ************************************************************
  ;; MUSIC

  (labels
      ((emit-frame (frame)
         (unless (<= (length frame) 16)
           (error "Too many writes! ~X" (mapcar 'second frame)))
         (setf frame (pad-frame frame))
         (unless (gethash frame write-patterns)
           (setf (gethash frame write-patterns) *origin*)
           ;; Reverse order, because player scans backward!
           (dolist (pair (reverse frame)) (apply 'db pair)))
         (push (gethash frame write-patterns) music-sequence))

       (song (frames)
         ;; music-test.lisp has a sad tale to share.
         (print (subseq frames 0 10))   ; DEBUG KILLME
         (map nil (lambda (frame) (emit-frame (resolve-tree frame))) frames))

       ;; Song elements:

       (phrase-aaab (a b) (seq a a a b))

       (four-on-the-floor () (repeat 4 (thump 32 (et -24))))

       (rhythm (fn notes &optional (transpose 0))
         (seq
          (funcall fn 32 (et transpose (nth 0 notes)))
          (funcall fn 24 (et transpose (nth 1 notes)))
          (funcall fn 16 (et transpose (nth 2 notes)))
          (funcall fn 24 (et transpose (nth 3 notes)))
          (funcall fn 32 (et transpose (nth 4 notes)))))

       (swagger ()
         (seq
          (kick 16)
          (hat 8)
          (hat 8)
          (snare 16)
          (hat 8)
          (hat 8 4)))

       (stagger ()
         (seq
          (hat 8)
          (kick 8)
          (hat 8)
          (hat 8)
          (snare 16)
          (rst 16)))

       (jagger ()
         (seq
          (shaker 8 15)
          (shaker 8 4)
          (shaker 8 8)
          (shaker 8 12)
          (shaker 8 15)
          (shaker 8 5)
          (shaker 8 11)
          (shaker 8 14)))

       (intro-beat ()
         (measure
          (four-on-the-floor)
          (seq
           (swagger)
           (swagger))))

       (intro-fill-1 ()
         (measure
          (four-on-the-floor)
          (seq (swagger)
               (stagger))))

       (intro-fill-2 ()
         (measure
          (four-on-the-floor)
          (seq (jagger)
               (jagger)))))

    (align 32)

    (song
     (seq
      ;; Smooth section
      (seq
       (para
        (phrase-aaab
         (intro-beat)
         (intro-fill-1))
        (seq
         (measure (fat-arp 128 '(0.00  0 3 7 11)   :rate 4 :volume (volramp 8 -1/22)))
         (measure (fat-arp 128 '(0.00  0 2 5 8 )   :rate 4 :volume (volramp 8 -1/22)))
         (measure (fat-arp 128 '(0.00  -2 7 8 12 ) :rate 4 :volume (volramp 9 -1/20)))
         (measure (fat-arp 128 '(0.00  -1 2 3 7 )  :rate 4 :volume (volramp 10 -1/18)))))

       (para
        (phrase-aaab
         (intro-beat)
         (intro-fill-2))
        (seq
         (measure (arpeggio 0 128 (chord -0.02   0  3  7 11)       :rate 4 :volume (volramp 11 -1/16))
                  (arpeggio 1 128 (chord  0.02  12  0  3 14 7 11)  :rate 3 :volume (volramp 11 -1/13)))
         (measure (arpeggio 0 128 (chord -0.02   0  2  5 8)        :rate 4 :volume (volramp 12 -1/14))
                  (arpeggio 1 128 (chord  0.02   2  5  8 12 15)    :rate 3 :volume (volramp 12 -1/14)))
         (measure (arpeggio 0 128 (chord -0.02  -2  7  8 12)       :rate 4 :volume (volramp 13 -1/12))
                  (arpeggio 1 128 (chord  0.02   5 15  8 12 15 17) :rate 3 :volume (volramp 13 -1/12)))
         (measure (arpeggio 0 128 (chord -0.02  -1  2  3 7)        :rate 4 :volume (volramp 15 -1/10) :mute t)
                  (arpeggio 1 128 (chord  0.02   3  7 14 11 19 14) :rate 4 :volume (volramp 15 -1/10) :mute t)))))

      ;; Funky section
      (seq
       (para
        (phrase-aaab
         (measure
          (rhythm #'thump '(0 0 0 0 7) -12)
          (seq (swagger) (stagger)))
         (measure
          (seq (stagger) (stagger))
          (four-on-the-floor)))
        (seq
         (measure
          (funky-arp 0 12 0 3 11 14 7 17 0 17 12 19 15 17 10 15))
         (measure
          (para
           (note 0 32 (et  0.026) :cfg '(:duty 1 :env t :vol 2 :loop nil))
           (note 1 32 (et -0.026) :cfg '(:duty 3 :env t :vol 2 :loop nil))))
         (measure
          (funky-arp 5 0 5 7 8 11 12 0 12 17 15 17 15 14 12 8 7))
         (measure
          (funky-arp 7 0 7 11 12 3 17 19 20 24 23 20 0 19 17 20))))
       (para
        (phrase-aaab
         (measure
          (seq (swagger) (stagger))
          (rhythm #'thump '(0 3 3 -2 0) -12))
         (measure
          (seq (stagger) (jagger))))
        (seq
         (measure
          (fat-arp 128 '(0.0  0 12 0 3 11 14 7 17 0 17 12 19 15 17 20 19)
                   :d 6 :rate 8 :env t :loop nil :volume (constantly 3) :mute nil))
         (measure
          (seq
           (para
            (note 0 32 (et  0.026 24) :cfg '(:duty 1 :env t :vol 2 :loop nil))
            (note 1 32 (et -0.026 24) :cfg '(:duty 3 :env t :vol 0 :loop nil)))
           (para
            (note 0 32 (et  0.026 24) :cfg '(:duty 1 :env t :vol 2 :loop nil))
            (note 1 32 (et -0.026 12) :cfg '(:duty 3 :env t :vol 2 :loop nil)))))
         (measure
          (fat-arp 128 '(5.0  0 3 7 0 3 7 0 3 8 0 3 7 0 3 8) :rate 4 :d 3 :volume (volramp 10 -1/18)))
         (measure
          (fat-arp 128 '(0.0  2 5 8 11 2 5 8 14) :rate 4 :d 3 :volume (volramp 9 -1/18))))))))

    (align 256)
    (with-label music-start
      (unless (= (length music-sequence) (* 128 (expt 2 log2-song-length)))
        (error "Song length is ~:D, should be ~:D"
               (length music-sequence)
               (* 128 (expt 2 log2-song-length))))
      (print (list :num-unique  (length (remove-duplicates music-sequence))))
      ;; Write the pointer table:
      (mapcar #'dw (reverse music-sequence))))

  ;; ************************************************************

  (format t "~&Empty space begins at ~X~%" *origin*)

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
