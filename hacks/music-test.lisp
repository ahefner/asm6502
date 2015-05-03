;;;; Compiling this file will result in an output file /tmp/dollhouse.nsf

(defpackage :music-test
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes :nesmus))

(in-package :music-test)

#+KILLME
(defun wait (&optional (frames 20))
  (ldx (imm frames))
  (as/until :zero (jsr 'wait) (dex)))

;;;; **********************************************************************

(let* ((*context* (make-instance 'basic-context :address 0))

       ;; Music player
       (mfr-addr #x40)
       (mfr-get (indi mfr-addr))
       (mptr #x42)
       (mptr-msb  (zp (1+ mptr)))
       (mptr-lsb  (zp mptr))
       (log2-song-length 4)             ; Base 2 log of song length.

       ;; Reduce space by reusing patterns of registers.
       (regs-table (make-hash-table :test 'equal))
       (histogram (make-array 16))
       (music-sequence '())

       #+NIL (vblank-flag (zp #x96)))

  ;;; Build file header
  (emit-nsf-header 1 :load 'reset 'player-step
                   :song-name "Dollhouse"
                   :artist "Andy Hefner <ahefner@gmail.com>")
  (setf *origin* #x8000)
  (set-label :load)

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

  ;; Step music playback. Advances MFR.
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


  (labels
      ((emit-frame (frame)
         (unless (<= (length frame) 16)
           (error "Too many writes! ~X" (mapcar 'second frame)))
         (incf (aref histogram (length frame)))
         (setf frame (pad-frame frame))
         (unless (gethash frame regs-table)
           (setf (gethash frame regs-table) *origin*)
           ;; Reverse order, because player scans backward!
           (dolist (pair (reverse frame)) (apply 'db pair)))
         (push (gethash frame regs-table) music-sequence))

       (song (frames)
         ;; So, I had this awesome idea of doing a sort of barber pole
         ;; "infinite modulation" by slowly sliding the tuning
         ;; downward through the course of the music loop via some
         ;; clever abuse of the delay/force mechanism.. but it turns
         ;; out there's not enough quite enough space in the ROM to do
         ;; it well without a more sophisticated player routine. Darn.
         (loop with initial-tuning = (* 261.0 (expt 2 1/12))
               with final-tuning = (* initial-tuning #+NIL (expt 2 1/12))
               with length = (length frames)
               with asm6502::*memoize-promises* = nil
               for frame in frames
               for position upfrom 0
               as frame-tuning = (+ initial-tuning (* (- final-tuning initial-tuning)
                                                        (/ position length)))
               do (let ((*tuning-root* frame-tuning))
                    ;;(print (list :frame position :tuning frame-tuning :frame frame))
                    (emit-frame (resolve-tree frame)))))

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

    (align 16)

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
          (repeat 2
           (para
            (note 0 32 (et  0.026) :cfg '(:duty 1 :env t :vol 2 :loop nil))
            (note 1 32 (et -0.026) :cfg '(:duty 3 :env t :vol 2 :loop nil)))))
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

    (print (list :histogram histogram))

    (align 256)
    (with-label music-start
      (unless (= (length music-sequence) (* 128 (expt 2 log2-song-length)))
        (error "Song length is ~:D, should be ~:D"
               (length music-sequence)
               (* 128 (expt 2 log2-song-length))))
      (print (list :num-unique  (length (remove-duplicates music-sequence))))
      (mapcar #'dw (reverse music-sequence))))

  (print (list :music-size (- *origin* #x8000)))

  (procedure reset
    (cld)
    ;; Init sound hardware..
    (poke 0 #x4015)                     ; Silence all channels.
    (ldx (imm #x11))                    ; Zero the registers
    (lda (imm 0))
    (as/until :negative
      (sta (abx #x4000))
      (dex))

    (poke #x0F #x4015)                  ; Enable square, triangle, noise.

    ;; Set initial song playback pointer:
    (pokeword (label 'music-start) mptr)
    (rts))

  ;; Write .NSF file
  (setf (binary-file "/tmp/dollhouse.nsf") (link *context*)))
