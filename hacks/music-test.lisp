;;;; Compiling this file will result in an output file /tmp/dollhouse.nsf

(defpackage :music-test
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :music-test)

(defun register (address value) (list value address))

(defun nop-write ()
  (register #x0D 0))

(defun pad-list (list padding desired-length)
  (assert (<= (length list) desired-length))
  (append list (loop repeat (- desired-length (length list)) collect padding)))

(defun pad-frame (frame)
  (pad-list frame (nop-write) 16))                           ; Dummy write to unused sound register.

(defun segment (length list)
  (if (< (length list) length)
      (pad-list list nil length)
      (subseq list 0 length)))

(defun translate-freq (seqlen lbits freq)
  ;; DELAY doesn't deal with multiple values, thus the duplication here:
  (values (delay 'reg2 (freq)
              (ldb (byte 8 0)
                   (round (/ +ntsc-clock-rate+ seqlen freq))))
          (delay 'reg3 (freq)
              (logior (ldb (byte 3 8)
                           (round (/ +ntsc-clock-rate+ seqlen freq)))
                      (ash lbits 3)))))

(defun noteon (chan lbits freq)
  (multiple-value-bind (base seqlen)
      (ecase chan
        (0 (values 0 8))
        (1 (values 4 8))
        (2 (values 8 32)))
    (multiple-value-bind (v2 v3) (translate-freq seqlen lbits freq)
      (list
       (register (+ 2 base) v2)
       (register (+ 3 base) v3)))))

(defun translate-length (length)
  "Find closest match to load the length counter."
  (first
   (first
    (sort
     (copy-list
      '((0 #x0A)  (1 #xFE)
        (2 #x14)  (3 #x02)
        (4 #x28)  (5 #x04)
        (6 #x50)  (7 #x06)
        (8 #xA0)  (9 #x08)
        (10 #x3C) (11 #x0A)
        (12 #x0E) (13 #x0C)
        (14 #x1A) (15 #x0E)
        (16 #x0C) (17 #x10)
        (18 #x18) (19 #x12)
        (20 #x30) (21 #x14)
        (22 #x60) (23 #x16)
        (24 #xC0) (25 #x18)
        (26 #x48) (27 #x1A)
        (28 #x10) (29 #x1C)
        (30 #x20) (31 #x1E)))
     #'<
     :key (lambda (p) (abs (- (second p) length)))))))

(defun cfg (channel &key (duty 2) (vol 15) (env t) (loop nil))
  (list
   (list (register (* channel 4)
                   (logior (ash duty 6)
                           (if env 0 #x10)
                           (if loop #x20 0)
                           vol)))))

(defun note (channel length freq &key (d length) cfg)
  (check-type channel (integer 0 1))
  (segment length
           (para
            (and cfg (apply 'cfg channel cfg))
            (list
             (noteon channel (translate-length d) freq)))))

(defun silence-channel (channel)
  (ecase channel
    (0 (note 0 1 1 :d 0 :cfg '(:vol 0 :loop t :env nil)))
    (1 (note 1 1 1 :d 0 :cfg '(:vol 0 :loop t :env nil)))))

(defun tri (length freq &key (d length))
  (check-type d (integer 0 31))
  (segment length
           (list
            (list* (register #x8 (* d 4))
                   (noteon 2 1 freq)))))

(defun noise (length duration period &key short loop (env t) (vol 15))
  (check-type duration (integer 0 31))
  (check-type vol (integer 0 15))
  (check-type period (integer 0 15))
  (segment length
    (list
     (list
      (register #xC (logior (if loop #x20 0)
                            (if env 0 #x10)
                            vol))
      (register #xE (logior (if short #x80 0)
                            period))
      (register #xF (ash (translate-length duration) 3))))))

(defun para (&rest args)
  (apply #'mapcar #'append (mapcar (lambda (x) (pad-list x nil (reduce #'max args :key #'length))) args)))

(defun measure (&rest args)
  (segment 128 (apply 'para args)))

;;; These look familiar:
(defun seq (&rest args)
  (apply #'concatenate 'list args))

(defun repeat (n &rest args)
  (apply #'seq (mapcan #'copy-list (loop repeat n collect args))))

(defun rst (length) (segment length nil))

(defparameter *tuning-root* nil)

(defun get-tuning-root ()
  (make-promise :name "Tuning Root"
                :fun (lambda ()
                       ;;(when *tuning-root* (print (list :tuning-root *tuning-root*)))
                       (or *tuning-root*
                           (error 'asm6502::resolvable-condition
                                  :path "Tuning root not set.")))))

(defun et (&rest args)
  (delay 'et ((tuning (get-tuning-root)))
    (* tuning (expt 2 (/ (apply '+ args) 12)))))

(defun kick (length)
  (noise length 8 15 :vol 1))

(defun snare (length &optional (variation 0))
  (noise length 8 (+ 10 variation) :vol 1))

(defun hat (length &optional (variation 0))
  (noise length 4 (+ variation 1) :vol 1))

(defun thump (length &optional (pitch (et -24)))
  (segment
   length
   (seq (tri 1 (delay nil (pitch) (* pitch 1)))
        (tri 1 (delay nil (pitch) (* pitch 4/3)))
        (tri 1 (delay nil (pitch) (* pitch 2/3)))
        (tri 1 (delay nil (pitch) (* pitch 1/2))))))

(defun shaker (length volume)
  (assert (>= length 2))
  (segment
   length
   (seq
    (noise 1 1 1 :env nil :loop t :vol volume)
    (noise 1 1 1 :env nil :vol 0))))

(defun eltmod (i seq) (elt seq (mod i (length seq))))
(defun clamp (x min max) (max (min x max) min))

(defun volramp (&optional (start 15) (rate -1/10))
  (lambda (time)
   (clamp (round (+ start (* time rate)))
          0
          15)))

(defun shimmer (&optional (time-shift -4) (phase-offset 0))
  (lambda (time) (mod (+ phase-offset (ash time time-shift)) 4)))

(defun arpeggio (channel length chord &key
                 (rate 3)
                 (d rate)
                 (env nil)
                 (loop t)
                 (mute nil)
                 (volume (volramp))
                 (duty (shimmer)))
  (segment length
           (para
            (loop for time below length by rate
                  for index upfrom 0
                  append (note channel rate (eltmod index chord)
                               :d d
                               :cfg (list :duty (funcall duty time)
                                          :vol (funcall volume time)
                                          :env env
                                          :loop loop)))
            (seq
             (rst (1- length))
             (and mute (silence-channel channel))))))

(defun fat-arp (length chord &rest args)
  (para
   (apply #'arpeggio 0 length (apply #'chord (- (first chord) 0.06) (rest chord))
          :duty (shimmer -2) args)
   (apply #'arpeggio 1 length (apply #'chord (+ (first chord) 0.06) (rest chord))
          :duty (shimmer -2 2) args)))

(defun funky-arp (&rest args)
  (fat-arp (* 8 (length args)) (list* 0.0 args)
           :d 15 :rate 8 :env t :loop nil :volume (constantly 1) :mute t))

(defun chord (root &rest notes)
  (mapcar (lambda (note) (et root note)) notes))

(defun wait (&optional (frames 20))
  (ldx (imm frames))
  (as/until :zero (jsr 'wait) (dex)))

(defun resolve-tree (tree)
  (etypecase tree
    (cons (cons (resolve-tree (car tree))
                (resolve-tree (cdr tree))))
    (null tree)
    (integer tree)
    (promise (force tree))))


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
