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
  (let ((fbits (round (/ +ntsc-clock-rate+ seqlen freq))))
    (values (ldb (byte 8 0) fbits)
            (logior (ldb (byte 3 8) fbits)
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
     (mapcar (lambda (pair) (list (first pair) (- (second pair) length)))
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
     :key (lambda (p) (abs (second p)))))))

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

;;; These look familiar:
(defun seq (&rest args)
  (apply #'concatenate 'list args))

(defun repeat (n &rest args)
  (apply #'seq (mapcan #'copy-list (loop repeat n collect args))))

(defun et (&rest args) (* 261.0 (expt 2 (/ (apply '+ args) 12))))

(defun kick (length)
  (noise length 8 15 :vol 1))

(defun snare (length &optional (variation 0))
  (noise length 8 (+ 10 variation) :vol 1))

(defun hat (length &optional (variation 0))
  (noise length 4 (+ variation 1) :vol 1))

(defun thump (length &optional (pitch (et -24)))
  (segment
   length
   (seq (tri 1 (* pitch 1))
        (tri 1 (* pitch 4/3))
        (tri 1 (* pitch 2/3))
        (tri 1 (* pitch 1/2)))))

(defun rst (length) (segment length nil))



(defun wait (&optional (frames 20))
  (ldx (imm frames))
  (as/until :zero (jsr 'wait) (dex)))

(let* ((*context* (make-instance 'basic-context :address #x8000))

       ;; Music player
       (mfr-addr #x40)
       (mfr-get (indi mfr-addr))
       (mptr #x42)
       (mptr-msb  (zp (1+ mptr)))
       (mptr-lsb  (zp mptr))
       (log2-song-length 0)             ; Base 2 log of song length.

       ;; Reduce space by reusing patterns of registers.
       (regs-table (make-hash-table :test 'equal))
       (music-sequence '())

       (vblank-flag (zp #x96)))

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


  (flet ((emit-frame (frame)
           (setf frame (pad-frame frame))
           (unless (gethash frame regs-table)
             (setf (gethash frame regs-table) *origin*)
             ;; Reverse order, because player scans backward!
             (dolist (pair (reverse frame)) (apply 'db pair)))
           (push (gethash frame regs-table) music-sequence)))

    (align 16)
    (mapcar
     #'emit-frame
     (segment
      128
      (para
       (seq
        (thump 32 (et -12))
        (thump 24 (et -12 2))
        (thump 16 (et -12 2))
        (thump 24 (et -12 3))
        (thump 32 (et -12 5)))
       (seq
        (kick 16)
        (hat 8)
        (hat 8)
        (snare 16)
        (hat 8)
        (hat 8 4)
        (hat 8)
        (kick 8)
        (hat 8)
        (hat 8)
        (snare 16)
        (rst 16)))



      #+NIL
      (repeat 8
              (seq (noise 1 2 15 :env nil :vol 1)
                   (noise 1 2 13 :env nil :vol 2 :short t)
                   (noise 1 2 11 :env nil :vol 3)
                   (noise 1 2 9 :env nil :vol 4 :short t)
                   (noise 1 2 7 :env nil :vol 5)
                   (noise 1 2 5 :env nil :vol 6 :short t)
                   (noise 1 2 3 :env nil :vol 7)
                   (noise 1 2 1 :env nil :vol 8))
              (noise 8 1 1 :vol 0))
      #+NIL
      (seq
       (noise 16 16 3 :short nil :env t :loop t :vol 0)
       (noise 16 1 12 :short nil :env t :loop t :vol 2)
       (noise 16 16 7 :short t :env t :loop t :vol 0)
       (noise 16 16 11 :short t :env t :loop t :vol 0)
       (noise 16 1 1 :vol 1 )
       (noise 16 1 3 :vol 1)
       (noise 16 1 9 :vol 1) )
      #+NIL
      (seq
       (para
        (note 0 8 (et 0.00)  :cfg '(:duty 3 :loop nil :vol 15))
        (note 1 8 (et 0.04)  :cfg '(:duty 3 :loop nil :vol 15)))
       (para
        (note 0 8 (et 0.00)  :cfg '(:duty 3 :loop nil :vol 15))
        (note 1 8 (et 0.08)  :cfg '(:duty 3 :loop nil :vol 15)))
       (para
        (note 0 8 (et 0.00)  :cfg '(:duty 3 :loop nil :vol 15))
        (note 1 8 (et 0.12)  :cfg '(:duty 3 :loop nil :vol 15)))
       (para
        (note 0 8 (et 0.00)  :cfg '(:duty 3 :loop nil :vol 15))
        (note 1 8 (et 0.15)  :cfg '(:duty 3 :loop nil :vol 15)))

       (note 0 32 (et 12))
       (note 0 32 (et 7))
       (note 0 32 (et 10)))))

;    (emit-frame (pad-frame (noteon 0 #b01000 220.0)))
;    (dotimes (i 63) (emit-frame (pad-frame '())))
;    (emit-frame (pad-frame (noteon 0 #b01000 330.0)))
;    (dotimes (i 63) (emit-frame (pad-frame '())))

    (align 256)
    (with-label music-start
      (assert (= (length music-sequence) (* 128 (expt 2 log2-song-length)))) ; FIXME
      (print (list :num-unique  (length (remove-duplicates music-sequence))))
      (mapcar #'dw (reverse music-sequence))))

  (procedure reset
    (sei)                               ; Init CPU
    (cld)
    (poke 0 +ppu-cr1+)
    (ldx (imm #xFF))
    (txs)
;;    (as/until :negative (bita (mem +ppu-status+))) ; PPU warmup
;;    (as/until :negative (bita (mem +ppu-status+)))

    ;; Init sound hardware..

    (poke 0 #x4015)                     ; Silence all channels.
    (poke #x40 #x4017)                  ; IRQ off, 4-step.
    (ldx (imm #xF))                     ; Zero the registers
    (lda (imm 0))
    (as/until :negative
      (sta (abx #x4000))
      (dex))
    (poke 0 #x4011)                     ; Hit the DMC DAC, for good measure.

    (poke #x0F #x4015)                  ; Enable square, triangle, noise.


    (poke #b10000000 +ppu-cr1+)         ; Enable NMI
    (jsr 'wait)


    (brk) (db 4)
    (pokeword (label 'music-start) mptr)
    (with-label loop
      (brk) (db 5)
      (jsr 'player-step)
      (jsr 'wait)
      (jmp (mem 'loop)))


    )

  (procedure brk-handler (rti))

  (procedure nmi-handler
    (inc vblank-flag)
    (rti))

  (procedure wait
    (as/until :not-zero (lda vblank-flag))
    (poke 0 vblank-flag)
    (rts))

  (advance-to +nmi-vector+)
  (dw (label 'nmi-handler))
  (dw (label 'reset))
  (dw (label 'brk-handler))

  ;; Write .NES file
  (write-ines "/tmp/music.nes" (link *context*)))


;;;; Old shit:

#+NIL
    (with-label main-loop

      (brk) (db 1)


      (poke #x08 #x4008)

      (dotimes (i 4)
        (poke #b10001111 #x4000)
        (pr 0 #b00101 220.0)
        (pr 2 0 110.0)
        (wait 3)
        (poke #b10001111 #x4000)
        (pr 0 #b00101 440.0)
        (pr 2 0 220.0)
        (wait 1))

      (dotimes (i 4)
        (poke #b10001111 #x4000)
        (pr 0 #b00101 294.0)
        (pr 2 0 147.0)
        (wait 3)
        (poke #b11001111 #x4000)
        (pr 0 #b00101 (* 3/2 294.0))
        (pr 2 0 73.0)
        (wait 1))

      (wait 30)



#+NIL
      (loop for interval in '(1/1 4/3 3/2 9/5)
            as freq = (* 440 interval)
            as detune = (* freq 1.05)
            do

            (poke #b10101000 #x4000)
            (poke 0 #x4001)
            (let* ( (fbits (round (/ +ntsc-clock-rate+ 8 freq)))
                   (lbits #b11000)
                    (a (ldb (byte 8 0) fbits))
                    (b (logior (ldb (byte 3 8) fbits)
                               (ash lbits 3))))
              (poke a #x4002)
              (poke b #x4003))

            (poke #b10001000 #x4004)
            (poke 0 #x4005)
            (let* ((fbits (round (/ +ntsc-clock-rate+ 8 (+ detune freq))))
                   (lbits #b11000)
                    (a (ldb (byte 8 0) fbits))
                    (b (logior (ldb (byte 3 8) fbits)
                               (ash lbits 3))))
              (poke a #x4006)
              (poke b #x4007))

            (ldx (imm 2))
            (as/until :zero (jsr 'wait) (dex)))

      (jmp (mem 'main-loop)))