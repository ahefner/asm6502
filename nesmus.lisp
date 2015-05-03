(in-package :nesmus)

;;;; --- Music language ---

(defun register (address value) (list value address))
(defun nop-write () (register #x0D 0)) ; Dummy write to unused sound register.

(defun pad-list (list padding desired-length)
  (assert (<= (length list) desired-length))
  (append list (loop repeat (- desired-length (length list)) collect padding)))

(defun pad-frame (frame)
  (pad-list frame (nop-write) 16))

(defun segment (length list)            ; rewrite as map-into?
  (if (< (length list) length)
      (pad-list list nil length)
      (subseq list 0 length)))

(defun translate-freq (seqlen lbits freq)
  (let ((fbits (delay 'fbits (freq) (round (/ +ntsc-clock-rate+ seqlen freq)))))
   (values (delay 'reg2 (fbits) (ldb (byte 8 0) fbits))
           (delay 'reg3 (fbits) (logior (ldb (byte 3 8) fbits)
                                       (ash lbits 3))))))

#+NIL
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

(defparameter *tuning-root* 276.0)

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










