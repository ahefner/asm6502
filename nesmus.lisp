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
  (cond
    ((<= d 31)
     (segment length
              (list
               (list* (register #x8 (* d 4))
                      (noteon 2 1 freq)))))
    (t
     (segment length
       (seq
         (list (list* (register #x8 #x8F)
                      (noteon 2 1 freq)))
         (rst (if (= d length)
                  (- length 2)
                  (1- d)))
         (list (list (register #x8 0) (register #xB #x07))))))))

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
  (seq
   (segment (if mute (1- length) length)
     (loop for time below length by rate
        for index upfrom 0
        append (note channel rate (eltmod index chord)
                     :d d
                     :cfg (list :duty (funcall duty time)
                                :vol (funcall volume time)
                                :env env
                                :loop loop))))
   (and mute (silence-channel channel))))

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

;;;; Song authoring framework

(defun write-song-data-for-reg-player (song-frames start-label end-label)
  (let ((write-patterns (make-hash-table :test 'equal))
        (histogram (make-array 16))
        (music-sequence nil)
        (start-address *origin*))
    (align 16)
    (map nil (lambda (frame)
               (unless (<= (length frame) 16)
                 (error "Too many writes! ~X" (mapcar 'second frame)))
               (incf (aref histogram (length frame)))
               (setf frame (pad-frame frame))
               (unless (gethash frame write-patterns)
                 (setf (gethash frame write-patterns) *origin*)
                 ;; Reverse order, because player scans backward!
                 (dolist (pair (reverse frame)) (apply 'db pair)))
               (push (gethash frame write-patterns) music-sequence))
         song-frames)
    (setf music-sequence (nreverse music-sequence))
    (align 2)
    (set-label start-label)
    (map nil #'dw music-sequence)
    (set-label end-label)
    (print (list :pattern-count (hash-table-count write-patterns)
                 :frame-count (length music-sequence)
                 :write-count-histogram histogram
                 :start-address start-address
                 :seq-start (label start-label)
                 :seq-end (label end-label)
                 ;;:sequence music-sequence
                 :total-size (- *origin* start-address)))))

(defvar *last-audition-function* nil)

;;; TODO: Implement more memory-efficient encoding..

(defmacro define-song (name options)
  (unless (stringp name)
    (error "Song name must be a string"))
  (let* ((package-name (if (symbolp name)
                           name
                           (format nil "~A (song)" name)))
         (package (or (find-package package-name)
                      (make-package package-name)))
         (asm-fn-name (intern "ASSEMBLE-IN-CONTEXT" package)))
   `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defpackage ,package-name
        (:use :common-lisp :6502 :asm6502 :asm6502-utility
              :asm6502-nes :nesmus
              ,@ (getf options :use-packages)))
      (in-package ,package-name)
      (defun ,asm-fn-name ()
        ())                             ; How I do?
      (defmacro ,(intern "DEFPATTERN" package) (name (&key parameters audition accompany) &body body)
        `(progn
           ;; Tempted to transform the name so it can't collide with
           ;; CL package..
           (defun ,name ,parameters
             (print (list :defining ',name))
             ,@body)
           (setf (get ',name 'audition)
                 (lambda (loop-count)
                   (print (list :previewing ',name))
                   (generate-nsf-preview
                    ',name
                    (lambda () (,name)
                            (loop repeat loop-count
                               nconcing (copy-list
                                         (para
                                          ,@accompany
                                          (,name ,@audition)))))
                    :break-at-end t))
                 *last-audition-function*
                 (prog1 (get ',name 'audition)
                   (print "Set last audition function.")))))
      (defun ,(intern "NSF-OUTPUT-FILE" package) (filename)
        (generate-nsf-preview ,name #',asm-fn-name :filename filename)))))

(defun generate-nsf-preview (name continuation &key filename (break-at-end nil))
  (setf filename (or filename (format nil "/tmp/nsf-audition/~A.nsf" name)))
  (let* ((*context* (make-instance 'basic-context))
         ;; Music player vars
         (mfr-addr #x40)                ; Frame working pointer (temporary)
         (mfr-get (indi mfr-addr))
         (mptr #x42)                    ; Playback pointer
         (mptr-msb  (zp (1+ mptr)))
         (mptr-lsb  (zp mptr)))

    (emit-nsf-header 1 #x8000 'init 'play :song-name (format nil "~A" name))

    (setf *origin* #x8000)

    (procedure init
      (cld)
      (pokeword (label :seq-start) mptr)
      (rts))

    (procedure play
      (cld)

      ;; Transfer *MPTR to MFR and play this frame.
      (ldy (imm 0))                       ; LSB of new music frame pointer
      (lda (indi mptr))
      (sta (zp mfr-addr))
      (iny)                               ; MSB of new music frame pointer
      (lda (indi mptr))
      (sta (zp (1+ mfr-addr)))
      (jsr 'player-write)                 ; Play frame from MFR.

      ;; Advance music pointer
      (clc)
      (inc mptr-lsb)                    ; Requires music is word aligned
      (inc mptr-lsb)
      (asif :zero
        (inc mptr-msb))

      (lda mptr-lsb)
      (cmp (imm (lsb (label :seq-end))))
      (asif :equal
        (lda mptr-msb)
        (cmp (imm (msb (label :seq-end))))
        (asif :equal
          (when break-at-end
            (brk)
             (db #xF1)
             (rts))
          (pokeword (label :seq-start) mptr)))

      (rts))

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

    (write-song-data-for-reg-player (funcall continuation) :seq-start :seq-end)

    (ensure-directories-exist filename)
    (setf (binary-file filename) (link *context*))
    (format t "~&Wrote output to ~A~%" filename))
  filename)

(defun play-audition (loop-count player-cmd)
  (when *last-audition-function*
    (uiop:run-program
     (list player-cmd (funcall *last-audition-function* loop-count))
     :output :interactive
     :ignore-error-status t)))
