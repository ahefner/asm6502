(nesmus:define-song "Steps" ())

(defpattern bassline-1A ()
  (tri 24 (et  -1) :d 24 :vibrato-delay 12)
  (tri 24 (et  -3) :d 24)

  (tri 24 (et  -5) :d 24 :vibrato-delay 8)
  (tri 18 (et  -7) :d 16)
  (tri 18 (et  -9) :d 17)
  (tri 12 (et -10) :d 10)
  (tri 12 (et -12) :d 10)
  (tri 12 (et -14) :d 10)

  (tri 12 (et  -3) :d 12)
  (tri 12 (et  -5) :d 10)
  (tri 12 (et  -6) :d 10)
  (tri 12 (et  -3) :d 10))

(defpattern bassline-1B ()
  (tri 24 (et  -5) :d 24 :vibrato-delay 12)
  (tri 24 (et  -7) :d 23)

  (tri 24 (et  -9) :d 23 :vibrato-delay 8)
  (tri 18 (et -11) :d 16)
  (tri 18 (et -13) :d 15)
  (tri 12 (et  -6) :d 10)
  (tri 12 (et  -2) :d 10)
  (tri 12 (et  -6) :d 10)

  (tri 12 (et  -7) :d 12)
  (tri 12 (et  -5) :d 10)
  (tri 12 (et  -4) :d 10)
  (tri 12 (et  -2) :d 12 :vibrato-delay 0))

(defpattern bassline-1C ()
  (apply
   'seq
   (mapcar
    (lambda (pitch duration) (tri 12 (et pitch) :d duration))
    '(-9  -2 -5  -9
      -3 -10 -8  -7
      -5  -3 -1  -5
      -8 -11 -6 -14)
    '(9 8 8 10  9 8 8 10  9 8 8 10  9 8 8 12))))

(defpattern bassline-1D ()
  (apply
   'seq
   (mapcar
    (lambda (pitch duration) (tri 12 (et pitch) :d duration))
    '(-13 -6 -1 -6
      -7 -5 -3 -2
      -9 -2 -5 -8
      -11 -8 -6 -14)
    '(8 8 8 11  8 8 8 9  8 8 8 11  9 9 11 9))))

(defpattern bassline-seq-1 ()
  (bassline-1A)
  (bassline-1B)
  (bassline-1C)
  (bassline-1D))

(defparameter *sax-config* '(:env nil :loop nil :duty 1 :vol 7))
(defparameter *sax-long* '(:env nil :loop t :duty 1 :vol 7))

(defun bup (note length &key (d (1- length)) vibrato-delay)
  (para
   ;;(note 1 length (et note) :d d :cfg '(:env nil :loop nil :duty 0 :vol 6))
   (note 0 length (* (et (+ note -1.6 ))) :d d :cfg *sax-config* :vibrato-delay vibrato-delay)
   (seq (list (list (register 1 #x8F))
              nil
              nil
              nil
              nil
              (list (register 1 0))))))

(defpattern sax-1A (:accompany ((bassline-1A)))
  (bup 18 24 :vibrato-delay 16)
  (note 0 24 (et 14) :vibrato-delay 0)
  (bup 11 24)
  (note 0 18 (et  7))
  (note 0 (+ 48 6) (et 10) :d 48 :vibrato-delay 12)
  (bup 11 18)
  (note 0 (+ 6 24) (et 9) :vibrato-delay 8))

(defpattern sax-1BCD (:accompany ((seq (bassline-1B)
                                       (bassline-1C)
                                       (bassline-1D))))
  (bup 14 24)
  (note 0 24 (et 11) :vibrato-delay 8)
  (bup 7 24 :vibrato-delay 16)
  (note 0 18 (et 3))
  (note 0 (+ 48 6) (et 6) :d 48 :vibrato-delay 20)
  (bup 7 24)
  (note 0 18 (et 5) :vibrato-delay 0)
  (note 0 (+ 48 6) (et 10) :vibrato-delay 24)
  (note 0 24 (et 11) :d 30 :vibrato-delay 8)
  (note 0 18 (et  9) :d 23)
  (note 0 (+ 6 48) (et 14) :vibrato-delay 8)

  (bup 15 24)
  (note 0 18 (et 15) :d 20)
  (note 0 (+ 6 48) (et 18) :d 64 :vibrato-delay 24)
  (note 0 24 (et 19) :vibrato-delay 8)
  (note 0 18 (et 19) :d 20)
  (note 0 (+ 6 48) (et 22) :d 64 :vibrato-delay 24)

  (note 0 18 (et 18) :vibrato-delay 0)
  (note 0 9 (et 18) :vibrato-delay 0)
  (rst 18))

;; FIXME: redefines existing definition, breaks other hacks..
(defun chord (length volume decay mute &rest notes)
  (arpeggio 1 length (mapcar (lambda (x) (et (+ x 12))) notes)
            :rate (if (<= (length notes) 3) 2 1)
            :duty (constantly 2)
            :volume (volramp volume decay)
            :mute mute))

(defpattern chords-1A (:accompany ((bassline-1A) (sax-1A)))
  (segment (* 48 4)
    (seq
      (chord 24 7 -0.2 nil  10  6  3  -1  -6 -13)
      (chord 24 6 -0.2 nil   7  2  0  -3  -7 -15)
      (chord 24 7 -0.2 nil   9  6  2  -1 -10 -17)
      (chord 18 6 -0.3 nil   7  2  0  -4 -14 -19)
      (chord (+ 48 6) 7 -0.2 nil   7  5  2  -2 -13)
      (chord 18 6 -0.3 nil   4  0 -5  -8 -15)
      (chord 18 7 -0.3 t     6  2 -3 -10))))

(defpattern chords-1BCD (:accompany ((seq (bassline-1B)
                                          (bassline-1C)
                                          (bassline-1D))
                                     (sax-1BCD)))
  (chord 24 7 -0.2 nil   9  6  2  -3  -8 -17)
  (chord 24 6 -0.2 nil   7  2  0  -4 -10 -19)
  (chord 24 7 -0.2 nil   2 -2 -5 -14 -21)
  (chord 18 6 -0.3 nil   6  3 -2 -13 -23)
  (chord 54 7 -0.2 nil  10  6  3  -1 -18 -25)
  (chord 24 6 -0.2 nil   5  3  0  -4  -7)
  (chord 18 6 -0.3 nil  10  2  0  -7 -14)
  (chord 54 6 -0.2 nil   7  2 -2 -14 -21)

  (chord 24 7 -0.2 nil   4  0 -3 -8)
  (chord 18 6 -0.3 nil  14 10  5  0 -6 -10)
  (chord 54 7 -0.2 nil   9  6  2 -1 -8 -13)

  (chord 24 7 -0.2 nil   8  4  1 -1 -10 -11)
  (chord 18 6 -0.3 nil   6  4  1 -6 -14)
  (chord 54 7 -0.2 nil  13 10  6  3  -8 -13)

  (chord 24 7 -0.2 nil  12  8  5  3  -7)
  (chord 18 6 -0.3 nil  10  8  2 -4 -14)
  (chord 54 7 -0.2 nil  14 10  5  0  -5 -9)

  (chord 18 6 -0.3 nil  11  8  3  1 -11)
  (chord 18 7 -0.3 t    10  6  4  1  -6)
  (rst 12))

(defpattern section-1 ()
  (para (bassline-1A)
        (sax-1A)
        (chords-1A))
  (para (sax-1BCD)
        (seq (bassline-1B)
             (bassline-1C)
             (bassline-1D))
        (chords-1BCD)))

;;; ------------------------------------------------------------

;; TODO: Fix note durations here
(defpattern bassline-2A ()
  (tri 24 (et  -1) :d 23 :vibrato-delay 12)
  (tri 24 (et  -3) :d 22)
  (tri 24 (et  -5) :d 22 :vibrato-delay 6)
  (tri 18 (et  -7) :d 16)
  (tri 18 (et -14) :d 16 :vibrato-delay 12)
  (tri 12 (et  -9) :d 9)
  (tri 12 (et  -5) :d 9)
  (tri 12 (et  -2) :d 9)
  (tri 12 (et  -3) :d 9)
  (tri 12 (et  -5) :d 9)
  (tri 12 (et  -6) :d 9)
  (tri 12 (et  -8) :d 11 :vibrato-delay 4))

(defpattern bassline-2B ()
  (tri 24 (et  -5) :d 23 :vibrato-delay 12)
  (tri 24 (et  -7) :d 22)
  (tri 24 (et  -9) :d 20 :vibrato-delay 6)
  (tri 18 (et -11) :d 17)
  (tri 18 (et -13) :d 16 :vibrato-delay 12)
  (tri 12 (et  -6) :d 9)
  (tri 12 (et  -2) :d 9)
  (tri 12 (et  -1) :d 9)
  (tri 12 (et  -7) :d 9)
  (tri 12 (et  -5) :d 9)
  (tri 12 (et  -4) :d 9)
  (tri 12 (et  -2) :d 11))

(defpattern bassline-2CD ()
  (apply 'seq
         (mapcar (lambda (pitch-or-whatevs)
                   (etypecase pitch-or-whatevs
                     (integer (tri 12 (et pitch-or-whatevs) :d 8))
                     (list (apply 'tri 12 (et (first pitch-or-whatevs)) (rest pitch-or-whatevs)))))

                 '(-9 -2 -5 (-8 :d 10)
                   -3 (-3 :vibrato-delay 0) -10 (-10 :vibrato-delay 0)
                   -5 -3 -1 -5
                   (-8 :d 10 :vibrato-delay 0) -11 (-6 :d 12 :vibrato-delay 0) -14

                   -13 -6 -3 (-1 :d 10 :vibrato-delay 4)
                   (-7 :d 10) -5 (-4 :d 10) -2
                   -9 -10 (-12 :vibrato-delay 4) -14
                   (-11 :d 10) -8 (-6 :d 10 :vibrato-delay 4) -14))))

(defpattern sax-2A (:accompany ((bassline-2A)))
  (bup 18 24)
  (note 0 24 (et 14) :vibrato-delay 10)
  (note 0 24 (et 11) :vibrato-delay 10)
  (note 0 18 (et  7))
  (note 0 54 (et 10) :vibrato-delay 20)
  (bup 11 18)
  (note 0 30 (et 9) :d 27 :vibrato-delay 8))

(defpattern sax-2B (:accompany ((bassline-2B)))
  (note 0 24 (et 14) :vibrato-delay 10 :d 21 :cfg *sax-config*)
  (note 0 24 (et 11) :vibrato-delay 10 :d 21)
  (bup 7 24)
  (note 0 18 (et 3))
  (note 0 54 (et 6) :vibrato-delay 24 :d 48))

(defpattern sax-2C ()
  (bup 7 24)
  (note 0 18 (et 5) :d 20)
  (note 0 54 (et 10) :d 48 :vibrato-delay 18)
  (bup 11 24)
  (note 0 18 (et 9) :d 15)
  (note 0 54 (et 14) :d 48 :vibrato-delay 10)
  (note 0 24 (et 15) :d 17)
  (note 0 18 (et 15) :d 17 :vibrato-delay 0)
  (note 0 54 (et 18) :d 47 :vibrato-delay 18)
  (note 0 24 (et 19) :d 18 :vibrato-delay 6)
  (note 0 18 (et 19) :d 16 :vibrato-delay 10)
  (note 0 54 (et 22) :d 48 :vibrato-delay 16))

(defpattern sax-2D ()
  (note 0  6 (et 13) :cfg *sax-config*)
  (note 0  6 (et 16))
  (note 0  6 (et 20))
  (note 0  6 (et 23))
  (note 0 18 (et 22) :vibrato-delay 5)
  (note 0  6 (et 19)))

(defpattern chords-2A (#|:accompany ((bassline-2A) (sax-2A))|#)
  (chord 24 7 -0.2 nil  13 -13)
  (chord 24 6 -0.2 nil  11   6  5   0  -6 -15)
  (chord 24 7 -0.2 nil   9   6  2  -1 -10 -17)
  (chord 18 6 -0.3 nil   7   2  0  -4 -14 -19)
  (chord 54 7 -0.2 nil   2  -2 -5 -13 -21)
  (chord 18 6 -0.3 nil   4   0 -5  -8 -15)
  (chord 30 7 -0.3   t  14  10  5   0  -6 -10))

(defpattern chords-2B ()
  (chord 24 7 -0.2 nil   9   6   2  -1  -8 -17)
  (chord 24 6 -0.2 nil   7   2   0  -3 -10 -19)
  (chord 24 7 -0.2 nil   2  -2  -5 -14 -21)
  (chord 18 6 -0.3 nil   6   3  -2 -13 -23)
  (chord 54 7 -0.2 nil  10   6   3  -3 -18 -25))

(defpattern chords-2C ()
  (rst 24)
  (chord 18 7 -0.2 nil   7   2   0  -2 -10 -16)
  (chord 54 7 -0.2 nil   2  -2  -5 -14 -21)
  (chord 24 7 -0.2 nil   4   0  -3  -8)
  (chord 18 6 -0.2 nil  14  10   5   0  -6 -10)
  (chord 54 7 -0.2 nil   9   6   2  -1  -8 -13))

(defpattern chords-2D ()
  (chord 24 7 -0.2 nil   8   4   1  -1  -8 -11)
  (chord 18 6 -0.2 nil   6   4   1  -6 -14)
  (chord 54 7 -0.2 nil  13  10   5   3  -6 -13)
  (chord 24 7 -0.2 nil  12   8   5   3  -7)
  (chord 18 6 -0.3 nil  10   8   2  -4 -14)
  (chord 54 7 -0.2 nil  14  10   5   0  -5  -9)
  (chord 24 7 -0.2 nil  11   8   4  -1 -11)
  (chord 12 8 -0.3   t  13  10   6   4 -10  -6)
  (rst 12))

(defpattern section-2 ()
  (para
   (seq (bassline-2A)
        (bassline-2B)
        (bassline-2CD))
    (seq (sax-2A)
        (sax-2B)
        (sax-2C)
        (sax-2D))
   (seq (chords-2A)
        (chords-2B)
        (chords-2C)
        (chords-2D))))

;;; ------------------------------------------------------------

(defun walking-bassline (notes &key (note-length 12))
  (assert (not (zerop note-length)))
  (apply
   'seq
   (mapcar (lambda (x)
             (etypecase x
               (integer (tri note-length (et x) :d (max 1 (round (* 0.7 note-length)))))
               (list (walking-bassline x :note-length (ash note-length -1)))))
           notes)))

(defpattern bassline-3A ()
  (walking-bassline
   '(-13 -1  -3 -6
     -10 -5  -7 -2
      -9 -2  -5 -9
      -8 -3 -10 -3)))

(defpattern bassline-3B ()
  (walking-bassline
   '(-5 -10 -14 -2
     -9  -2  -1 -6
     -1  -6  -2 -6)))

(defpattern bassline-3C ()
  (walking-bassline
   '(-7 -5 -4 -2
     -9 -2 -5 -9
     -8 (-3 -8) -10 -3
     -5 -10 -14 -10)))

(defpattern bassline-3D ()
  (walking-bassline
   '(-11  -8  -6 -14
     -13  -6  -2  -1
      -6  -5  -4  -2
      -9 -10 -12 -14
     -11  -8  -6 -11)))                 ; sounds weird...

(defun fast-line (notes)
  (apply
   'seq
   (mapcar
    (lambda (length pitch)
      (etypecase pitch
        (integer (note 0 length (et pitch) :cfg (list :env nil :loop nil :duty (random 3) :vol 7)))
        (null (rst length))))
    '#1=(6 . #1#)
    notes)))

(defpattern sax-3A (:accompany ((bassline-3A)))
  (fast-line
   '(18 15 11  8 14 16 18 21
     19 14 11  7 12  8  7  5
      3  5  7  8 11 12 14 17
     16 12  9  7  6 15 14 12)))

(defpattern sax-3B (:accompany ((bassline-3B)))
  (fast-line
   '(11 14 19 23 14 17 20 24
     15 17 19 22 16 20 23 25))
  (bup 22 12)
  (note 0 6 (et 20))
  (note 0 18 (et 18) :vibrato-delay 4)
  (rst 12))

(defpattern sax-3C (:accompany ((bassline-3C)))
  (fast-line
   '(22 21 20 19 17 15 14 12
     10 20 19 14 17 15 14 17
     16 12  9  4  7  4  6 15
     14 12 11  9  7  9 11 14)))

(defpattern sax-3D (:accompany ((bassline-3D)))
  (fast-line
   '(15 13 11 10 nil 6 8 9
     11 13 15 18 22 21 20 19))
  (note 0 (+ 12 4) (et 17) :vibrato-delay 6)
  (rst (- 12 4))
  (fast-line '(19 18 17 14))
  (fast-line '(15 17 nil 22 27 22 nil nil))
  (rst 12)
  (note 0 4 (et 22))
  (note 0 4 (et 23))
  (note 0 (+ 4 18) (et 22))
  (note 0 6 (et 18)))

(defpattern chords-3A (:accompany ((sax-3A) (bassline-3A)))
  (chord 12 7 -0.2 t  13 10 6 3 -6 -13)
  (rst 12)
  (chord 12 7 -0.2 t  11 6 4 0 -6 -15)
  (rst 12)
  (rst 6)                               ; fixme, there's a bass note here
  (chord (+ 6 4) 6 -0.3 t  9 6 2 -8 -13)
  (rst (- 12 4))
  (chord 24 6 -0.2 t  7 2 0 -4 -7 14)
  (chord 12 6 -0.2 t  10 7 2 -2 -9)
  (rst (+ 12 24))
  (chord 12 7 -0.2 t -8 4 0 -5)
  (rst 12)
  (chord 12 7 -0.2 t 14 10 5 0 -6)
  (rst 12))

(defpattern chords-3B (:accompany ((sax-3B) (bassline-3B)))
  (chord 12 7 -0.2 t  9 6 2 -1)
  (rst 12)
  (chord 24 7 -0.2 t  7 2 0 -4 -10 -21)
  (chord 24 7 -0.2 t  2 -2 -5 -14 -21)
  (chord 24 7 -0.2 t  4 -18)
  (rst 12)
  (chord (+ 24 12) 7 -0.2 t  6 3 -2 -6 -13))

(defpattern chords-3C (:accompany ((bassline-3C) (sax-3C)))
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  7 3 0 -4 -7)
  (rst (- 12 4))
  (chord 24 7 -0.2 t  7 2 0 -3 -7)

  (chord 12 7 -0.2 t  2 -2 -9 -14)
  (rst 12)
  (chord 12 7 -0.2 t  3 -2 -5 -14)
  (rst 12)

  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  4 0 -5 -8)
  (rst (- 12 4))
  (chord 24 8 -0.2 t  14 10 6 0 -6)

  (chord (+ 12 6) 7 -0.2 t  9 6 2 -1 -5 -10)
  (rst (- 36 6)))

(defpattern chords-3D (:accompany ((bassline-3D) (sax-3D)))
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  13 8 4 1 -11)
  (rst (- 12 4))
  (chord 24 7 -0.2 t  10 6 4 -2 -6)

  (chord 12 7 -0.2 t  13 6 3 -6 -13)
  (rst 12)
  (chord 12 7 -0.2 t  10 6 3 -6 -13)
  (rst 12)

  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  7 3 0 -4 -9)
  (rst (- 12 4))
  (chord 12 7 -0.2 t  10 5 2 0 -3)
  (rst 12)                              ; skipped random bass note
  (chord 12 7 -0.2 t  7 2 -2 -9)
  (rst 12)
  (chord 12 7 -0.2 t  5 2 -2 -12)
  (rst 12)

  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  11 8 4 1 -11)
  (rst (- 12 4))
  (chord 12 7 -0.2 t  13 10 6 4 -2 -6)
  (rst 12))


(defpattern section-3 ()
  (para (bassline-3A)
        (sax-3A)
        (chords-3A))
  (para (bassline-3B)
        (sax-3B)
        (chords-3B))
  (para (bassline-3C)
        (sax-3C)
        (chords-3C))
  (para (bassline-3D)
        (sax-3D)
        (chords-3D)))

;;; ------------------------------------------------------------

(defpattern bassline-4A ()
  (walking-bassline
   '(-13 -1 -3 -6
     -10 -13 -14 -7
     -14 -9 -5 -2
     -3 -5 -6 -8)))

(defpattern chords-4A (:accompany ((bassline-4A)))
  (chord (+ 12 4) 7 -0.2 t  13 10 6 3 -6 -13)
  (rst (- 12 4))
  (chord 24 7 -0.2 nil  11 6 4 0 -6 -10)
  (chord 24 7 -0.2 nil  9 6 2 -1 -8 -13)
  (chord 24 6 -0.2 nil  7 2 -2 -4 -14)
  (chord (+ 12 4) 7 -0.2 t  10 7 2 -5 -9)
  (rst (- 12 4))
  (chord (+ 12 4) 7 -0.2 t  10 7 2 -5 -9)
  (rst (- 12 4))
  (rst 6)                               ; skipped bass note..
  (chord (+ 6 4) 6 -0.2 t  11 7 4 0 -8)
  (rst (- 12 4))
  (chord 24 7 -0.2 t  14 9 6 4 2 0 -6))

(defpattern sax-4A (:accompany ((bassline-4A) (chords-4A)))
  (note 0 1 (et 23) :cfg *sax-long*)
  (note 0 1 (et 24) :cfg *sax-long*)
  (note 0 1 (et 25) :cfg *sax-long*)
  (note 0 39 (et 26) :vibrato-delay 16 :cfg *sax-long*)
  (note 0  3 (et 25) :cfg *sax-config*)
  (note 0  3 (et 24))

  (fast-line '(23 14 19 23 22 21 20 24))

  (fast-line '(22 19 17 15 19 15))
  (rst 12)

  (bup 26 18 :vibrato-delay 12)
  (note 0 6 (et 22))
  (rst 6)
  (note 0 6 (et 18))
  (note 0 12 (et 14)))

(defpattern bassline-4B ()
  (walking-bassline
   '(-10 -13 -14 -2
     -9 -2 -6 -11
     -13 -6 -2 -1
     -7 -5 -4 -2)))

(defpattern chords-4B (:accompany ((bassline-4B)))
  (chord 24 7 -0.2 nil  9 6 2 -1 -8 -17)
  (chord 24 7 -0.2 nil  7 2 0 -4 -14 -19)

  (chord 24 7 -0.2 nil  2 -2 -5 -14 -21)
  (chord 24 7 -0.2 nil  4 -1 -6 -11 -18)

  (chord 24 7 -0.2 t  10 6 1 -6 -13)
  (chord (+ 12 6) 7 -0.2 t  13 10 6 -2 -6 -13)
  (rst (- 12 6))

  (rst 6)
  (chord 10 7 -0.2 t  8 3 0 -3 -12)
  (rst 8)
  (chord 24 7 -0.2 t  10 5 2 0 -2))

(defpattern sax-4B (:accompany ((bassline-4B)))
  (fast-line '(nil 14 19 23 14 17 20 24))
  (fast-line '(15 17 19 22 16 18 20 23))
  (note 0 18 (et 18) :vibrato-delay 4)
  (fast-line '(15 13 12 11 7))
  (fast-line '(12 14 15 12 19 17 14 12)))

(defpattern bassline-4C ()
  (walking-bassline
   '(-9 -2 -5 -8
     -3 -5 -6 -8
     -10 -12 -13 -10
     -11 -8 -6 -11)))

(defpattern chords-4C (:accompany ((bassline-4C)))
  (chord 18 7 -0.2 t  10 7 2 -5 -9)
  (rst (+ 6 24))
  (chord 24 7 -0.2 nil  4 0 -3)
  (chord 24 7 -0.2 nil  3 0 -3 -6)
  (chord 18 7 -0.2 t  2 -1 -3 -8)
  (rst 6)
  (chord 18 7 -0.2 t  7 2 -1 -3)
  (rst 6)
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  8 4 -1 -8 -11)
  (rst (- 12 4))
  (chord 24 7 -0.2 t  10 6 1 -2))

(defpattern sax-4C (:accompany ((bassline-4C)))
  (note 0 12 (et 10) :cfg *sax-config*)
  (fast-line '(nil 20 19 15 nil nil))

  (rst 6)
  (note 0 6 (et 9))
  (note 0 4 (et 12))
  (note 0 4 (et 16))
  (note 0 4 (et 19))
  (fast-line '(23 21 18 16))

  (fast-line '(14 16 18 21 19 21 23))
  (note 0 (+ 6 12) (et 22))

  (note 0 1 (et 22.5) :cfg *sax-long*)
  (note 0 (- 36 1) (et 23) :vibrato-delay 10))

(defpattern bassline-4D ()
  (walking-bassline
   '(-13 -6 -2 -1
     -6 -5 -4 -3
     -2 -4 -5 -7
     -8 -11 -6 -14)))

(defpattern chords-4D (:accompany ((bassline-4D)))
  (chord 24 7 -0.2 t  13 10 6 3 -6 -13)
  (chord (+ 12 4) 7 -0.3 t  6 3 -2 -6)
  (rst (- 12 4))

  (rst 6)
  (chord (+ 6 4) 7 -0.3 t  8 3 0 -7)
  (rst (- 12 4))
  (chord 18 7 -0.3 t  10 5 2 0 -9)
  (rst 6)

  (chord 18 7 -0.2 t  7 2 -2 -9)
  (chord (+ 6 6) 7 -0.2 t  7 2 -2 9 -12)
  (rst 6)
  (chord 12 7 -0.2 t  3 -2 -12)

  (rst 6)
  (chord (+ 6 4) 7 -0.2 t  11 8 4 -8 -11)
  (rst (- 12 4))
  (chord (+ 12 5) 7 -0.2 t  10 6 4 -6 -12)
  (rst (- 12 5)))

(defpattern sax-4D (:accompany ((bassline-4D)))
  (fast-line '(22 18 20 22 18 15 13 11))
  (fast-line '(12 14 15 12 19 17 14 12))
  (note 0 18 (et 10))
  (note 0 (+ 6 9) (et 19) :vibrato-delay 4)
  (rst (- 24 9))
  (rst 6)
  (note 0 6 (et 13))
  (note 0 4 (et 16))
  (note 0 4 (et 20))
  (note 0 4 (et 23))
  (fast-line '(18 19 22 25)))

(defpattern section-4 ()
  (para (bassline-4A)
        (chords-4A)
        (sax-4A))
  (para (bassline-4B)
        (chords-4B)
        (sax-4B))
  (para (bassline-4C)
        (chords-4C)
        (sax-4C))
  (para (bassline-4D)
        (chords-4D)
        (sax-4D)))

(defpattern testme ()
  (section-3)
  (section-4))

(defpattern bassline-5A ()
  (walking-bassline
   '(-13 -1 -3 -6
     -10 -13 -14 -2
     -9 -2 -5 -9
     -8 -5 -3 -10)))

(defpattern chords-5A (:accompany ((bassline-5A)))
  (chord 24 7 -0.2 nil 13 10 6 3 -6 -13)
  (chord 24 6 -0.2 nil 11 6 4 0 -10 -15)
  (chord 24 7 -0.2 nil 9 6 2 -1 -8 -17)
  (chord 24 6 -0.2 nil 7 2 0 -4 -14 -19)
  (chord (+ 12 4) 7 -0.2 t 2 -2 -5 -14 -21)
  (rst (- 12 4))
  (chord (+ 12 4) 7 -0.3 t 5 0 -5)
  (rst (- 12 4))
  (rst 6)
  (chord (+ 6 4) 7 -0.3 t 11 4 0 -5 -15)
  (rst (- 12 4))
  (chord 24 7 -0.2 t 11 6 0 -10))

(defpattern sax-5A (:accompany ((bassline-5A) (chords-5A)))
  (fast-line '(23 13 15 18  14 16 17 21
               19 14 11  7  12  8  7  5
                3  5  7  8  10 12 14 17
               16 20 19 nil 23 14 16 18)))

(defpattern bassline-5B ()
  (walking-bassline
   '(-5 -7 -7 -2
     -9 -2 -6 -11
     -13 -6 -2 -1
     -7 -5 -4 -3)))

(defpattern chords-5B ()
  (chord (+ 12 4) 7 -0.2 t 9 2 -1 -10 -17)
  (rst (- 12 4))
  (chord 24 7 -0.2 nil 7 2 0 -4 -14 -19)
  (chord 24 7 -0.2 nil 2 -2 -5 -14 -21)
  (chord 24 7 -0.2 nil 4 -1 -6 -11 -18)
  (chord (+ 12 6) 7 -0.2 t 6 1 -2 -6 -13)
  (rst (- 12 6))
  (rst 24)
  (rst 6)
  (chord (+ 6 6) 7 -0.3 t 7 3 0 -4 -9)
  (rst (- 12 6))
  (chord 24 7 -0.2 t 7 2 0 -4 -14))

(defpattern sax-5B (:accompany ((bassline-5B) (chords-5B)))
  (fast-line '(19 21 23 26 20 24 22 20
               19 15 17 19 16 18 20 22))
  (note 0 12 (et 25))
  (fast-line '(22 20 18 15))
  (rst 12)
  (rst 24)
  (bup 26 12)
  (note 0 6 (et 14))
  (note 0 6 (et 24)))

(defpattern bassline-5C ()
  (walking-bassline
   '(-2 -4 -5 -9
     -8 -3 -10 -3
     -5 -10 -13 -10
     -11 -8 -6 -14)))

(defpattern chords-5C ()
  (chord (+ 12 6) 7 -0.2 t 10 5 0 -5 -9)
  (rst (- 12 6))
  (rst 24)
  (rst 6)
  (chord (+ 6 6) 7 -0.3 t 4 0 -5 -8)
  (rst (- 12 6))
  (chord 24 7 -0.2 nil 14 10 5 0 -6)
  (chord 24 7 -0.2 t 9 6 2 -1 -3 -10)
  (rst 24)
  (rst 6)
  (chord (+ 6 6) 7 -0.3 t 18 11 6 -1 -4)
  (rst (- 12 6))
  (chord 24 7 -0.2 t 19 13 10 -2 -6))

(defpattern sax-5C ()
  (fast-line '(22 20 19 17 15 17 nil 22))
  (note 0 (+ 36 6) (et 26) :vibrato-delay 10)
  (note 0 3 (et 25))
  (note 0 3 (et 24))
  (fast-line '(23 18 21 18 19 14 11 7
               15 11 10  6  8  9  5 9)))

(defpattern bassline-5D ()
  (walking-bassline
   '(-13 -6 -2 -1
     -7 -5 -4 -3
     -2 -4 -5 -9
     -11 -8 -6 -11)))

(defpattern chords-5D ()
  (chord 36 7 -0.15 t 18 15 10 2 -6 -13)
  (rst 12)
  (rst 6)
  (chord (+ 6 6) 7 -0.2 t 12 8 1 -4 -7)
  (rst (- 12 6))
  (chord 24 7 -0.2 nil 11 7 1 8 1 -4)
  (chord 12 7 -0.2 t 10 5 2 -6 -8)
  (rst 12)
  (chord 12 7 -0.2 t 10 5 0 -6 -8)
  (rst 12)
  (rst 6)
  (chord (+ 6 6) 7 -0.2 t 11 8 4 -1 -4 -11)
  (rst (- 12 6))
  (chord 18 7 -0.2 t 10 6 4 -2 -6)
  (rst 6))

(defpattern sax-5D ()
  (fast-line '(11 13 15 18 21 18 15 11))
  (bup 20 18)
  (note 0 3 (et 19))
  (note 0 3 (et 18))
  (fast-line '(17 15 14 12))
  (fast-line '(10 8 7 5 3 5 7 8))
  (note 0 12 (et 15) :cfg *sax-config*)
  (rst 12)
  (rst 24))

(defpattern section-5 ()
  (para (bassline-5A)
        (chords-5A)
        (sax-5A))
  (para (bassline-5B)
        (chords-5B)
        (sax-5B))
  (para (bassline-5C)
        (chords-5C)
        (sax-5C))
  (para (bassline-5D)
        (chords-5D)
        (sax-5D)))

(defpattern testme2 ()
  (section-4)
  (section-5))

(defpattern bassline-6A ()
  (walking-bassline '(-11 -1 -3 -6 -10 -13 -14 -2 -9 -2 -5 -9 -3 -1 0 2)))

(defpattern bassline-6B ()
  (walking-bassline '(-5 -10 -14 -2 -9 -2 -6 -11 -13 -6 -2 -1 -7 -5 -4 -3)))

(defpattern bassline-6C ()
  (walking-bassline '(-2 -4 -5 -9 -8 -3 -10 -3 -5 -3 -1 -5 -1 -8 -2 -6)))

(defpattern bassline-6D ()
  (walking-bassline '(-1 (-2 -1) -2 -1 -7 -5 -4 -3 -2 -4 -5 -9 -8 -11 -6 -14)))

(defpattern chords-6A (:accompany ((bassline-6A)))
  (chord 12 7 -0.2 t 13 10 6 3 -6 -11)
  (rst 12)
  (chord 24 7 -0.2 nil 11 6 4 0 -10 -15)
  (chord 24 7 -0.2 nil 9 4 2 -1 -8 -17)
  (chord 24 7 -0.2 nil 7 2 0 -4 -10 -19)
  (chord 12 7 -0.2 t 10 7 2 -5 -9)
  (rst 36)
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t 4 0 -5 -8 -15)
  (rst (- 12 4))
  (chord 24 7 -0.2 t 3 0 -3 -6))

(defpattern chords-6B (:accompany ((bassline-6B)))
  (chord 24 7 -0.2 nil 9 2 -1 -8 -17)
  (chord 24 7 -0.2 nil 7 2 0 -4 -14 -19)
  (chord 24 7 -0.2 t 2 -2 -8 -21)
  (rst 24)
  (chord (+ 12 6) 7 -0.2 t 6 3 -9 -13)
  (rst (- 36 6))
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t 7 3 0 -4 -7)
  (rst (- 12 4))
  (chord 24 7 -0.2 t 7 2 0 -3))

(defpattern chords-6C (:accompany ((bassline-6C)))
  (chord (+ 12 4) 7 -0.2 t 7 2 -2 -5 -9)
  (rst (- 36 4))
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t 4 0 -5 -15)
  (rst (- 12 4))
  (chord 24 7 -0.2 nil 14 9 6 4 -6)
  (chord 48 7 -0.15 t 9 6 2 -1 -8)
  (rst 48))

(defpattern chords-6D (:accompany ((bassline-6D)))
  (chord 48 7 -0.15 t 13 10 6 3 -6 -13)
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t 8 3 0 -7)
  (rst (- 12 4))
  (chord 24 7 -0.2 nil 9 4 2 -4)
  (chord 12 7 -0.2 t 7 2 -2 -9)
  (rst 12)
  (chord 12 7 -0.2 t 5 2 -1 -9)
  (rst 12)
  (rst 6)
  (chord (+ 6 4) 7 -0.2 t 11 8 4 -11)
  (rst (- 12 4))
  (chord 24 7 -0.2 t 10 6 4 -2))

(defpattern sax-6A (:accompany ((bassline-6A) (chords-6A)))
  (fast-line '(nil 6 11 15 14 16 18 21
               19 14 11 7 12 8 7 5
               3 5 7 8 10 12 14 17
               16 20 19 nil 23 14 16 21)))

(defpattern sax-6B (:accompany ((bassline-6B) (chords-6B)))
  (fast-line '(19 21 23 24 14 17 20 24
               15 17))
  (note 0 (+ 12 6) (et 19) :vibrato-delay 2)
  (rst (- 24 6))
  (note 0 12 (et 18))
  (fast-line '(15 13 11 12 14 17
               22 21 20 19 17 15 14 12)))

(defpattern sax-6C (:accompany ((bassline-6C) (chords-6C)))
  (fast-line '(10 20 19 14 17 15 14 17))

  (fast-line '(16 12 9 4))
  (note 0 18 (et 7) :vibrato-delay 6)
  (note 0 6 (et 6))

  (note 0 (+ 12 4) (et 14) :vibrato-delay 6)
  (rst (- 6 4))
  (note 0 2 (et 11))
  (note 0 2 (et 14))
  (note 0 2 (et 19))
  (note 0 12 (et 23))
  (rst 12)

  (rst 6)
  (note 0 6 (et 13))
  (note 0 4 (et 16))
  (note 0 4 (et 20))
  (note 0 4 (et 23))
  (note 0 12 (et 22))
  (note 0 12 (et 25)))

(defpattern sax-6D (:accompany ((bassline-6D) (chords-6D)))
  (bup 25 18)
  (note 0 3 (et 22))
  (note 0 3 (et 18))
  (note 0 12 (et 14))
  (rst 6)
  (note 0 (+ 6 18) (et 26) :vibrato-delay 9)
  (note 0 3 (et 25))
  (note 0 3 (et 24))
  (fast-line '(22 20 19 17  15 17 19 22))
  (note 0 (+ 12 4) (et 27) :vibrato-delay 4)
  (rst (- 12 4))
  (rst 6)
  (fast-line '(27 25 23))
  (note 0 18 (et 22))
  (note 0 6 (et 18)))

(defpattern section-6 ()
  (para (bassline-6A)
        (chords-6A)
        (sax-6A))
  (para (bassline-6B)
        (chords-6B)
        (sax-6B))
  (para (bassline-6C)
        (chords-6C)
        (sax-6C))
  (para (bassline-6D)
        (chords-6D)
        (sax-6D)))

(defpattern bassline-7A ()
  (walking-bassline '(-11 -1 -3 -10 -5 -10 -14 -2 -9 -2 -5 -9 -8 -6 -10 -3)))

(defpattern bassline-7B ()
  (walking-bassline '(-5 (-10 -13) -14 -2 -9 -2 -6 -9 (-13 -1) -6 -2 -1 -7 -5 -4 -3)))

(defpattern bassline-7C ()
  (walking-bassline '(-2 -4 -5 -9 -8 -15 -10 -3 -5 -3 -1 -5 0 1 2 -2)))

(defpattern bassline-7D ()
  (walking-bassline '(-1 -2 -3 -1 -7 -5 -4 -3 -2 -4 -5 -9 -4 -8 -9 -11)))

(defpattern chords-7A (:accompany ((bassline-7A)))
  (chord (+ 4 12) 7 -0.2 t  10 6 1 -6 -13)
  (rst (- 12 4))
  (chord 24 7 -0.2 nil  11 6 4 0 -6 -15)
  (chord 24 7 -0.2 nil  9 6 2 -1 -10 -17)
  (chord 24 7 -0.2 nil  7 2 0 -4 -14 -19)
  (chord 24 7 -0.2 nil  2 -2 -7 -14 -21)
  (chord (+ 12 4) 7 -0.2 t  5 2 -1 -13)
  (rst (- 12 4))
  (rst 6)
  (chord (+ 6 6) 7 -0.2 t  7 4 0 -5 -15)
  (rst (- 12 6))
  (chord 12 7 -0.3 t  6 3 0 -6))

;;; ------------------------------------------------------------

(defpattern song ()
  (section-1)
  (section-2)
  (section-3)
  (section-4)
  (section-5))
