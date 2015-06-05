(nesmus:define-song "Steps" ())

(defpattern bassline-1A ()
  (tri 32 (et  -1) :d 31 :vibrato-delay 12)
  (tri 32 (et  -3) :d 30)

  (tri 32 (et  -5) :d 31 :vibrato-delay 8)
  (tri 24 (et  -7) :d 21)
  (tri 24 (et  -9) :d 23)
  (tri 16 (et -10) :d 13)
  (tri 16 (et -12) :d 13)
  (tri 16 (et -14) :d 13)

  (tri 16 (et  -3) :d 13)
  (tri 16 (et  -5) :d 15)
  (tri 16 (et  -6) :d 13)
  (tri 16 (et  -3) :d 15))

(defpattern bassline-1B ()
  (tri 32 (et  -5) :d 30 :vibrato-delay 12)
  (tri 32 (et  -7) :d 30)

  (tri 32 (et  -9) :d 30 :vibrato-delay 8)
  (tri 24 (et -11) :d 21)
  (tri 24 (et -13) :d 21)
  (tri 16 (et  -6) :d 12)
  (tri 16 (et  -2) :d 12)
  (tri 16 (et  -6) :d 14)

  (tri 16 (et  -7) :d 12)
  (tri 16 (et  -5) :d 12)
  (tri 16 (et  -4) :d 12)
  (tri 16 (et  -2) :d 14 :vibrato-delay 0))

(defpattern bassline-1C ()
  (apply 'seq (mapcar (lambda (pitch duration) (tri 16 (et pitch) :d duration))
                      '(-9  -2 -5  -9
                        -3 -10 -8  -7
                        -5  -3 -1  -5
                        -8 -11 -6 -14)
                      '(11 11 11 14  11 11 11 14  11 11 11 14  11 11 11 15))))

(defpattern bassline-1D ()
  (apply 'seq (mapcar (lambda (pitch duration) (tri 16 (et pitch) :d duration))
                      '(-13 -6 -1 -6
                        -7 -5 -3 -2
                        -9 -2 -5 -8
                        -11 -8 -6 -14)
                      '(11 11 11 14  11 11 11 12  11 11 11 14  11 11 11 11))))

(defpattern bassline-seq-1 ()
  (bassline-1A)
  (bassline-1B)
  (bassline-1C)
  (bassline-1D))

(defparameter *sax-config* '(:env nil :loop nil :duty 1 :vol 7))

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
  (bup 18 32 :vibrato-delay 24)
  (note 0 32 (et 14) :vibrato-delay 0)
  (bup 11 32)
  (note 0 24 (et  7))
  (note 0 72 (et 10) :d 64 :vibrato-delay 12)
  (bup 11 24)
  (note 0 (+ 8 32) (et 9) :vibrato-delay 8))

(defpattern sax-1BCD (:accompany ((seq (bassline-1B)
                                      (bassline-1C)
                                      (bassline-1D))))
  (bup 14 32)
  (note 0 32 (et 11) :vibrato-delay 8)
  (bup 7 32 :vibrato-delay 24)
  (note 0 24 (et 3))
  (note 0 72 (et 6) :d 64 :vibrato-delay 24)
  (bup 7 32)
  (note 0 24 (et 5) :vibrato-delay 0)
  (note 0 72 (et 10) :vibrato-delay 24)
  (note 0 32 (et 11) :d 30 :vibrato-delay 8)
  (note 0 24 (et  9) :d 23)
  (note 0 (+ 8 64) (et 14) :vibrato-delay 8)

  (bup 15 32)
  (note 0 24 (et 15) :d 20)
  (note 0 (+ 8 64) (et 18) :d 64 :vibrato-delay 24)
  (note 0 32 (et 19) :vibrato-delay 8)
  (note 0 24 (et 19) :d 20)
  (note 0 (+ 8 64) (et 22) :d 64 :vibrato-delay 24)

  (note 0 24 (et 18) :vibrato-delay 0)
  (note 0 12 (et 18) :vibrato-delay 0)
  (rst 24))

(defun chord (length volume decay mute &rest notes)
  (arpeggio 1 length (mapcar (lambda (x) (et (+ x 12))) notes)
            :rate (if (<= (length notes) 3) 2 1)
            :duty (constantly 2)
            :volume (volramp volume decay)
            :mute mute))

(defpattern chords-1A (:accompany ((bassline-1A) (sax-1A)))
  (segment 256
    (seq
      (chord 32 7 -0.2 nil  10  6  3  -1  -6 -13)
      (chord 32 6 -0.2 nil   7  2  0  -3  -7 -15)
      (chord 32 7 -0.2 nil   9  6  2  -1 -10 -17)
      (chord 24 6 -0.3 nil   7  2  0  -4 -14 -19)
      (chord 72 7 -0.2 nil   7  5  2  -2 -13)
      (chord 24 6 -0.3 nil   4  0 -5  -8 -15)
      (chord 24 7 -0.3 t     6  2 -3 -10))))

(defpattern chords-1BCD (:accompany ((seq (bassline-1B)
                                          (bassline-1C)
                                          (bassline-1D))
                                       (sax-1BCD)))
  (chord 32 7 -0.2 nil   9  6  2  -3  -8 -17)
  (chord 32 6 -0.2 nil   7  2  0  -4 -10 -19)
  (chord 32 7 -0.2 nil   2 -2 -5 -14 -21)
  (chord 24 6 -0.3 nil   6  3 -2 -13 -23)
  (chord 72 7 -0.2 nil  10  6  3  -1 -18 -25)
  (chord 32 6 -0.2 nil   5  3  0  -4  -7)
  (chord 24 6 -0.3 nil  10  2  0  -7 -14)
  (chord 72 6 -0.2 nil   7  2 -2 -14 -21)

  (chord 32 7 -0.2 nil   4  0 -3 -8)
  (chord 24 6 -0.3 nil  14 10  5  0 -6 -10)
  (chord 72 7 -0.2 nil   9  6  2 -1 -8 -13)

  (chord 32 7 -0.2 nil   8  4  1 -1 -10 -11)
  (chord 24 6 -0.3 nil   6  4  1 -6 -14)
  (chord 72 7 -0.2 nil  13 10  6  3  -8 -13)

  (chord 32 7 -0.2 nil  12  8  5  3  -7)
  (chord 24 6 -0.3 nil  10  8  2 -4 -14)
  (chord 72 7 -0.2 nil  14 10  5  0  -5 -9)

  (chord 24 6 -0.3 nil  11  8  3  1 -11)
  (chord 24 7 -0.3 t    10  6  4  1  -6)
  (rst 16))

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

(defpattern bassline-2A ()
  (tri 32 (et  -1) :d 31 :vibrato-delay 12)
  (tri 32 (et  -3) :d 31)
  (tri 32 (et  -5) :d 29 :vibrato-delay 6)
  (tri 24 (et  -7) :d 23)
  (tri 24 (et -14) :d 22 :vibrato-delay 12)
  (tri 16 (et  -9) :d 11)
  (tri 16 (et  -5) :d 11)
  (tri 16 (et  -2) :d 11)
  (tri 16 (et  -3) :d 11)
  (tri 16 (et  -5) :d 11)
  (tri 16 (et  -6) :d 11)
  (tri 16 (et  -8) :d 14 :vibrato-delay 4))

(defpattern bassline-2B ()
  (tri 32 (et  -5) :d 31 :vibrato-delay 12)
  (tri 32 (et  -7) :d 31)
  (tri 32 (et  -9) :d 29 :vibrato-delay 6)
  (tri 24 (et -11) :d 23)
  (tri 24 (et -13) :d 22 :vibrato-delay 12)
  (tri 16 (et  -6) :d 11)
  (tri 16 (et  -2) :d 11)
  (tri 16 (et  -1) :d 11)
  (tri 16 (et  -7) :d 11)
  (tri 16 (et  -5) :d 11)
  (tri 16 (et  -4) :d 11)
  (tri 16 (et  -2) :d 11))

(defpattern bassline-2CD ()
  (apply 'seq
         (mapcar (lambda (pitch-or-whatevs)
                   (etypecase pitch-or-whatevs
                     (integer (tri 16 (et pitch-or-whatevs) :d 11))
                     (list (apply 'tri 16 (et (first pitch-or-whatevs)) (rest pitch-or-whatevs)))))

                 '(-9 -2 -5 (-8 :d 14)
                   -3 (-3 :d 14 :vibrato-delay 0) -10 (-10 :d 14 :vibrato-delay 0)
                   -5 -3 -1 -5
                   (-8 :d 14 :vibrato-delay 0) -11 (-6 :d 15 :vibrato-delay 0) -14

                   -13 -6 -3 (-1 :d 13 :vibrato-delay 4)
                   (-7 :d 13) -5 (-4 :d 13) -2
                   -9 -10 (-12 :d 13 :vibrato-delay 4) -14
                   (-11 :d 13) -8 (-6 :d 13 :vibrato-delay 4) -14))))

(defpattern sax-2A (:accompany ((bassline-2A)))
  (bup 18 32)
  (note 0 32 (et 14) :vibrato-delay 12)
  (note 0 32 (et 11) :vibrato-delay 12)
  (note 0 24 (et  7))
  (note 0 72 (et 10) :vibrato-delay 24)
  (bup 11 24)
  (note 0 40 (et 9) :d 36 :vibrato-delay 8))

(defpattern sax-2B (:accompany ((bassline-2B)))
  (note 0 32 (et 14) :vibrato-delay 12 :d 28 :cfg *sax-config*)
  (note 0 32 (et 11) :vibrato-delay 12 :d 28)
  (bup 7 32)
  (note 0 24 (et 3))
  (note 0 72 (et 6) :vibrato-delay 24 :d 64))

(defpattern sax-2C ()
  (bup 7 32)
  (note 0 24 (et 5) :d 20)
  (note 0 72 (et 10) :d 64 :vibrato-delay 24)
  (bup 11 32)
  (note 0 24 (et 9) :d 20)
  (note 0 72 (et 14) :d 64 :vibrato-delay 12)
  (note 0 32 (et 15) :d 23)
  (note 0 24 (et 15) :d 23 :vibrato-delay 0)
  (note 0 72 (et 18) :d 64 :vibrato-delay 24)
  (note 0 32 (et 19) :d 24 :vibrato-delay 6)
  (note 0 24 (et 19) :d 22 :vibrato-delay 12)
  (note 0 72 (et 22) :d 64 :vibrato-delay 24))

(defpattern sax-2D ()
  (note 0  8 (et 13) :cfg *sax-config*)
  (note 0  8 (et 16))
  (note 0  8 (et 20))
  (note 0  8 (et 23))
  (note 0 24 (et 22) :vibrato-delay 5)
  (note 0  8 (et 19)))

(defpattern chords-2A (#|:accompany ((bassline-2A) (sax-2A))|#)
  (chord 32 7 -0.2 nil  13 -13)
  (chord 32 6 -0.2 nil  11   6  5   0  -6 -15)
  (chord 32 7 -0.2 nil   9   6  2  -1 -10 -17)
  (chord 24 6 -0.3 nil   7   2  0  -4 -14 -19)
  (chord 72 7 -0.2 nil   2  -2 -5 -13 -21)
  (chord 24 6 -0.3 nil   4   0 -5  -8 -15)
  (chord 40 7 -0.3   t  14  10  5   0  -6 -10))

(defpattern chords-2B ()
  (chord 32 7 -0.2 nil   9   6   2  -1  -8 -17)
  (chord 32 6 -0.2 nil   7   2   0  -3 -10 -19)
  (chord 32 7 -0.2 nil   2  -2  -5 -14 -21)
  (chord 24 6 -0.3 nil   6   3  -2 -13 -23)
  (chord 72 7 -0.2 nil  10   6   3  -3 -18 -25))

(defpattern chords-2C ()
  (rst 32)
  (chord 24 7 -0.2 nil   7   2   0  -2 -10 -16)
  (chord 72 7 -0.2 nil   2  -2  -5 -14 -21)
  (chord 32 7 -0.2 nil   4   0  -3  -8)
  (chord 24 6 -0.2 nil  14  10   5   0  -6 -10)
  (chord 72 7 -0.2 nil   9   6   2  -1  -8 -13))

(defpattern chords-2D ()
  (chord 32 7 -0.2 nil   8   4   1  -1  -8 -11)
  (chord 24 6 -0.2 nil   6   4   1  -6 -14)
  (chord 72 7 -0.2 nil  13  10   5   3  -6 -13)
  (chord 32 7 -0.2 nil  12   8   5   3  -7)
  (chord 24 6 -0.3 nil  10   8   2  -4 -14)
  (chord 72 7 -0.2 nil  14  10   5   0  -5  -9)
  (chord 32 7 -0.2 nil  11   8   4  -1 -11)
  (chord 16 8 -0.3   t  13  10   6   4 -10  -6)
  (rst 16))

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

(defun walking-bassline (notes &key (note-length 16))
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
        (integer (note 0 length (et pitch) :cfg *sax-config*))
        (null (rst length))))
    '#1=(9 7 . #1#)
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
  (bup 22 16)
  (note 0 8 (et 20))
  (note 0 24 (et 18) :vibrato-delay 4)
  (rst 16))

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
  (note 0 (+ 16 6) (et 17) :vibrato-delay 6)
  (rst (- 16 6))
  (fast-line '(19 18 17 14))
  (fast-line '(15 17 nil 22 27 22 nil nil))
  (rst 16)
  (note 0 5 (et 22))
  (note 0 5 (et 23))
  (note 0 (+ 6 24) (et 22))
  (note 0 8 (et 6)))



(defpattern chords-3A (:accompany ((sax-3A) (bassline-3A)))
  (chord 16 7 -0.2 t  13 10 6 3 -6 -13)
  (rst 16)
  (chord 16 7 -0.2 t  11 6 4 0 -6 -15)
  (rst 16)
  (rst 8)                               ; fixme, there's a bass note here
  (chord (+ 8 6) 6 -0.3 t  9 6 2 -8 -13)
  (rst (- 16 6))
  (chord 32 6 -0.2 t  7 2 0 -4 -7 14)
  (chord 16 6 -0.2 t  10 7 2 -2 -9)
  (rst (+ 16 32))
  (chord 16 7 -0.2 t -8 4 0 -5)
  (rst 16)
  (chord 16 7 -0.2 t 14 10 5 0 -6)
  (rst 16))

(defpattern chords-3B (:accompany ((sax-3B) (bassline-3B)))
  (chord 16 7 -0.2 t  9 6 2 -1)
  (rst 16)
  (chord 32 7 -0.2 t  7 2 0 -4 -10 -21)
  (chord 32 7 -0.2 t  2 -2 -5 -14 -21)
  (chord 32 7 -0.2 t  4 -18)
  (rst 16)
  (chord 48 7 -0.2 t  6 3 -2 -6 -13))

(defpattern chords-3C (:accompany ((bassline-3C) (sax-3C)))
  (rst 8)
  (chord (+ 8 6) 7 -0.2 t  7 3 0 -4 -7)
  (rst (- 16 6))
  (chord 32 7 -0.2 t  7 2 0 -3 -7)

  (chord 16 7 -0.2 t  2 -2 -9 -14)
  (rst 16)
  (chord 16 7 -0.2 t  3 -2 -5 -14)
  (rst 16)

  (rst 8)
  (chord (+ 8 6) 7 -0.2 t  4 0 -5 -8)
  (rst (- 16 6))
  (chord 32 8 -0.2 t  14 10 6 0 -6)

  (chord (+ 16 8) 7 -0.2 t  9 6 2 -1 -5 -10)
  (rst (- 48 8)))

(defpattern chords-3D (:accompany ((bassline-3D) (sax-3D)))
  (rst 8)
  (chord (+ 8 6) 7 -0.2 t  13 8 4 1 -11)
  (rst (- 16 6))
  (chord 32 7 -0.2 t  10 6 4 -2 -6)

  (chord 16 7 -0.2 t  13 6 3 -6 -13)
  (rst 16)
  (chord 16 7 -0.2 t  10 6 3 -6 -13)
  (rst 16)

  (rst 8)
  (chord (+ 8 6) 7 -0.2 t  7 3 0 -4 -9)
  (rst (- 16 6))
  (chord 16 7 -0.2 t  10 5 2 0 -3)
  (rst 16)                              ; skipping random bass note cuz who gives a shit..
  (chord 16 7 -0.2 t  7 2 -2 -9)
  (rst 16)
  (chord 16 7 -0.2 t  5 2 -2 -12)
  (rst 16)

  (rst 8)
  (chord (+ 8 6) 7 -0.2 t  11 8 4 1 -11)
  (rst (- 16 6))
  (chord 16 7 -0.2 t  13 10 6 4 -2 -6)
  (rst 16))


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

(defpattern song ()
  (section-1)
  (section-2)
  (section-3))
