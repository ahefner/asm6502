(nesmus:define-song "Steps" ())

(defpattern bassline-1 ()
  (seq
   (tri 32 (et  -1) :d 31 :vibrato-delay 12)
   (tri 32 (et  -3) :d 31)

   (tri 32 (et  -5) :d 31 :vibrato-delay 8)
   (tri 24 (et  -7) :d 23)
   (tri 24 (et  -9) :d 23)
   (tri 16 (et -10) :d 13)
   (tri 16 (et -12) :d 13)
   (tri 16 (et -13) :d 13)

   (tri 16 (et  -3) :d 13)
   (tri 16 (et  -5) :d 13)
   (tri 16 (et  -6) :d 13)
   (tri 16 (et  -3) :d 13)))

(defpattern bassline-2 ()
  (seq
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
   (tri 16 (et  -2) :d 14 :vibrato-delay 0)))

(defpattern bassline-3 ()
  (apply 'seq (mapcar (lambda (pitch duration) (tri 16 (et pitch) :d duration))
                      '(-9  -2 -5  -9
                        -3 -10 -8  -7
                        -5  -3 -1  -5
                        -8 -11 -6 -14)
                      '(13 13 13 14  13 13 13 14  13 13 13 14  13 13 13 15))))

(defpattern bassline-4 ()
  (apply 'seq (mapcar (lambda (pitch duration) (tri 16 (et pitch) :d duration))
                      '(-13 -6 -1 -6
                        -7 -5 -3 -2
                        -9 -2 -5 -8
                        -11 -8 -6 -14)
                      '(13 13 13 14  13 13 13 14  13 13 13 14  13 13 13 15))))

(defpattern bassline-seq ()
  (seq
   (bassline-1)
   (bassline-2)
   (bassline-3)
   (bassline-4)))

(defun bup (note length &key (d (1- length)) vibrato-delay)
  (para
   ;;(note 1 length (et note) :d d :cfg '(:env nil :loop nil :duty 0 :vol 6))
   (note 0 length (* (et (+ note -1.6 ))) :d d :cfg '(:env nil :loop nil :duty 1 :vol 7) :vibrato-delay vibrato-delay)
   (seq (list (list (register 1 #x8F))
              nil
              nil
              nil
              nil
              (list (register 1 0))))))

(defpattern sax-1 (:accompany ((bassline-1)))
  (seq (bup 18 32 :vibrato-delay 24)
       (note 0 32 (et 14) :vibrato-delay 0)
       (bup 11 32)
       (note 0 24 (et  7))
       (note 0 72 (et 10) :d 64 :vibrato-delay 12)
       (bup 11 24)
       (note 0 (+ 8 32) (et 9) :vibrato-delay 8)))

(defpattern sax-234 (:accompany ((seq (bassline-2)
                                      (bassline-3)
                                      (bassline-4))))
  (seq (bup 14 32)
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
       (rst 24)))

(defun chord (length volume decay mute &rest notes)
  (arpeggio 1 length (mapcar (lambda (x) (et (+ x 12))) notes)
            :rate 1
            :duty (constantly 2)
            :volume (volramp volume decay)
            :mute mute))

(defpattern chords-1 () #+NIL (:accompany ((bassline-1) (sax-1)))
  (segment 256
    (seq
      (chord 32 8 -0.2 nil  10  6  3  -1  -6 -13)
      (chord 32 8 -0.2 nil   7  2  0  -3  -7 -15)
      (chord 32 8 -0.2 nil   9  6  2  -1 -10 -17)
      (chord 24 8 -0.3 nil   7  2  0  -4 -14 -19)
      (chord 72 8 -0.2 nil   7  5  2  -2 -13)
      (chord 24 8 -0.3 nil   4  0 -5  -8 -15)
      (chord 24 8 -0.3 t     6  2 -3 -10))))

(defpattern chords-234 nil  #+NIL (:accompany ((seq (bassline-2)
                                         (bassline-3)
                                         (bassline-4))
                                    (sax-234)
                                    ))
  (seq
   (chord 32 8 -0.2 nil   9  6  2  -3  -8 -17)
   (chord 32 8 -0.2 nil   7  2  0  -4 -10 -19)
   (chord 32 8 -0.2 nil   2 -2 -5 -14 -21)
   (chord 24 8 -0.3 nil   6  3 -2 -13 -23)
   (chord 72 8 -0.2 nil  10  6  3  -1 -18 -25)
   (chord 32 8 -0.2 nil   5  3  0  -4  -7)
   (chord 24 8 -0.3 nil  10  2  0  -7 -14)
   (chord 72 8 -0.2 nil   7  2 -2 -14 -21)

   (chord 32 8 -0.2 nil   4  0 -3 -8)
   (chord 24 8 -0.3 nil  14 10  5  0 -6 -10)
   (chord 72 8 -0.2 nil   9  6  2 -1 -8 -13)

   (chord 32 8 -0.2 nil   8  4  1 -1 -10 -11)
   (chord 24 8 -0.3 nil   6  4  1 -6 -14)
   (chord 72 8 -0.2 nil  13 10  6  3  -8 -13)

   (chord 32 8 -0.2 nil  12  8  5  3  -7)
   (chord 24 8 -0.3 nil  10  8  2 -4 -14)
   (chord 72 8 -0.2 nil  14 10  5  0  -5 -9)

   (chord 24 8 -0.3 nil  11  8  3  1 -11)
   (chord 24 8 -0.3 t    10  6  4  1  -6)
   (rst 16)))

(defpattern song ()
  (seq (para (bassline-1)
             (sax-1)
             (chords-1))
       (para (sax-234)
             (seq (bassline-2)
                  (bassline-3)
                  (bassline-4))
             (chords-234))))
