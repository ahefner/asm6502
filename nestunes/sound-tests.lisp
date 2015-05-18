(nesmus:define-song "Audio Tests" ())

(defpattern vibrato-test ()
  (para
   (print (note 0 96 (et 0) :cfg '(:env nil :duty 2)))
   (seq
    (rst 24)
    (repeat 6
     (list (list (register 2 #x2b)))
     (list (list (register 2 #x2c)))
     (list (list (register 2 #x2d)))
     (list (list (register 2 #x2c)))
     (list (list (register 2 #x2b)))
     (list (list (register 2 #x2a)))
     (list (list (register 2 #x29)))
     (list (list (register 2 #x2a)))))))

(defpattern sweep-vibrato-test ()
  (para
   (print (note 0 128 (et 0) :cfg '(:env nil :duty 2)))
   (list (list (register 1 0)))
   (seq
    (rst 24)
    (repeat 8
     (list (list (register 1 #xDF)))
     (rst 2)
     (list (list (register 1 #xD7)))
     (rst 2)))))

(defpattern triangle-vibrato-test ()
  (para
   ;;(print (tri 31  (et 0)))
   (list (list (register 8 #x8C) (register 10 #xCB) (register #xB 8)))
   (seq
    (rst 24)
    (repeat
     6
     (list (list (register 10 #xCB)))
     (list (list (register 10 #xCA)))
     (list (list (register 10 #xC9)))
     (list (list (register 10 #xCA)))
     (list (list (register 10 #xCB)))
     (list (list (register 10 #xCC)))
     (list (list (register 10 #xCD)))
     (list (list (register 10 #xCC)))))))

;;; Test new vibrato feature of TRI function
(defpattern triangle-vibrato-1 ()
  (apply 'seq
   (tri 128 (et -24) :vibrato-delay 24)
   (tri 128 (et -12) :vibrato-delay 24)
   (tri 128 (et   0) :vibrato-delay 24)
   (tri 128 (et  12) :vibrato-delay 24)
   (tri 128 (et  24) :vibrato-delay 24)
   (loop for pitch from -24 upto 24
         collect (tri 32 (et pitch) :vibrato-delay 0))))

(defpattern pulse-vibrato-1 ()
  (apply 'seq
   (cfg 0 :env nil :loop t)
   (note 0 96 (et 0)  :vibrato-delay 24)
   (note 0 96 (et 12) :vibrato-delay 24)
   (note 0 96 (et 24) :vibrato-delay 24)
   (note 0 96 (et 36) :vibrato-delay 24)
   (note 0 96 (et 48) :vibrato-delay 24)
   (loop for pitch from -4 upto 48
        collect (note 0 32 (et pitch) :vibrato-delay 0))))
