(in-package :asm6502-nes)

;;;; -------------------------------------------------
;;;;   Definitions for programming the NES / Famicom
;;;; -------------------------------------------------

(defconstant +ntsc-clock-rate+ 1789772.5
  "CPU clock rate of an NTSC NES system. Derived from NTSC video clock.")

;;;; Hardware register addresses:

(defconstant +ppu-cr1+     #x2000)
(defconstant +ppu-cr2+     #x2001)
(defconstant +ppu-status+  #x2002)
(defconstant +spr-addr+    #x2003)
(defconstant +spr-io+      #x2004)
(defconstant +vram-scroll+ #x2005)
(defconstant +vram-addr+   #x2006)
(defconstant +vram-io+     #x2007)

(defconstant +pulse1-control+ #x4000)
(defconstant +pulse1-ramp+    #x4001)
(defconstant +pulse1-fine+    #x4002)
(defconstant +pulse1-coarse+  #x4003)
(defconstant +pulse2-control+ #x4004)
(defconstant +pulse2-ramp+    #x4005)
(defconstant +pulse2-fine+    #x4006)
(defconstant +pulse2-coarse+  #x4007)
(defconstant +tri-cr1+        #x4008)
(defconstant +tri-cr2+        #x4009)
(defconstant +tri-freq1+      #x400A)
(defconstant +tri-freq2+      #x400B)
(defconstant +noise-control+  #x400C)
(defconstant +noise-freq1+    #x400E)
(defconstant +noise-freq2+    #x400F)
(defconstant +dmc-control+    #x4010)
(defconstant +dmc-dac+        #x4011)
(defconstant +dmc-address+    #x4012)
(defconstant +dmc-length+     #x4013)

(defconstant +sprite-dma+     #x4014)
(defconstant +papu-control+   #x4015)

(defconstant +joypad-1+ #x4016)
(defconstant +joypad-2+ #x4017)

;;;; iNES File Output

(defun ines-header (prg-pages chr-pages &key
                    (mirror-mode :horizontal)
                    (mapper 0) sram)
  "Generate an iNES header. PRG pages are 16 kilobyte. CHR pages are 8 kilobyte."
  (setf mirror-mode (case mirror-mode
                      (:horizontal 0)
                      (:vertical 1)
                      (otherwise mirror-mode)))
  (let ((control-1 (logior mirror-mode
                           (ash (logand mapper #x0F) 4)
                           (if sram 2 0)))
        (control-2 (logand mapper #xF0)))
    (vector #x4E #x45 #x53 #x1A prg-pages chr-pages control-1 control-2 0 0 0 0 0 0 0 0)))

(defun write-ines (filename prg &key
                   (chr (make-array 8192 :initial-element 1))
                   (mirror-mode :horizontal)
                   (mapper 0)
                   (sram nil))
  "Write a iNES (.nes) file."
  (assert (zerop (mod (length prg) #x4000)))
  (assert (zerop (mod (length chr) #x2000)))
  (dumpbin filename
           (concatenate 'vector
                        (ines-header (/ (length prg) #x4000)
                                     (/ (length chr) #x2000)
                                     :mirror-mode mirror-mode
                                     :mapper mapper
                                     :sram sram)
                          prg
                          chr))
  (format *trace-output* "~&Created \"~A\"~%" filename)
  (values))

;;;; NES Utilities

(defun ppuaddr (address)
  (poke (msb address) +vram-addr+)
  (poke (lsb address) +vram-addr+))

(defun ppuxy (x y &optional (nametable #x2000))
  (ppuaddr (+ nametable x (* y 32))))


;;;; DAC stuff

;;; I'm sure these maps are way off. My measurement methods are bogus,
;;; and my results inconsistent. Regardless, I'm convinced there's a
;;; significant curve, correcting for which will greatly improve the
;;; quality of digital audio output. To be continued.

(defparameter *dac-reverse-map*
  #(0 1 1 2 3 3 4 5 5 6 7 7 8 9 9 10 11 11 12 13 14 14 15 16 16 17 18 19 19
    20 21 22 23 23 24 25 26 26 27 28 29 30 31 31 32 33 34 35 36 36 37 38 39
    40 41 42 43 44 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62
    63 64 65 66 67 68 69 71 72 73 74 75 76 77 79 80 81 82 83 84 86 87 88 89
    91 92 93 94 96 97 98 100 101 102 104 105 106 108 109 111 112 113 115 116
    118 119 121 122 124 125 127)
  "Mapping to compensate for DMC DAC nonlinearity")

(defparameter *dac-value-map*
  #(0.0 1.5541384 1.5541384 3.0946667 4.621763 4.621763 6.1356025 7.636356
    7.636356 9.124193 10.59928 10.59928 12.061781 13.511853 13.511853
    14.949657 16.375347 16.375347 17.789076 19.190994 20.581245 20.581245
    21.95998 23.327337 23.327337 24.683456 26.02848 27.362541 27.362541
    28.68577 29.9983 31.300266 32.591785 32.591785 33.872993 35.144005
    36.404945 36.404945 37.65593 38.897083 40.128517 41.35034 42.56267
    42.56267 43.765617 44.959286 46.14379 47.31923 48.485706 48.485706
    49.64333 50.792194 51.9324 53.064045 54.187225 55.302036 56.40857
    57.506916 57.506916 58.59717 59.67942 60.753754 61.820248 62.879
    63.93009 64.97361 66.00963 67.03822 68.05949 69.073494 70.08032 71.08003
    72.072716 73.05845 74.0373 75.00932 75.97462 76.933235 77.885254
    78.83074 79.76975 80.70237 81.62865 82.54866 84.3701 85.27167 86.16721
    87.05679 87.940475 88.81831 89.69035 91.41732 92.27234 93.1218 93.96576
    94.80425 95.637344 97.28754 98.10473 98.91673 99.72357 101.32201
    102.1137 102.90043 103.68225 105.23135 105.9987 106.76134 108.27257
    109.021255 109.765396 111.24015 111.97085 112.69715 114.136734
    114.850075 116.2641 116.964836 117.661446 119.04242 119.72685 121.0838
    121.75639 123.08993 123.75097 125.0617 125.71149 127.0)
  "Unquantized output values associated with each entry of the reverse map, for computing error.")

(defun clamp (min max x) (max min (min max x)))

(defun process-dac-waveform (vector &key
                             (prescale 0.5)
                             (white-noise-bits 0.0)
                             (error-feedback 1.0))
  (let ((x 0.0))
    (map 'vector
         (lambda (y)
           (incf x (* prescale y))
           (let ((idx (clamp 0 127 (round x))))
             (prog1 (aref *dac-reverse-map* idx)
               (incf x (* white-noise-bits (- (random 2.0) 1.0)))
               (setf x (* error-feedback
                          (- x (aref *dac-value-map* idx)))))))
         vector)))
