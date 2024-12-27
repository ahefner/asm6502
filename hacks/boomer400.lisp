(defpackage :boomer400
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility))

(in-package :boomer400)

(defvar *path* #.*compile-file-pathname*)

;;; Atari constants
(defconstant COLBK #xD01A)
(defconstant SKCTL #xD20F)
(defconstant DMACTL #xD400)
(defconstant SDMCTL #x22f)
(defconstant PMBASE #xD407)
(defconstant CHBAS 756)

(defconstant SDLIST #x230)
(defconstant DLIST #xD402)		; ...
(defconstant DLISTL #xD402)
(defconstant DLISTH #xD403)

(defconstant CHBASE #xD409)

(setf
 (binary-file "/tmp/boomer400.car")
 (let* ((global (make-instance 'basic-context :address #xA000))
	(*context* global))
#+NIL
   (db #b00000101
       #b00000101
       #b00000101
       #b00000101
       #b10101111
       #b10101111
       #b10101111
       #b10101111)

   (emit
    (ichr:append-rows
     (ichr:swizzle-rows 16
      (ichr:chunkify 2 8
       (ichr:read-gif (merge-pathnames "boomer3.gif" *path*))))))
   
   (db #b10000000
       #b11000000
       #b11100000
       #b11110000
       #b11111000
       #b11111100
       #b11111110
       #b11111111 )
   
   (set-label :start)
   (poke 0 SKCTL)
   (poke 3 SKCTL)
;;   (poke #x22 DMACTL)
;;   (poke #x3e SDMCTL)
   
   (poke 0 SDMCTL)
   (poke #x00 PMBASE)
   (poke #xA0 CHBAS)


   (poke 0 160)
   (poke 1 200)
   (poke 2 161)
   (poke 3 201)
   
   (poke 8 140)
   (poke 9 180)
   (poke 10 141)
   (poke 11 181)

   (poke 12 144)
   (poke 13 184)
   (poke 14 145)
   (poke 15 185)

   (poke 4 148)
   (poke 5 188)
   (poke 6 149)
   (poke 7 189)


   ;;(pokeword (label 'display-list) DLIST)
   (pokeword (label 'display-list) SDLIST)
   (poke #x22 SDMCTL)

   (poke #x56 (zp 77))
   ;;(poke #x56 (mem COLBK))
   ;; (poke #x07 708)			; Playfield colors
   ;; (poke #x10 709)
   ;; (poke #x3c 710)
   ;; (poke #x57 711)

   ;;(poke #xD6 712)
   (poke #x08 712)
   (poke #x3A 708)
   ;;(poke #x08 709)
   (poke #xD6 709)
   (poke #x00 710)
   (poke #xFE 711)
   
   


   (set-label :loop)

   ;; (lda (zp 77))
   ;; (sta (mem COLBK))
   ;; (inc (zp 77))
   ;; (nop)
   ;; (nop)
   ;; (nop)
   ;; (nop)
   (jmp (mem #xe471))
   (jmp (mem :loop))

   (with-label display-list
     ;; (db #x70 #x70 #x70 #x42)
     ;; ;; #x00 #x38
     ;; ;;(dw (label 'message))
     ;; (dw 0)
     ;; (db #x02 #x02 #x02 #x02 #x82 #x70 #x86 #x70 #x70 #x02
     ;; 	 #x70 #x07 #x70 #x30 #x06 #x70 #x06 #x70 #x30 #x06 #x70 #x70 #x02)

     ;; Plain text mode display list
     (db #x70 #x70 #x70 #x44)
     ;;(dw (label 'message))
     ;;(dw #x3c40)
     (dw 0)
     (loop repeat 23 do (db #x04))
     (db #x41)
     (dw (label 'display-list)))

   (with-label message
     (loop for i from 0 below 256 do (db i))
     (emit (map 'list 'char-code "  YOUR ATARI WORKS  "))
     (emit (loop repeat 20 collect #x20)))

   (with-label :init
     (rts))

   (advance-to #xBFFA)
   (dw (label :start) #x0400 (label :init))
   (link global)))
