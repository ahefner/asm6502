(defpackage :boomer400
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility))

(in-package :boomer400)

(defvar *path* #.*compile-file-pathname*)

;;; Atari constants
(defconstant COLBK #xD01A)
(defconstant SKCTL #xD20F)
(defconstant DMACTL #xD400)

(defconstant DLIST #xD402)		; ...
(defconstant DLISTL #xD402)
(defconstant DLISTH #xD403)



;;; Hello?
(setf
 (binary-file "/tmp/boomer400.car")
 (let* ((global (make-instance 'basic-context :address #xA000))
	(*context* global))

   (poke 0 SKCTL)
   (poke 3 SKCTL)
   (poke #x22 DMACTL)
   (pokeword (label 'display-list) DLIST)

   (set-label :loop)
   (lda (zp 77))
   (sta (mem COLBK))
   (inc (zp 77))

   (jmp (mem :loop))

   (with-label display-list
;;     (db #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70 #x70)
     (db #x70 #x70 #x70)
     (db #x42)
     (dw (label 'message))
     (loop repeat 23 do (db #x02))
     (db #x41)
     (dw (label 'display-list)))

   (with-label message
     (emit (map 'list 'char-code "  YOUR ATARI WORKS  "))
     (emit (loop repeat 20 collect #x20)))
   
   (advance-to #xBFFA)
   (dw #xA000 #x8000 #xA000)
   (link global)))
