(defpackage :boomer400
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility))

(in-package :boomer400)

(defvar *path* #.*compile-file-pathname*)

;;;; Atari constants
(defconstant SDMCTL #x22f)
(defconstant SDLIST #x230)
(defconstant CHBAS 756)

(defconstant SKCTL #xD20F)
(defconstant DMACTL #xD400)
(defconstant PMBASE #xD407)

;;;; Data structures

;;; We're a cartridge, there's no DOS loaded, and I'm going to assume
;;; everything from 600h upward is available to use.

;;; Our logical playfield is 20x12 = 240 tiles
;;; On screen each tile is 2x2 ANTIC mode 4 characters.
;;; 

;;; 20x12 map of gameplay tiles on screen

;;; The scheme below doesn't work. Two shortcomings:
;;;  1) Can't distinguish different bomb strengths
;;;  2) Can't distinguish horizontal vs. vertical vs. intersecting explosions
;;; 
;;; For each byte:
;;;  bit 7: tile impassable by player
;;;  bit 5-6: tile type
;;;   00: empty
;;;   01: item or brick
;;;   10: bomb       (this doesn't work if we have bomb upgrades!)
;;;   11: explosion
;;;  bits 4-0: countdown (bomb countdown or explosion progress) or item type
;;;    item 00000: brick
;;;    other items: ...
(defconstant BOARD #x0600)

;;; At 0700h I was going to put one of the AI pathfinding maps but probably
;;; I'll need to use it for counters or something to fix the above mess.

;;; 0800h is the screen buffer.

;;;; Program

(setf
 (binary-file "/tmp/boomer400.car")
 (let* ((global (make-instance 'basic-context :address #xA000))
	(*context* global))

;;; At the beginning of the ROM, for alignment convenience, we
;;; place the character set.

   ;; Palette test pattern.
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

;;; Program starts here
   (set-label :start)
   (poke 0 SKCTL)
   (poke 3 SKCTL)
   
   (poke 0 SDMCTL)
   (poke #x00 PMBASE)
   (poke #xA0 CHBAS)

;;; For the moment we're looking at the zero page. Bash some
;;; recognizable images in there.
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

   (pokeword (label 'display-list) SDLIST)
   (poke #x22 SDMCTL)

   ;; Configure palette
   (poke #x04 712)			; grey brackground
   (poke #x37 708)			; vaguely red
   (poke #xD6 709)			; green turf
   (poke #x00 710)			; black
   (poke #xFE 711)			; alt color - orange
   
   ;; Halt and catch fire
   (set-label :loop)
   (jmp (mem :loop))

   (with-label display-list
     ;; Funky 5-color text mode
     (db #x70 #x70 #x70 #x44)
     (dw 0)				; Screen buffer
     (loop repeat 23 do (db #x04))
     (db #x41)
     (dw (label 'display-list)))

   (with-label :init
     (rts))

   ;; At the end of the cartridge there are init and run vectors as well as
   ;; a flags byte that the OS checks when booting.
   (advance-to #xBFFA)
   (dw (label :start) #x0400 (label :init))
   (link global)))
