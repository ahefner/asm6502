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

(defparameter TX (zp 128))
(defparameter TY (zp 129))

(defparameter TEMP-PTR-L (zp 130))
(defparameter TEMP-PTR-H (zp 131))
(defparameter TEMP-PTR (indi 130))

(defparameter TILEIDX (zp 132))
(defparameter TILETYPE (zp 133))

;;;; Data structures

;;; We're a cartridge, there's no DOS loaded, and I'm going to assume
;;; everything from 600h upward is available to use.

;;; Our logical playfield is 20x12 = 240 tiles
;;; On screen each tile is 2x2 ANTIC mode 4 characters.
;;; 

;;; BOARD is a 20x12 map of gameplay tiles on screen.

;;;  bit 7: impassable by player (bricks or barrier)
;;;  bit 6: vertical explosion
;;;  bit 5: horizontal explosion
;;;  bits 0-4: item/tile encoding
;;;
;;;  1000 0000 - Impassable barrier
;;;  1001 0000 - Bricks
;;;  0000 0000 - Empty (walkable) tile
;;;  0000 0xxx - Power-up (TBD, xxx != 0)
;;;  0000 1xxx - Bomb (xxx indicates strength)
;;;  010x xxxx - Vertical explosion (xxxxx indicates explosion progress)
;;;  001x xxxx - Horizontal explosion
;;;  011x xxxx - H+V (intersecting) explosion

(defconstant BOARD #x0600)

;;; 20x12 byte array of counters for bomb countdowns. For explosions I've moved the
;;; countdown into BOARD because it impacts the visual appearance and I don't want to
;;; have to check two places.
(defconstant COUNTERS #x0700)

;;; 0800h - 0DFFh is the screen buffer.
(defconstant SCREEN #x1000)

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
     (ichr:swizzle-rows
      8
      (ichr:chunkify
       2 8
       (ichr:read-gif (merge-pathnames "boomer3.gif" *path*))))))

;;; Program starts here
   (set-label :start)
   (cld)
   (clc)
   (poke 0 SKCTL)
   (poke 3 SKCTL)
   
   (poke 0 SDMCTL)
   (poke #x00 PMBASE)
   (poke #xA0 CHBAS)

   ;; Configure palette
   (poke #x04 712)			; grey brackground
   (poke #x37 708)			; vaguely red
   (poke #xD6 709)			; green turf
   (poke #x00 710)			; black
   (poke #xFE 711)			; alt color - orange

;;; I set us up the bomb
   (poke 6 (+ SCREEN 256 0))
   (poke 26 (+ SCREEN 256 40))
   (poke 7 (+ SCREEN 256 1))
   (poke 27 (+ SCREEN 256 41))

   ;;; Test out the screen drawing function
   (poke 4 TILEIDX)
   (poke 0 TY)
   (poke 19 TX)
   (as/until :zero
     (jsr 'draw-tile)
     (dec TX)
     #+NIL (dec TX))

   (jsr 'draw-tile)
   ;;(poke 8 TILEIDX)
   (poke #b10010000 TILETYPE)
   (poke 3 TY)
   (poke 2 TX)
   (jsr 'set-tile)

   (pokeword (label 'display-list) SDLIST)
   (poke #x22 SDMCTL)
   
   ;; Halt and catch fire
   (set-label :loop)
   (jmp (mem :loop))

   (with-label display-list
     ;; Funky 5-color text mode
     (db #x70 #x70 #x70)
     ;; Give each pair of lines its own page. This wastes 12*(256-80)
     ;; bytes of RAM but simplifies the math for drawing tiles on the
     ;; screen - and no one says we can't sqeeze extra data in the gaps.
     (loop for y upto 11 do
	   (db #x44)
	   (dw (+ SCREEN (* y 256)))
	   (db #x04))
     (db #x41)
     (dw (label 'display-list)))

   (procedure draw-tile
     "Draw a tile TILEIDX into SCREEN at TX/TY. Doesn't update BOARD!"
     (lda TY)
     (clc)
     (adc (imm (msb SCREEN)))
     (sta TEMP-PTR-H)
     (lda TX)
     (asl)
     (sta TEMP-PTR-L)
     (lda (imm 0))
     (tay)
     (lda TILEIDX)
     (sta TEMP-PTR)
     (adc (imm 1))
     (iny)
     (sta TEMP-PTR)
     (tya)
     (adc (imm 39))
     (tay)
     (lda TILEIDX)
     (clc)
     (adc (imm 20))
     (sta TEMP-PTR)
     (iny)
     (adc (imm 1))
     (sta TEMP-PTR)
     (rts))

   (procedure decode-tiletype
     "Set TILEIDX based on value of TILETYPE"
     (lda TILETYPE)
     (asif :zero
       (sta TILEIDX)
       (rts)
       :else
       (asif :negative
	 (anda (imm #b00010000))
	 (asif :zero
	   (lda (imm 8))	      	; barrier
	   :else
	   (lda (imm 2)))		; brick
	 (sta TILEIDX)
	 (rts)
	 :else				; non-negative
	 ;; TODO: Explosion. bomb, or item.
	 (poke 6 TILEIDX)
	 (rts))))

   (set-label 'table-mul-by-20)
   (loop for i from 0 below 12 do (db (* i 20)))

   (procedure set-tile
     "Sets (into BOARD) and draws (into SCREEN, by way of DRAW-TILE) a tile of TILETYPE at TX / TY"
     (jsr 'decode-tiletype)
     (jsr 'draw-tile)
     (ldx TY)
     (lda (abx 'table-mul-by-20))
     (clc)
     (adc TX)
     (tax)
     (lda TILETYPE)
     (sta (abx BOARD))
     (rts))

   (with-label :init
     (rts))

   (print `(remaining space is ,(- #xBFFA *origin*) bytes))

   ;; At the end of the cartridge there are init and run vectors as well as
   ;; a flags byte that the OS checks when booting.
   (advance-to #xBFFA)
   (dw (label :start) #x0400 (label :init))
   (link global)))
