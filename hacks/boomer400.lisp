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

(defparameter PRNGLO (zp 134))
(defparameter PRNGHI (zp 135))

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

   (poke 192 #xD40E)			; NMIEN - enable DLI
   (poke (lsb (label 'dli-handler)) 512)
   (poke (msb (label 'dli-handler)) 513)

   ;; Configure palette and finish display setup
   (poke #x04 712)			; grey brackground
   (poke #x37 708)			; vaguely red
   (poke #xD6 709)			; green turf
   (poke #x00 710)			; black
   (poke #xFE 711)			; alt color - orange

   ;; Change of plans - set the colors for the logo, let the DLI fix
   ;; them back to the playfield colors.
   (poke #x1A 708)
   (poke #x14 709)

   (pokeword (label 'display-list) SDLIST)
   (poke #x22 SDMCTL)

   ;; Initialize random board
   (poke 10 TY)
   (as/until :negative
     (poke 18 TX)
     (as/until :negative
       (jsr 'getrand)
       (asif :negative
	 (lda (imm #x00))			; default - empty square
	 :else
	 (lda (imm #b10010000)))	; brick
       (sta TILETYPE)
       (lda (imm 1))
       (bita TX)
       (asif :not-zero			; Place indestructible barriers on grid
	 (bita TY)
	 (asif :not-zero
	   (lda (imm #b10000000))
	   (sta TILETYPE)))
       (jsr 'set-tile)
       (dec TX))
     (dec TY))

   ;; Zero out the corners of the board so the player always has a starting position
   (poke 0 TILETYPE)
   (poke 0 TX)
   (poke 0 TY)
   (jsr 'set-tile)
   (inc TX)
   (jsr 'set-tile)
   (poke 17 TX)
   (jsr 'set-tile)
   (inc TX)
   (jsr 'set-tile)
   (poke 0 TX)
   (inc TY)
   (jsr 'set-tile)
   (poke 18 TX)
   (jsr 'set-tile)
   (poke 0 TX)
   (poke 10 TY)
   (jsr 'set-tile)
   (inc TX)
   (jsr 'set-tile)
   (poke 17 TX)
   (jsr 'set-tile)
   (inc TX)
   (jsr 'set-tile)
   (poke 0 TX)
   (dec TY)
   (jsr 'set-tile)
   (poke 18 TX)
   (jsr 'set-tile)
   
   ;; Halt and catch fire
   (set-label :loop)
   (jmp (mem :loop))

   (with-label dli-handler
     (pha)
     (lda (imm #x8F))
     (poke #x37 #xD016)			; vaguely red
     (poke #xD6 #xD017)			; green turf

     ;;(poke 0 #xD40F)			; clear NMI status. not sure if I should do this or no.
     (pla)
     (rti))

   (with-label titlebar
     (loop for i from 0 below 18 do (db (+ i 40)))
     (loop repeat 22 do (db 60))
     (loop for i from 0 below 18 do (db (+ i 60)))
     (loop repeat 22 do (db 60)))

   ;; Display list for funky 5-color text mode
   (with-label display-list
     (db #x70 #x70)
     ;; Stick the title header up top with LMS to ROM
     (db #x44)
     (dw (label 'titlebar))
     (db #x04)
     (db #xF0)				; blank line + DLI
     ;; Give each pair of lines its own page. This wastes 11*(256-80)
     ;; bytes of RAM but simplifies the math for drawing tiles on the
     ;; screen - and no one says we can't sqeeze extra data in the gaps.
     (loop for y upto 10 do
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

   (procedure getrand
     (asl PRNGLO)
     (rol PRNGHI)
     (asif :carry
       (lda PRNGHI)
       (eor (imm #x11))
       (sta PRNGHI)
       (lda PRNGLO)
       (eor (imm #xC9))
       (sta PRNGLO))
     (lda PRNGLO)
     (rts))

   (with-label :init
     ;; Initialize PRNG  (maybe)
     (lda PRNGHI)
     (asif :zero
       (lda (imm #x57))
       (sta PRNGLO)
       (sta PRNGHI))
     (rts))

   (print `(remaining space is ,(- #xBFFA *origin*) bytes))

   ;; At the end of the cartridge there are init and run vectors as well as
   ;; a flags byte that the OS checks when booting.
   (advance-to #xBFFA)
   (dw (label :start) #x0400 (label :init))
   (link global)))
