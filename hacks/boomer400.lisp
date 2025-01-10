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

;;; 1000h - 1AFFh is the screen buffer.
;;; Not actually quite 1AFF as there is much empty space in each page.
(defconstant SCREEN #x1000)

;;; If we point PMBASE at $1800 then missiles start at $1B00 and players at $1C00.
(defconstant MY-PMBASE-VAL #x1800)
(defconstant MY-PM0-BUF (+ MY-PMBASE-VAL 1024))

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
   (poke (msb MY-PMBASE-VAL) PMBASE)
   (poke #xA0 CHBAS)
   (poke 0 #xD400)			; DMACTL - screen off

   ;; TEMP: test player pattern
   (let ((y 0))
     (poke #b11111111 (+ 0 y #x1C30))	; 48 pixels into the page to get us to 0,0 on the game grid
     (poke #b11111111 (+ 1 y #x1C30))
     (loop for foo from 2 upto 13 do
       (poke #b10000001 (+ foo y #x1C30)))
     (poke #b11111111 (+ 14 y #x1C30))
     (poke #b11111111 (+ 15 y #x1C30)))

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

   ;; Draw in the borders on the left and right playfield edges
   (poke (msb SCREEN) TEMP-PTR-H)
   (ldx (imm 10))
   (as/until :negative
     (poke 0 TEMP-PTR-L)
     (lda (imm 58))		       ; playfield left border feather
     (ldy (imm 0))
     (sta TEMP-PTR)
     (ldy (imm 40))
     (sta TEMP-PTR)
     (lda (imm 59))		      ; playfield right border feather
     (ldy (imm 39))
     (sta TEMP-PTR)
     (ldy (imm 79))
     (sta TEMP-PTR)

     (inc TEMP-PTR-H)			; advance to next row
     (dex))


   ;; Should I do this after drawing the board? Yes, probably.
   (pokeword (label 'display-list) SDLIST)
   (poke #x22 SDMCTL)

   ;; Initialize random board
   (poke 10 TY)
   (as/until :negative
     (poke 18 TX)
     (as/until :negative
       (jsr 'getrand)
       (asif :negative
	 (lda (imm #x00))		; default - empty square
	 :else
	 (lda (imm #b10010000)))	; brick
       (sta TILETYPE)
       (lda (imm 1))
       (bita TX)
       (asif :not-zero	       ; Place indestructible barriers on grid
	 (bita TY)
	 (asif :not-zero
	   (lda (imm #b10000000))
	   (sta TILETYPE)))
       (jsr 'set-tile)
       (dec TX))
     (dec TY))

   ;; Add extra bricks to make it unlikely the player can run too far
   ;; from their starting corner
   ;;(poke #b10000000 TILETYPE)		; barrier
   (poke #b10010000 TILETYPE)		; brick
   ;; Left and right edges - three brick barrier
   (poke 0 TX)
   (poke 4 TY)
   (jsr 'set-tile)
   (inc TY)
   (jsr 'set-tile)
   (inc TY)
   (jsr 'set-tile)
   (poke 18 TX)
   (jsr 'set-tile)
   (dec TY)
   (jsr 'set-tile)
   (dec TY)
   (jsr 'set-tile)

   ;; Single bricks at the center of the top and bottom row
   (poke 9 TX)
   (poke 0 TY)
   (jsr 'set-tile)
   (poke 10 TY)
   (jsr 'set-tile)

   ;; Fence in the top and bottom row so player can run at most 4 squares left/right
   (poke 4 TX)
   (poke 0 TY)
   (jsr 'set-tile)
   (poke 10 TY)
   (jsr 'set-tile)
   (poke 14 TX)
   (jsr 'set-tile)
   (poke 0 TY)
   (jsr 'set-tile)

   ;; Also the first four-way junctions nearest the corners
   (poke 2 TX)
   (poke 2 TY)
   (jsr 'set-tile)
   (poke 16 TX)
   (jsr 'set-tile)
   (poke 8 TY)
   (jsr 'set-tile)
   (poke 2 TX)
   (jsr 'set-tile)

   ;; And, randomly, their inward corridors... first the top..
   (poke 3 TX)
   (poke 2 TY)
   (jsr 'maybe-set-tile)
   (poke 15 TX)
   (jsr 'maybe-set-tile)
   (poke 2 TX)
   (poke 3 TY)
   (jsr 'maybe-set-tile)
   (poke 16 TX)

   ;; Then the bottom..
   (jsr 'maybe-set-tile)
   (poke 3 TX)
   (poke 8 TY)
   (jsr 'maybe-set-tile)
   (poke 15 TX)
   (jsr 'maybe-set-tile)
   (poke 2 TX)
   (poke 7 TY)
   (jsr 'maybe-set-tile)
   (poke 16 TX)
   (jsr 'maybe-set-tile)

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

   ;; TODO / Idea? Force extra bricks to distance players on the left and right edges.

   (poke #b00111010 #xD400) ; DMACTL - normal playfield, DMA + single line players
   
   ;; Halt and catch fire
   (set-label :loop)
   (poke 0 77)			    ; pin this to disable ATTRACT mode
   ;;   (poke #x55 #xD00D)
   (poke #b00000001 #x26F)		; GPRIOR
   (poke #x1F #xD012)			; COLPM0
   (poke #x6F #xD013)			; COLPM1
   (poke #xAF #xD014)			; COLPM2
   (poke #xEF #xD015)			; COLPM3
   (poke #b00111010 #x22f) ; Enable screen DMA and players (no missiles)
   (poke 52 #xD000)	   ; HPOSP0
   (poke 60 #xD001)	   ; HPOSP1
   (poke 68 #xD002)	   ; HPOSP2
   (poke 76 #xD003)	   ; HPOSP3
   (poke 2 #xD01D)	   ; GRACTL enable players
   (jmp (mem :loop))

   (with-label dli-handler
     (pha)
     (lda (imm #x8F))
     (poke #x37 #xD016)			; vaguely red
     (poke #xD6 #xD017)			; green turf

     ;;(poke 0 #xD40F)			; clear NMI status. not sure if I should do this or no.
     (pla)
     (rti))

   ;;; Don't ask.
   #+NIL (with-label titlebar (loop repeat 80 do (db 0)))

   (with-label titlebar
     (loop for i from 0 below 18 do (db (+ i 40)))
     (loop repeat 22 do (db 60))
     (loop for i from 0 below 18 do (db (+ i 60)))
     (loop repeat 22 do (db 60)))

   ;; Display list for funky 5-color text mode.
   ;; I should move this somewhere stable so I'm not risking alignment issues..
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
     (inc TEMP-PTR-L)	      ; offset everything right by 1 character
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
	 :else			    ; non-negative
	 ;; TODO: Explosion. bomb, or item.
	 (poke 6 TILEIDX)
	 (rts))))

   (set-label 'table-mul-by-20)
   (loop for i from 0 below 12 do (db (* i 20)))

   (procedure maybe-set-tile
     (jsr 'getrand)
     (asif :negative			; return, else fall through into set-tile
       (rts)))

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
