;;;; A collection of simple hacklets, chosen from a menu. Navigate the
;;;; menu using the Select and Start buttons.

;;; This targets my first EPROM cart, a modified SMB2 board with a
;;; 27C256 (64KB) EPROM.  The text output assumes that the original
;;; SMB2 character ROM is present.

;;; There's a simple framework in place here. Each program is defined
;;; using the SUBPROGRAM macro, which emits a short header containing
;;; the name of the program and a pointer to the next byte after the
;;; end of the program. These form a linked list, used to construct
;;; the menu when the cart is reset.

;;; Programming an old 64 KB EPROM takes a long time, so I take some
;;; care to avoid having to erase or completely reprogram the chip. An
;;; erased EPROM with no bad bits will contain all '1' bits, which the
;;; programmer selective flips to zeros.  We can run a programmed chip
;;; through the programmer again, so long as we never have to turn a
;;; bit back on.  Unused space is left with all bits on, and
;;; subprograms are singly-linked forward from the beginning of the
;;; ROM, with the menu code able to distinguish unused space from a
;;; valid subprogram header. If the existing programs aren't disturbed
;;; (except in terribly clever ways), you can add new subprograms
;;; after the old ones and reprogram just that region of the rom. The
;;; CHECK-COMPATIBILITY function compares new and current ROM contents
;;; to verify it's possible to skip erasing the chip.

;;; Terribly clever ways? Well, mildly clever. Once upon a time, I
;;; thought my UV eraser was broken, and I hacked up this menu because
;;; I only had one or two blank 27C256s left. The menu worked in
;;; emulation, but not when I tried it on a real NES. I hadn't
;;; properly initialized the memory mapper, and needed to add a few
;;; instructions. Thinking I couldn't erase the chip, I knew I could
;;; modify the reset address by zeroing out bits, thus moving the
;;; address down in memory, and chose the least significant bit I
;;; could zero out which would move it below the existing code and
;;; give me space to add a new init routine. This worked perfectly,
;;; and I was excessively pleased with my cleverness. Subsequently I
;;; discovered the UV eraser worked fine, I'd just failed repeatedly
;;; to push the door tightly closed, and felt much less clever than I
;;; had.

(defpackage :nes-hacklets
  (:use :common-lisp :asm6502 :6502 :6502-modes :asm6502-nes :asm6502-utility))

(in-package :nes-hacklets)

(defparameter *current-prg* nil       ; "test2-burnt-7.bin"
  "Previously programmed version of PRG ROM. Used to determine whether
  the newly assembled ROM can be safely programmed over top the
  existing EPROM, and to determine lower/upper bounds of the changed
  region, to minimize the programming time.")

(defmacro program ((filename &rest rom-args) &body body)
  `(let* ((program
              (link
               (let ((*context* (make-instance 'basic-context :address #x8000)))
                 ,@body
                 *context*)))
          (current-rom (and *current-prg* (loadbin (merge-pathnames
                                                    *current-prg*
                                                    *path*)))))
     (when current-rom
       (format t "~&PRG size:~X~%Changed extents: ~X-~X~&"
               (length program)
               (mismatch program current-rom
                         :end1 (length current-rom)
                         :end2 (length current-rom))
               (mismatch program current-rom :from-end t
                         :end1 (length current-rom)
                         :end2 (length current-rom)))
       (check-compatibility program current-rom)
       (dumpbin "/tmp/hacklets.bin" program))
     (write-ines ,filename program ,@rom-args)))

(defun check-compatibility (new current)
  (loop for x across new
        for y across current
        for index upfrom 0
        unless (zerop (logand x (logxor x y)))
        do (error "New program cannot be programmed over current program.
New byte at ~X is ~8,'0,,B (versus ~8,'0,,B" index x y)))

(defvar *path* #.*compile-file-truename*)

(defparameter *ticker*  (zp 0))
(defparameter *color*   (zp 1))
(defparameter *chrbank* (zp 4))

(defparameter *jtmp*    (zp #xF0))
(defparameter *lastj*   (zp #xF1))
(defparameter *j1pressed*  (zp #xF2))
(defparameter *j1released* (zp #xF3))

(defparameter *shadow-irq*  #xFE)
(defparameter *shadow-nmi*  #xFC)

(defparameter *default-bank* #x28)

(defun translate-char (char)
  "Translate character from ASCII to the limited set of characters in the SMB2 CHR ROM"
  (setf char (char-upcase char))
  (cond
    ((alpha-char-p char) (+ #xDA (char-code char) (- (char-code #\A))))
    ((alphanumericp char) (+ #xD0 (char-code char) (- (char-code #\0))))
    ((char= char #\-) #xF4)
    ((char= char #\?) #xF5)
    ((char= char #\.) #xF6)
    ((char= char #\,) #xF7)
    ((char= char #\Space) #xFF)
    (t #xF8)))

(defun translate-string (string) (map 'vector #'translate-char string))

(defun vram-string (string)
  "Translate a string from ASCII and write it to the PPU IO port."
  (let ((*context* (make-instance 'asm6502::local-context :parent *context*)))
    (jmp (mem :skip))
    (set-label :data)
    (context-emit *context* (reverse (translate-string string)))
    (set-label :skip)
    (ldx (imm (length string)))
    (set-label :copy)
    (lda (abx (label :data -1)))
    (sta (mem +vram-io+))
    (dex)
    (bne :copy)))

(defun emit-subprogram-header (name end-symbol)
  (dw (label end-symbol))
  (emit (translate-string name))
  (db 0))

(defmacro subprogram ((start-sym name) &body body)
  (let ((end-symbol (gensym name)))
    `(progn
       (emit-subprogram-header ,name ',end-symbol)
       (set-label ',start-sym)
       ,@body
       (set-label ',end-symbol))))

;; Should banks get a local namespace? Probably.
(defmacro bank ((origin size) &body body)
  (assert (<= (+ size origin) #x10000))
  `(progn
     (assert (zerop (logand *origin* (1- ,size)))) ; Dubious
     (setf *origin* ,origin)
     ,@body
     (advance-to (+ ,size ,origin) #xFF)
     (values)))

(program ("/tmp/hacklets.nes"
          :mapper 4
          :chr (loadbin (merge-pathnames "test2m.chr" *path*)))

  (bank (#x8000 #x4000))                ; 0,1
  (bank (#x8000 #x4000))                ; 2,3
  (bank (#x8000 #x4000))                ; 4,5

  ;;; These pages are mapped into the high 16 KB of memory at bootup:

  (bank (#xC000 #x4000)                 ; 7,8

  ;;;; -------------------------------------------------------
  ;;;; Program 1 - "Colors and Characters"

    (subprogram (colors-and-characters "Colors and Characters")
      (sei)
      (poke (lsb (label 'vblank-ticker)) *shadow-nmi*)
      (poke (msb (label 'vblank-ticker)) (1+ *shadow-nmi*))
      (poke (lsb (label 'ignore-irq)) *shadow-irq*)
      (poke (msb (label 'ignore-irq)) (1+ *shadow-irq*))
      (ldx (imm #xFF))
      (txs)
      (poke 0 *color*)
      (poke *default-bank* *chrbank*)

      (jsr (mem (label 'ppu-init)))
      (ldx (imm #xff))
      (jsr (mem (label 'fill-nametable-x)))

      ;; Zero attribute table
      (ppuaddr #x23C0)
      (lda (imm 0))
      (ldx (imm 64))
      (set-label :loop)
      (sta (mem +vram-io+))
      (dex)
      (bne (rel :loop))

      ;; Display something
      (ppuaddr #x2063)
      (ldx (imm 10))
      (set-label :loop)
      (txa)
      (clc)
      (adc (imm #xCF))
      (sta (mem +vram-io+))
      (dex)
      (bne (rel :loop))

      ;; Show the character rom
      (lda (mem +ppu-status+))
      (ldx (imm 0))
      (set-label :loop)
      (txa)
      (anda (imm 15))
      (asif :zero
        (txa)
        (asl)
        (lda (imm 0))
        (adc (imm #x21))
        (sta (mem +vram-addr+))
        (txa)
        (asl)
        (clc)
        (adc (imm 8))
        (sta (mem +vram-addr+)))
      (stx (mem +vram-io+))
      (inx)
      (bne (rel :loop))

      ;; Draw gradient stripes
      (poke #b00000100 +ppu-cr1+)       ; vertical write mode
      (ldy (imm 6))
      (clc)
      (set-label :loop)
      (poke #x20 +vram-addr+)
      (tya)
      (adc (imm #x59))
      (sta (mem +vram-addr+))
      (jsr (mem (label 'write-stripe)))
      (dey)
      (bne (rel :loop))
      (poke #b00000000 +ppu-cr1+)       ; horizontal write mode
      (ppuaddr (+ #x23c0 0 6))          ; alternating attributes
      (poke #b01000000 +vram-io+)
      (poke #b00100000 +vram-io+)
      (ppuaddr (+ #x23c0 8 6))          ; alternating attributes
      (poke #b01000100 +vram-io+)
      (poke #b00101110 +vram-io+)
      (ppuaddr (+ #x23c0 16 6))
      (poke #b01000100 +vram-io+)
      (poke #b00101110 +vram-io+)
      (ppuaddr (+ #x23c0 24 6))
      (poke #b00000100 +vram-io+)
      (poke #b00001110 +vram-io+)

;;; Main loop - alternate palette colors for intermediate shades
      (lda (imm 0))
      (pha)                             ; Color Phase (0 or 1)

      (set-label :mainloop)
      (jsr (mem (label 'configure-ppu)))
      (jsr (mem (label 'wait-for-vblank)))
      (jsr (mem (label 'char-test-process-input)))
      (jsr (mem (label 'program-palette)))

      (pla)
      (eor (imm 1))
      (pha)

      (asif :zero
        (clc)
        (ppuaddr #x3F0D)
        (poke *color* +vram-io+)
        (adc (imm 16))
        (sta (mem +vram-io+))
        (adc (imm 16))
        (sta (mem +vram-io+))

        (ppuaddr #x3F09)
        (lda *color*)
        (adc (imm 1))
        (anda (imm 63))
        (jsr (mem (label 'ramp3)))

        :else
        (ppuaddr #x3F0D)
        (lda *color*)
        (jsr (mem (label 'ramp3)))
        (ppuaddr #x3F09)
        (lda *color*)
        (jsr (mem (label 'ramp3))))

      ;; Select current character bank
      (ldx (imm 0))
      (stx (mem #x8000))
      (lda *chrbank*)
      (sta (mem #x8001))
      (inx)
      (stx (mem #x8000))
      (clc)
      (adc (imm 2))
      (sta (mem #x8001))

      (jmp (mem (label :mainloop)))

;;;; ----------------------------------------------------------------

;;; Process joypad input
      (with-label char-test-process-input
        (jsr (mem (label 'read-joypad-1)))
        (lda *jtmp*)
        (tax)
        (and 15)
        (sta *color*)
        (inc *color*)
        (eor *lastj*)
        ;; Test A Button
        (asl)
        (tay)
        (asif :carry
          (lda *lastj*)
          (asif :positive
            (lda *chrbank*)
            (clc)
            (adc (imm 4))
            (sta *chrbank*)))
        ;; Test B Button
        (tya)
        (asif :negative
          (lda *lastj*)
          (asl)
          (asif :positive
            (ldy *chrbank*)
            (dey) (dey) (dey) (dey)
            (sty *chrbank*)))
        ;; Almost done.
        (stx *lastj*)
        (rts))

;;; Program palette
      (with-label program-palette
        (lda (mem +ppu-status+))
        (ppuaddr #x3F00)
        (clc)
        (lda *color*)
        (jsr (mem (label 'ramp)))
        (adc (imm 17))
        (anda (imm 63))                   ; tail call.
        (set-label 'ramp)
        (sta (mem +vram-io+))
        (set-label 'ramp3)
        (adc (imm 16))
        (sta (mem +vram-io+))
        (adc (imm 16))
        (sta (mem +vram-io+))
        (adc (imm 16))
        (sta (mem +vram-io+))
        (rts))

;;; Reset PPU state at end of vblank
      (with-label configure-ppu
        (lda (mem +ppu-status+))
        (lda (imm 0))
        (sta (mem +vram-addr+))
        (sta (mem +vram-addr+))
        (sta (mem +vram-scroll+))
        (sta (mem +vram-scroll+))
        (poke #b10000000 +ppu-cr1+)  ; enable VBI, horizontal write mode
        (poke #b00001110 +ppu-cr2+)
        (rts))

      (with-label write-stripe
        (clc)
        (ldx (imm 3))
        (set-label :loop)
        (txa)
        (adc (imm #xFB))
        (sta (mem +vram-io+))
        (sta (mem +vram-io+))
        (sta (mem +vram-io+))
        (sta (mem +vram-io+))
        (dex)
        (bne (rel :loop))
        (rts))

      (with-label ignore-irq (rti)))


  ;;;; -------------------------------------------------------

    (subprogram (color-bars-1 "Stripes")
      (poke 0 +ppu-cr2+)
      (ldy (imm 0))
      (lda (imm #x3F))
      (set-label :loop)
      (sta (mem +vram-addr+))
      (sty (mem +vram-addr+))
      (stx (mem +ppu-cr1+))
      (dotimes (i 7) (nop))
      (ldx (imm #x20))
      (dotimes (i 14)
        (stx (mem +vram-io+))
        (inx))
      (jmp (mem (label :loop))))

  ;;;; -------------------------------------------------------

    (subprogram (crazy-stripes "Crazy Stripes")
      (pokeword (label 'crazy-stripes-nmi) *shadow-nmi*)
      (poke #x80 +ppu-cr1+)
      (poke 0 +ppu-cr2+)
      (lda (mem +ppu-status+))
      (ldy (imm 0))
      (lda (imm #x3F))
      (with-label :loop
        (sta (mem +vram-addr+))
        (sty (mem +vram-addr+))
        (dotimes (i 17)
          (inc (mem +vram-io+)))
        (jmp (mem (label :loop))))
      (with-label crazy-stripes-nmi
        (ldx (imm 31))
        (as/until :zero (dex))
        (rti)))

  ;;;; -------------------------------------------------------

    (subprogram (sprite-zero "Visual sprite zero hit")
      (jsr (mem (label 'wait-for-vblank)))
      (poke 0 +ppu-cr2+)
      (ldx (imm 255))
      (jsr (mem (label 'fill-nametable-x)))
      (ppuaddr #x2088)
      (poke #xF6 +vram-io+)
      (ppuaddr #x3F11)
      (poke #x27 +vram-io+)
      (ppuaddr #x3F00)
      (poke #x10 +vram-io+)
      (ldx (imm 0))
      (poke 0 +spr-addr+)
      (as/until :zero
        (sta (mem +spr-io+))
        (inx))
      (poke 0 +spr-addr+)
      (sta (mem +spr-addr+))
      (poke 34 +spr-io+)                ; Sprite Y Coordinate
      (poke #xF4 +spr-io+)              ; Sprite Tile #
      (poke #b00000000 +spr-io+)        ; Sprite attributes
      (poke 59 +spr-io+)                ; Sprite X Coordinate

      (jsr (mem (label 'configure-ppu)))
      (with-label :runloop2
        (poke #b00011000 +ppu-cr2+)
        (ldx (imm #b11111000))
        (as/until :negative
          (lda (mem +ppu-status+))
          (asl))
        (stx (mem +ppu-cr2+))
        (poke #b00111000 +ppu-cr2+)
        ;;(poke #b00011000 +ppu-cr2+)

        ;; I don't have the first clue as to why nothing I do to detect
        ;; vblank works in Nestopia here. Fortunately, waiting for sprite 0
        ;; hit to clear does work, although it screws us out of our vblank time.
        (as/until :no-overflow (bita (mem +ppu-status+)))
        (jmp (mem (label :runloop2)))))

  ;;;; -------------------------------------------------------

    (subprogram (ram-test "RAM Test 1")
      (let* ((test-mode (zp 2))
             (offset    (zp 4))
             (fail      (zp 5))
             (message "Testing SRAM")
             (addr 6)                   ; Word 6: Current address
             (addr-lsb (zp addr))
             (addr-msb (zp (1+ addr))))

        (jsr (mem (label 'wait-for-vblank))) ; Program message screen
        (poke 0 (mem +ppu-cr2+))
        (ldx (imm 255))
        (jsr (mem (label 'fill-nametable-x)))
        (pokeword (label :ram-test-string -1) addr)
        (lda (mem +ppu-status+))
        (ppuaddr (- #x2070 (ceiling (length message) 2)))
        (ldy (imm (length message)))
        (as/until :zero
          (poke (indi addr) +vram-io+)
          (dey))
        (jsr (mem (label 'configure-ppu)))

        (poke #x80 #xA001)              ; Enable cart ram at #x6000
        (poke 0 offset)

        (with-label :testing
          (poke 0 fail)
          (poke 0 test-mode)
          (jsr (mem (label 'ram-test-sub)))
          (poke 1 test-mode)
          (jsr (mem (label 'ram-test-sub)))

          (jsr (mem (label 'wait-for-vblank)))
          (lda fail)                    ; Display pass/fail message
          (asif :zero (pokeword (label :pass -1) addr)
                :else (pokeword (label :fail -1) addr))
          (ppuaddr #x20CE)
          (ldy (imm 4))
          (as/until :zero
            (poke (indi addr) +vram-io+)
            (dey))
          (jsr (mem (label 'configure-ppu)))

          (inc offset)
          (jmp (mem (label :testing))))

        (set-label 'ram-test-sub)
        (ldy (imm 0))
        (pokeword #x6000 addr)
        (as/until :equal
          (lda addr-lsb)
          (eor addr-msb)
          (eor offset)
          (ldx test-mode)
          (asif :zero                   ; Write mode
            (sta (idxi addr))
            :else                       ; Compare/test mode
            (cmp (indi addr))
            (asif :not-equal            ; Compare failed
              (poke 1 fail)
              (poke #b00101000 +ppu-cr2+)
              :else
              (poke #b00001000 +ppu-cr2+)))
          (clc)                         ; Increment address.
          (lda addr-lsb)                ; Exit when we reach #x8000
          (adc (imm 1))                 ; Why'd I do this in 16-bit?
          (sta addr-lsb)                ; Wasteful.
          (lda addr-msb)
          (adc (imm 0))
          (sta addr-msb)
          (cmp (imm #x80)))
        (rts)
        (with-label :ram-test-string (emit (translate-string (reverse message))))
        (with-label :fail (emit (translate-string (reverse "FAIL"))))
        (with-label :pass (emit (translate-string (reverse "PASS"))))))

  ;;;; -------------------------------------------------------

    (subprogram (crazier-stripes "Hyperplaid Freestyle")
      (pokeword (label 'freestyle-nmi) *shadow-nmi*)
      (poke #x80 +ppu-cr1+)
      (poke 0 +ppu-cr2+)
      (poke 0 (zp 6))
      (lda (mem +ppu-status+))
      (poke #x3C (zp 4))
      (poke #x0C (zp 3))
      (jsr (mem (label 'freestyle-palette)))

      (with-label :freestyle
        (lda (mem +ppu-status+))
        (ldy (imm 0))
        (lda (imm #x3F))

        (ldx (imm #b00100000))
        (stx (mem +ppu-cr2+))
        (ldx (zp 4))
        (as/until :zero (dex))          ; Delay loop
        (ldx (zp 3))
        (as/until :zero
          (sta (mem +vram-addr+))
          (sty (mem +vram-addr+))
          (dotimes (i 16)
            (inc (mem +vram-io+)))
          (dotimes (i 1) (cmp (zp 42)) (nop))
          (dex))

        (stx (mem +ppu-cr2+))
        (sta (mem +vram-addr+))
        (stx (mem +vram-addr+))
                                        ;      (ldx (zp 5))
                                        ;      (stx (mem +vram-io+))
                                        ;      (inc (zp 5))
        (ldx (zp 4))
        (as/until :zero (dex))          ; Delay loop
        (ldx (zp 3))
        (as/until :zero
          (sta (mem +vram-addr+))
          (sty (mem +vram-addr+))
          (dotimes (i 16)
            (inc (mem +vram-io+)))
          (dotimes (i 1) (cmp (zp 42)))
          (dex))

        (jmp (mem (label :freestyle))))

      (with-label freestyle-palette
        (lda (mem +ppu-status+))
        (ppuaddr #x3F00)
        (ldx (imm 32))
        (lda (imm #x20))
        (as/until :zero
          (sta (mem +vram-io+))
          (clc)
          (adc (imm 5))
          (dex))
        (rts))

      (with-label freestyle-nmi
        (pha)
        (txa)
        (pha)
        (tya)
        (pha)

        (ldx (zp 6))          ; Don't reenter (for display subroutine)
        (asif :zero
          (poke 1 (zp 6))
          (jsr (mem (label 'process-joypad)))
          (lda *j1pressed*)
          ;; Proecss input
          (asif :not-zero
            (anda (imm 8))              ; Test UP arrow
            (asif :not-zero
              (inc (zp 3))
              :else
              (lda (imm 4))             ; Test DOWN arrow
              (bita *j1pressed*)
              (asif :not-zero
                (dec (zp 3))))
            (lda (imm 1))               ; Test RIGHT arrow
            (bita *j1pressed*)
            (asif :not-zero
              (inc (zp 4))
              :else
              (lda (imm 2))             ; Test LEFT arrow
              (bita *j1pressed*)
              (asif :not-zero
                (dec (zp 4))))
            (lda (imm 16))              ; Test START button
            (bita *j1pressed*)
            (asif :not-zero
              (jsr (mem (label 'freestyle-display)))
              (poke #x80 +ppu-cr1+)
              (poke 0 +ppu-cr2+)
              (jsr (mem (label 'freestyle-palette)))))
          (poke 0 (zp 6)))
        (pla)
        (tay)
        (pla)
        (tax)
        (pla)
        (rti))

      (with-label freestyle-display
        (ldx (imm 255))
        (jsr (mem (label 'fill-nametable-x)))

        (ppuaddr #x2088)                ; Display (zp 4)
        (poke (translate-char #\X) +vram-io+)
        (poke 255 +vram-io+)
        (lda (zp 4))
        (jsr (mem (label 'printhex)))

        (ppuaddr #x20A8)                ; Display (zp 3)
        (poke (translate-char #\Y) +vram-io+)
        (poke 255 +vram-io+)
        (lda (zp 3))
        (jsr (mem (label 'printhex)))

        (ppuaddr #x3F00)                ; Program palette
        (poke 16 +vram-io+)
        (ppuaddr #x3F0D)
        (poke #x1D +vram-io+)
        (jsr (mem (label 'configure-ppu)))

        (as/until :not-zero             ; Wait for start button again
          (jsr (mem (label 'process-joypad)))
          (lda (imm 16))
          (bita *j1pressed*))
        (rts)))

    (subprogram (sawtooth-440 "Sawtooth 440")
      ;; The next few hacks use timed sections, so align to a page here
      ;; and hopefully they all fit.
      (align 256 #xEA)
      (poke 0 +ppu-cr1+)                ; Disable NMI
      (poke 0 +ppu-cr2+)                ; Disable display
      (poke 0 +papu-control+)           ; Silence audio
      (ldy (imm 0))
      (timed-section ((round (/ +ntsc-clock-rate+ 440 128)) :loop t)
        (sty (mem +dmc-dac+))
        (iny)))

    (subprogram (sawtooth-220 "Sawtooth 220")
      (poke 0 +ppu-cr1+)                ; Disable NMI
      (poke 0 +ppu-cr2+)                ; Disable display
      (poke 0 +papu-control+)           ; Silence audio
      (ldy (imm 0))
      (timed-section ((round (/ +ntsc-clock-rate+ 220 128)) :loop t)
        (sty (mem +dmc-dac+))
        (iny)))

    (subprogram (square-ramp "Square Ramp 110")
      (poke 0 +ppu-cr1+)                ; Disable NMI
      (poke 0 +ppu-cr2+)                ; Disable display
      (poke 0 +papu-control+)           ; Silence audio
      (ldy (imm 0))
      (sty (zp 1))
      ;; Note that timed-section may kill the X register, so don't use it.
      (timed-section ((round (/ +ntsc-clock-rate+ 110 2)) :loop t)
        (iny)
        (tya)
        (anda (zp 1))
        (sta (mem +dmc-dac+))
        (lda (zp 1))
        (eor (imm #xFF))
        (sta (zp 1))))

    (subprogram (pulse-test "Pulse Test")
      (poke 0 +ppu-cr1+)                ; Disable NMI
      (poke 0 +ppu-cr2+)                ; Disable display
      (poke 0 +papu-control+)           ; Silence audio
      (ldy (imm 0))                     ; Amplitude counter
      (timed-section ((round (/ +ntsc-clock-rate+ 60 2)) :loop t)
        (lda (imm 0))
        (sty (mem +dmc-dac+))
        (sta (mem +dmc-dac+))
        (iny)))

    (subprogram (pulse-test "Longer Pulse Test")
      (poke 0 +ppu-cr1+)                ; Disable NMI
      (poke 0 +ppu-cr2+)                ; Disable display
      (poke 0 +papu-control+)           ; Silence audio
      (ldy (imm 0))                     ; Amplitude counter
      (timed-section ((round (/ +ntsc-clock-rate+ 60 2)) :loop t)
        (lda (imm 0))
        (sty (mem +dmc-dac+))           ; Rising edge
        ;; At 96k, one sample is ~18.6 CPU cycles long. I want to wait for roughly two samples.
        (loop repeat 6 do (inc (mem 0))) ; 4 + 6*6 = 40 cycles
        (sta (mem +dmc-dac+))           ; Falling edge
        (iny)))

    (subprogram (dac-test "DAC Tester")
      (let ((dac-level (zp 1))
            (mask (zp 2))
            (mask-cycle (zp 3))
            (title "DAC Output Level Test"))

        (poke 0 dac-level)
        (poke 127 mask)
        (poke 0 mask-cycle)
        (poke #x80 +ppu-cr1+)
        (jsr 'disable-screen)
        (lda (mem +ppu-status+))
        (jsr 'clear-screen)
        (lda (mem +ppu-status+))
        (ppuxy (- 16 (floor (length title) 2)) 3)
        (vram-string title)
        (ppuxy 9 5)
        (vram-string "Output level")
        (ppuxy 17 6)
        (vram-string "Mask")
        (ppuxy 3 8)
        (vram-string "Arrows adjust level")
        (ppuxy 3 9)
        (vram-string "Select toggles pulse mode")
        (ppuxy 3 10)
        (vram-string "Button A toggles output")
        (jsr 'enable-screen)
        (pokeword (label 'dac-test-nmi) *shadow-nmi*)
        (jmp (mem *origin*))

        (with-label dac-test-nmi
          (jsr 'dac-test-output)        ; Program DAC and update mask
          (lda (mem +ppu-status+))      ; Display output level and mask
          (ppuxy 22 5)
          (lda dac-level)
          (anda (imm 127))
          (jsr 'printhex)
          (ppuxy 22 6)
          (lda mask)
          (jsr 'printhex)
          (ppuxy 25 6)
          (lda mask-cycle)
          (jsr 'printhex)

          (jsr 'process-joypad)         ; Poll controls
          (lda (imm 1))                 ; LEFT: Increment DAC level by 1.
          (bita *j1pressed*)
          (asif :not-zero (inc dac-level))
          (asl)                         ; RIGHT: Decremement DAC level by 1.
          (bita *j1pressed*)
          (asif :not-zero (dec dac-level))
          (lda (imm 4))                 ; DOWN: Decrement DAC level by 16.
          (anda *j1pressed*)
          (asif :not-zero
            (lda dac-level)
            (clc)
            (adc (imm -16))
            (sta dac-level))
          (lda (imm 8))                 ; UP: Increment DAC level by 16.
          (anda *j1pressed*)
          (asif :not-zero
            (lda dac-level)
            (clc)
            (adc (imm 16))
            (sta dac-level))
          (lda (imm #x20))              ; SELECT: Toggle pulse mode
          (anda *j1pressed*)
          (asif :not-zero
            (lda (imm 127))
            (eor mask-cycle)
            (sta mask-cycle))
          (lda (imm #x80))              ; Button A: Toggle channel silence
          (anda *j1pressed*)
          (asif :not-zero
            (lda (imm 127))
            (eor mask)
            (sta mask))
          (jsr 'configure-ppu)
          (jsr 'dac-test-output)
          (rti))

        (with-label dac-test-output
          (lda dac-level)               ; Program DAC level
          (anda mask)
          (sta (mem +dmc-dac+))
          (lda mask)                    ; Cycle mask
          (eor mask-cycle)
          (sta mask)
          (rts))))

    #+NIL
    (subprogram (arp-1 "Arpeggio")
      (poke #x80 +ppu-cr1+)
      (jsr 'wait-for-vblank)
      (poke 0 +ppu-cr2+)
      (ppuaddr #x3F00)                  ; Yellow background
      (poke #x38 +vram-io+)
      (ppuaddr #x3F00)
      (poke 0 #x4017)                   ; Mode 4, no IRQ
      (poke #b00000011 #x4015)          ; Enable square channels

      (poke 0 (zp 1))                   ; ZP 1 = control phase
      (ldy (imm 0))

      (labels ((freq->period (freq) (min #x7FF (round (/ 1789772.5 freq 8))))
               (pitch (n) (* 261.3 (expt 2 (/ n 12))))
               (prgsq (freq &key (length #x1F) (channel 0))
                 (let ((period (freq->period freq)))
                   (poke (ldb (byte 8 0) period)
                         (+ (* 4 channel) +pulse1-fine+))
                   (poke (logior (ash length 3) (ldb (byte 3 8) period))
                         (+ (* 4 channel) +pulse1-coarse+)))))
        (with-label :loop
          (loop for chord in '(0 3 7 10)
                with root = 12
                as note = (+ chord root)
                with length = 4
                with detune = nil       ; 5/1200
                do
                (prgsq (pitch note))
                (when detune (prgsq (pitch (+ note detune)) :channel 1))
                ;;(poke #b10011111 +pulse1-control+)
                (ldy (zp 1))
                (iny)
                (sty (zp 1))
                (tya)
                (clc)
                (lsr)
                (lsr)
                (anda (imm 3))
                (tax)
                (lda (abx (label 'arp-1-control-seq)))
                (brk) (db 9)
                (sta (mem +pulse1-control+))
                (when detune (sta (mem +pulse2-control+)))

                (poke #b01000000 +pulse1-ramp+)
                (poke #b01000000 +pulse2-ramp+)
                (ldx (imm length))
                (as/until :zero
                  (jsr 'wait-for-vblank)
                  (dex)))
          (jmp (mem :loop)))

        (with-label arp-1-control-seq (db #b01011111 #b10011111 #b01011111 #b00011111))))

#| This is how to add another page to the menu:

    (subprogram (page2 "Next Page")
      (pokeword (label 'new-menu) 9)
      (jmp (mem 'reset-menu-reentry)))

    (dw #xFFFF)
    (set-label 'new-menu)
    (subprogram (woots "Item 1") (jmp (mem 'crazier-stripes)))
    (subprogram (p2b "Item 2"))
    (subprogram (p2c "Item 3"))
|#

;;;; -------------------------------------------------------

    (format t "~&Total programs size: ~:D bytes.~%" (- *origin* #xC000))

    ;; This #xFFFF word marks the end of the menu.
;    (dw #xFFFF)

;;;; -------------------------------------------------------
;;;; Library routines

    (advance-to #xF000)

;;; When developing hacks, this is used bypass the menu. Always
;;; restore it to #xFFFF before programming the cart.
    (set-label 'menu-bypass)
    ;; Address to jump to, instead of menu:
    (dw #xFFFF)                         ; (dw (label 'dac-test))

    (with-label printhex
      "Print accumulator in hex to VRAM port"
      (pha)
      (dotimes (i 4) (lsr))
      (ora (imm #xD0))
      (sta (mem +vram-io+))
      (pla)
      (anda (imm 15))
      (ora (imm #xD0))
      (sta (mem +vram-io+))
      (rts))

;;; Strobe and read joypad 1 bits into *jtmp*
    (set-label 'read-joypad-1)
    (poke 1 +joypad-1+)
    (poke 0 +joypad-1+)
    (loop repeat 8 do (lda (mem +joypad-1+)) (lsr) (rol *jtmp*))
    (rts)

;;; Read and compute changed joypad bits, updating jtmp, lastj, j1pressed, j1released.
    (set-label 'process-joypad)
    (jsr (mem (label 'read-joypad-1)))
    (lda *jtmp*)
    (eor *lastj*)                       ; Changed buttons
    (sta (zp 0))
    (anda *lastj*)
    (sta *j1released*)
    ;;(asif :not-zero (brk) (db 64))
    (lda *lastj*)
    (eor (imm 255))
    (anda (zp 0))
    (sta *j1pressed*)
    ;;(asif :not-zero (brk) (db 65))
    (poke *jtmp* *lastj*)
    (rts)

;;; Simple VBI notifier
    (set-label 'vblank-ticker)
    (pha)
    (poke 0 *ticker*)
    (pla)
    (rti)

;;; Wait for vertical blank
    (set-label 'wait-for-vblank)
    (poke 1 *ticker*)
    (as/until :zero (lda *ticker*))
    (rts)

;;; Real interrupt handlers
    (set-label 'real-irq)
    (jmp (indirect *shadow-irq*))
    (set-label 'real-nmi)
    (jmp (indirect *shadow-nmi*))

    ;; Enable VBI and let PPU warm up
    (set-label 'ppu-init)
    (poke #b10000000 +ppu-cr1+)
    (poke #b00000000 +ppu-cr2+)
    (jsr (mem (label 'wait-for-vblank)))
    (jsr (mem (label 'wait-for-vblank)))
    (poke #b00000000 +ppu-cr1+)
    (rts)

    ;; Make a beep
    (with-label ding
      (poke #b00000011 +papu-control+)
      (poke 0 +pulse1-ramp+)
      (poke 00 +pulse1-fine+)
      (poke #xf1 +pulse1-coarse+)
      (poke #b10000010 +pulse1-control+)
      (poke 0 +pulse2-ramp+)
      (poke 50 +pulse2-fine+)
      (poke #xf1 +pulse2-coarse+)
      (poke #b10000010 +pulse2-control+)
      (rts))

    ;; Fill name and attribute tables with value in X
    (set-label 'fill-nametable-x)
    (ppuaddr #x2000)
    (txa)
    (ldy (imm 8))
    (set-label :outer)
    (ldx (imm 0))
    (set-label :loop)
    (sta (mem +vram-io+))
    (inx)
    (bne (rel :loop))
    (dey)
    (bne (rel :outer))
    (rts)

    (with-label disable-screen
      (jsr 'wait-for-vblank)
      (poke #x80 +ppu-cr1+)
      (poke #x00 +ppu-cr2+)
      (rts))

    (with-label enable-screen
      (jsr 'wait-for-vblank)
      (lda (mem +ppu-status+))
      (poke 0 +vram-scroll+)
      (sta (mem +vram-scroll+))
      (ppuaddr #x2000)
      (poke #x08 +ppu-cr2+)
      (rts))

    (with-label clear-screen
      (ldx (imm #xFF))
      (jsr 'fill-nametable-x)
      (rts))

   ;;; --------------------------------------------------------------------
   ;;; Menu Program

   ;;; Menu variables in the zero page:
   ;;;    0: VBlank tick variable
   ;;;  1/2: Pointer to current program in list
   ;;;  3/4: VRAM pointer for next line
   ;;;  5/6: Pointer to next program in list
   ;;;    7: Vertical position
   ;;; 9/10: Pointer to first menu item
    (advance-to #xFE00)

    (with-label :load-56
      (ldy (imm 0))                  ; Load Word 5: Next program pointer
      (lda (indi 1))
      (sta (zp 5))
      (iny)
      (lda (indi 1))
      (sta (zp 6))
      (rts))

    (set-label 'reset-menu)
    (sei)
    (cld)
    (ldx (imm #xFF))                    ; Set stack pointer
    (txs)
    (poke (lsb (label 'vblank-ticker)) *shadow-nmi*)
    (poke (msb (label 'vblank-ticker)) (1+ *shadow-nmi*))
    (poke (lsb (label 'ignore-irq)) *shadow-irq*)
    (poke (msb (label 'ignore-irq)) (1+ *shadow-irq*))
    (pokeword #xC000 9)                 ; First program address

    ;; Alternate entry point for setting up submenus
    (set-label 'reset-menu-reentry)
    (poke 0 +ppu-cr1+)
    (poke 6 #x8000)                     ; MMC3: Select PRG banks
    (poke 0 #x8001)
    (poke 7 #x8000)
    (poke 1 #x8001)
    (poke #b00000011 +papu-control+)
    (poke #b10100000 +pulse1-control+)
    (poke 0          +pulse1-ramp+)
    (poke #x00       +pulse1-fine+)
    (poke #xf0       +pulse1-coarse+)

    (set-label :loop)
    (lda (mem +ppu-status+))
    (and (imm #x80))
    (beq (rel :loop))
    (set-label :loop)
    (lda (mem +ppu-status+))
    (bpl (rel :loop))

    (poke 0 #x8000)                     ; MMC3: Select CHR banks
    (poke #x28 #x8001)
    (poke 1 #x8000)
    (poke #x2A #x8001)

    (poke 0 *lastj*)                  ; Init *lastj* for joystick code

    (jsr (mem (label 'ppu-init)))
    (ldx (imm #xFF))
    (jsr (mem (label 'fill-nametable-x)))
    (lda (mem +ppu-status+))
    (ppuaddr #x3F00)
    (poke #x20 +vram-io+)
    (ppuaddr #x3F0D)
    (poke #x05 +vram-io+)
    (poke #x15 +vram-io+)
    (poke #x25 +vram-io+)

    (ldx (mem 'menu-bypass))          ; Check if menu-bypass != #xFFFF
    (inx)
    (asif :zero
      (ldx (mem (label 'menu-bypass 1)))
      (inx)
      (asif :not-zero (jmp (indirect 'menu-bypass)))
      :else
      (jmp (indirect 'menu-bypass)))

;;; Display the program list
    ;;(pokeword #xC000 1)                 ; Word 1: Program pointer
    (poke (zp 9) (zp 1))
    (poke (zp 10) (zp 2))

    (pokeword #x2044 3)                 ; Word 3: VRAM address

    (set-label 'run-menu)

    (set-label :display-menu-loop)
    (lda (mem +ppu-status+))
    (jsr (mem (label :load-56)))
    (tax)
    (inx)
    (asif :not-zero                     ; Ensure next pointer != $FFFF
      (ldx (zp 5))
      (inx)
      (asif :not-zero
        ;; Display string
        (poke (zp 4) (mem +vram-addr+))
        (poke (zp 3) (mem +vram-addr+))
        (ldy (imm 2))
        (set-label :writing-name)
        (lda (indi 1))
        (asif :not-zero
          (sta (mem +vram-io+))
          (iny)
          (jmp (mem (label :writing-name))))
        (iny)    ; Skip null byte - ($1), y now points to program code
        ;; Increment VRAM address
        (lda (zp 3))
        (clc)
        (adc (imm 32))
        (sta (zp 3))
        (lda (zp 4))
        (adc (imm 0))
        (sta (zp 4))

        ;; Advance to next pointer
        (poke (zp 5) (zp 1))
        (poke (zp 6) (zp 2))
        (jmp (mem (label :display-menu-loop)))))

    ;; Initialize menu

    (set-label :menu-top)
    (poke 0 (zp 7))                    ; Byte 7: Current program index
    ;;(pokeword #xC000 1)                 ; Word 1: Program pointer
    (poke (zp 9) (zp 1))                ; Word 1: Program pointer
    (poke (zp 10) (zp 2))               ; (from first program pointer in word 9)
    (pokeword #x2044 3)                 ; Word 3: VRAM address
    (jsr (mem (label :load-56)))

    (set-label :menu-run-loop)
    (jsr (mem (label 'configure-ppu)))
    (jsr (mem (label 'wait-for-vblank)))

    ;; Process input - menu is controlled by select and start buttons
    (jsr (mem (label 'process-joypad)))
    (lda (imm #x20))
    (bita *j1pressed*)
    (asif :not-zero                     ; - SELECT button pressed -
      (jsr (mem (label 'ding)))
      (inc (zp 7))                      ; Increment vertical index
      (poke (zp 5) (zp 1))              ; Transfer next to current
      (poke (zp 6) (zp 2))
      (jsr (mem (label :load-56)))      ; Load new next pointer
      (ldx (zp 5))                      ; Test next pointer == FFFF
      (inx)
      (asif :zero
        (ldx (zp 6))
        (inx)
        (asif :zero
          (jmp (mem (label :menu-top))))))

    (lda (imm #x10))
    (bita *j1released*)
    (asif :not-zero                     ; - START button released -
      (ldy (imm 1))
      (set-label :scanloop)             ; Scan past program name
      (iny)
      (lda (indi 1))
      (bne (rel :scanloop))
      (iny)
      (tya)
      (clc)
      (adc (zp 1))                     ; Increment pointer accordingly
      (sta (zp 1))
      (lda (zp 2))
      (adc (imm 0))
      (sta (zp 2))
      (jmp (indirect 1)))

    ;; Draw arrow
    (poke #b10000100 +ppu-cr1+)         ; Vertical write mode
    (ppuaddr #x2042)
    (ldy (imm 0))
    (set-label :loop)
    (cpy (zp 7))
    (asif :equal
      (lda (imm (translate-char #\-)))
      :else
      (lda (imm (translate-char #\Space))))
    (sta (mem +vram-io+))
    (iny)
    (cpy (imm 25))
    (bne (rel :loop))

    (jmp (mem (label :menu-run-loop)))

;;; Interrupt Vectors
    (advance-to +nmi-vector+)
    (dw (label 'real-nmi))
    (dw (label 'reset-menu))
    (dw (label 'real-irq))))
