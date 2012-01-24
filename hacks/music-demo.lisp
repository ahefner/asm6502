;;;; Music demo: Looping audio through the 7-bit DAC.

;;;; This program will treat the listener to a seamlessly looping
;;;; excerpt from one of western music's great achievements,
;;;; "Can You Feel It" by Mr. Fingers.

;;;; It plays an 8 second (240KB) musical loop at 30.3 KHz.  This rate
;;;; was selected to fit the music loop into exactly the space
;;;; available. The sound quality is surprisingly good (though less so
;;;; on emulators other than my own, because of different DAC curves).

(defpackage :music-demo
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :music-demo)

(defvar *path* #.*compile-file-truename*)

;; Load the raw 8-bit PCM data.

;; PROCESS-DAC-WAVEFORM corrects for my claimed DAC non-linearity,
;; and add some bogus dithering. (I can't hear a real difference no
;; matter how I tweak the dither settings).

(defun load-audio ()
  ;(loadbin "/home/hefner/cl/asm6502/fingers_30299.pcm")
  (process-dac-waveform
   (loadbin (merge-pathnames "fingers_30299.pcm" *path*))
   :prescale 0.5
   :white-noise-bits 0.4
   :error-feedback 1.0))

(macrolet
    ((program ((filename &rest rom-args) &body body)
       `(let ((program
               (concatenate
                'vector
                ;; Audio banks:
                (load-audio)
                ;; Program bank:
                (let ((*context* (make-instance
                                  'basic-context
                                  :address #xC000)))
                  ,@body
                  (link *context*)))))
          (dumpbin "/tmp/prg.bin" program)
          (write-ines ,filename program ,@rom-args))))
 (program ("/tmp/music-demo.nes" :mapper 4)
  (advance-to #xE000)                   ; Put everything in the last bank.
  (with-label nmi (rti))
  (with-label irq (rti))
  (with-label reset
    ;; Initialize
    (sei)
    (cld)
    (ldx (imm #xFF))
    (txs)
    (poke 0 +ppu-cr1+)                  ; Disable NMI
    (poke 0 +ppu-cr2+)                  ; Disable display
    (poke 0 +papu-control+)             ; Silence audio
    (ppuaddr #x3F00)
    (poke #x17 +vram-io+)
    (ppuaddr #x3F00)

    (poke 6 #x8000)                     ; Select first bank
    (poke 0 #x8001)

    (ldy (imm 0))
    (poke 0 (zp 3))                     ; LSB of playback page (always zero)
    (poke #x80 (zp 4))                  ; MSB of playback pointer
    (poke 0 (zp 5))                     ; Current bank

    (align 256 #xEA)

    (with-label :loop
      (let* ((inner-cycles
              (counting-cycles
                (lda (indi 3))          ; Load audio byte
                (sta (mem +dmc-dac+))   ; Write to DAC register
                (tya)                   ; Increment audio pointer...
                (clc)
                (adc (imm 1))
                (tay)
                (lda (zp 4))            ; Carry into high address byte
                (adc (imm 0))
                (tax)
                (lda (abx (label :msbtable (- #x80))))
                (lsr)                   ; Low bit is carry into bank number
                (ora (imm #x80))
                (sta (zp 4))

                ;; I think I can get away without resetting #x8000
                ;; inside the loop, and I need the cycles.
                ;; This should be tested on real hardware.
                ;;(lda (imm 6))           ; Select low bank
                ;;(sta (mem #x8000))

                (lda (zp 5))            ; Carry into bank number
                (adc (imm 0))
                (tax)
                (lda (abx :banktable))
                (sta (mem #x8001))      ; Select bank and write back
                (sta (zp 5))

                ;; Add 3 extra cycles for the jump at the bottom of the loop:
                (context-note-cycles *context* 3)))
             (clock 1789772.5)
             (freq 30299)
             (loop-cycles (round clock freq))
             (delay-cycles (- loop-cycles inner-cycles)))

        (unless (<= inner-cycles loop-cycles)
          (cerror "Screw it."
                  "Loop is too slow! ~A cycles, need ~A"
                  inner-cycles loop-cycles))
        (format t "~&Inner cycles: ~D
Target frequency: ~D
Target cycles: ~D
Need to delay for ~D cycles
Actual frequency: ~D"
                inner-cycles freq loop-cycles delay-cycles
                (/ clock loop-cycles))

        (emit-delay (max 0 delay-cycles))
        (jmp (mem :loop)))))

  ;; This table controls the wrapping from #x9F00 to #x8000 page, and increment of the bank number
  (with-label :msbtable
    (loop for i from 0 upto 32
          do (db (logior (ash (mod i 32) 1) (if (= i 32) 1 0)))))

  ;; This table controls wrapping from bank 14 to bank 0
  (with-label :banktable
    (loop for bank from 0 upto 30 do (db (mod bank 30))))

  (advance-to +nmi-vector+)
  (dw (label 'nmi))
  (dw (label 'reset))
  (dw (label 'irq))))

