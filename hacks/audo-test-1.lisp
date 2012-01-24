;;;; NES audio test: Output a saw waveform through the DAC from a
;;;; timed loop.

(defpackage :audio-test-1
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :audio-test-1)

(defvar *path* #.*compile-file-truename*)

(defmacro program ((filename &rest rom-args)
                   &body body)
  `(let ((program
             (let ((*context* (make-instance 'basic-context :address #xC000)))
               ,@body
               (link *context*))))
     (dumpbin "/tmp/prg.bin" program)
     (write-ines ,filename program ,@rom-args)))

(program ("/tmp/audio-test-1.nes")
  (advance-to #xE000)                   ; Put everything in the last bank.
  (with-label nmi (rti))
  (with-label irq (rti))

  (with-label reset
    ;; Initialize registers.
    (poke 0 +ppu-cr1+)                  ; Disable NMI
    (poke 0 +ppu-cr2+)                  ; Disable display
    (poke 0 +papu-control+)             ; Silence audio

    (lda (imm 0))

    (with-label :saw
      (let* ((inner-cycles
              (counting-cycles
                ;; Square wave:
                ;;(sta (mem +dmc-dac+))
                ;;(eor (imm 32))
                ;; Saw wave:
                (sty (mem +dmc-dac+))
                (iny)
                ;; Add 3 extra cycles for the jump at the bottom of the loop:
                (context-note-cycles *context* 3)
                #+NIL (jmp (mem :saw))))
             (clock +ntsc-clock-rate+)
             (freq 440.0)
             (wave-detail 128)
             (loop-cycles (round clock (* wave-detail freq)))
             (delay-cycles (- loop-cycles inner-cycles)))

        (format t "~&Target frequency: ~A~%Target cycles: ~A~%" freq loop-cycles)
        (format t "~&Need to delay for ~A cycles~%Actual frequency: ~A"
                delay-cycles (/ clock loop-cycles wave-detail))

        (emit-delay delay-cycles)
        (jmp (mem :saw))))

    ;; Halt in an infinite loop.
    (jmp (mem *origin*)))

  ;; Interrupt vectors
  (advance-to +nmi-vector+)
  (dw (label 'nmi))
  (dw (label 'reset))
  (dw (label 'irq)))
