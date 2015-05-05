(defpackage :nsf-test-1
  (:use :common-lisp :6502 :6502-modes :asm6502 :asm6502-utility :asm6502-nes))

(in-package :nsf-test-1)

(let ((*context* (make-instance 'basic-context)))
  (emit-nsf-header 1 #x8000 :init :play)
  (format t "~&Header is ~A bytes.~%" (length (context-code-vector *context*)))
  (setf *origin* #x8000)

  (procedure init
    (cld)
    (rts))

  (procedure play
    (cld)
    (rts))

  (setf (binary-file "/tmp/test1.nsf") (link *context*)))

