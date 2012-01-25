;;;; Cycle-counting mode for 6502 assembler

(in-package :asm6502)

(defparameter *cycle-count*
  #(7 6 NIL NIL NIL 3 5 NIL 3 2 2 NIL NIL 4 6 NIL 4 5 NIL NIL NIL 4 6 NIL 2
    4 NIL NIL NIL NIL 7 NIL 6 6 NIL NIL 3 3 5 NIL 4 2 2 NIL 4 4 6 NIL 2 5
    NIL NIL NIL 4 6 NIL 2 4 NIL NIL NIL 4 7 NIL 4 6 NIL NIL NIL 3 5 NIL 3 2
    2 NIL 3 6 6 NIL 2 5 NIL NIL NIL 4 6 NIL 2 4 NIL NIL NIL 4 7 NIL 6 6 NIL
    NIL NIL 3 5 NIL 4 2 2 NIL 5 NIL 6 NIL 2 5 NIL NIL NIL 4 6 NIL 2 4 NIL
    NIL NIL 4 7 NIL NIL 6 NIL NIL 3 3 3 NIL 2 NIL 2 NIL 4 4 4 NIL 2 6 NIL
    NIL 4 4 4 NIL 2 5 2 NIL NIL 5 NIL NIL 2 6 2 NIL 3 3 3 NIL 2 2 2 NIL 4 4
    4 NIL 2 5 NIL NIL 4 4 4 NIL 2 4 2 NIL 4 4 4 NIL 2 6 NIL NIL 3 3 5 NIL 2
    2 2 NIL 4 4 6 NIL 2 5 NIL NIL NIL 4 6 NIL 2 4 NIL NIL NIL 4 7 NIL 2 6
    NIL NIL 3 3 5 NIL 2 2 2 NIL 4 4 6 NIL 2 5 NIL NIL NIL 4 6 NIL 2 4 NIL
    NIL NIL 4 7 NIL))

(defparameter *variable-timing*
  #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL
    NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL NIL NIL
    NIL NIL NIL T NIL NIL NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL T T NIL NIL NIL NIL NIL NIL NIL T NIL NIL
    NIL T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL T T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL T NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T T NIL NIL NIL NIL NIL NIL NIL
    T NIL NIL T T T NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL T T NIL NIL NIL NIL NIL NIL NIL T NIL NIL NIL T NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T NIL NIL
    NIL NIL NIL NIL NIL NIL T NIL NIL NIL T NIL NIL) )

(defun opcode-cycles (opcode)
  "Determine the number of cycles required to execute an
opcode. Returns two values: the number of cycles, and a boolean value
which is T if the opcode may take a variable number of cycles to
execute. The cycles table is not complete; if the number of cycles is
unknown, the first value is NIL and the second T."
  (if (aref *cycle-count* opcode)
      (values (aref *cycle-count* opcode)
              (aref *variable-timing* opcode))
      (values nil t)))

;;; You'd think this would be sufficient to let you count cycles even
;;; in nested non-cycle-counting context, but sorry, no.
(defgeneric context-note-cycles (context num-cycles)
  (:method (c n) (declare (ignore c n)))
  (:method ((context delegate-context) num-cycles)
    (context-note-cycles (context-parent context) num-cycles)))

(defclass cycle-counting-context (delegate-code-vector
                                  delegate-symbol-lookup)
  ((cycle-count :initform 0 :accessor cycle-count :initarg :cycle-count)
   (precise-p   :initform t   :accessor precise-p   :initarg :precise-p)))

(defmethod context-note-cycles ((context cycle-counting-context) num-cycles)
  (incf (cycle-count context) num-cycles))

(defmethod context-emit-instruction ((context cycle-counting-context) vector)
  (multiple-value-bind (cycles variable) (opcode-cycles (aref vector 0))
    (when variable (setf (precise-p context) nil))
    (if cycles
        (context-note-cycles context cycles)
        (warn "Don't know number of cycles for opcode ~X" (aref vector 0)))
    (call-next-method)))

(defmacro counting-cycles (&body body)
  `(let ((*context* (make-instance 'cycle-counting-context :parent *context*)))
     ,@body
     (cycle-count *context*)))

