(in-package :asm6502-utility)

(defconstant +nmi-vector+   #xFFFA)
(defconstant +reset-vector+ #xFFFC)
(defconstant +irq-vector+   #xFFFE)

;;;; Small utilities

(defun poke (value address)
  (when (typep value '(or integer promise))
    (setf value (imm value)))
  (lda value)
  (sta (typecase address
         ((integer 0 255) (zp address))
         ((or integer promise) (mem address))
         (t address))))

(defun pokeword (value address)
  (poke (lsb value) address)
  (poke (msb value) (1+ address)))

;;;; Control structures

;;; Assemble an if-then-else construct. The 'branch-compiler' is invoked
;;; to generate conditional branch to the else clause. If the 'else-compiler'
;;; is omitted, the jump following the "then" clause will be optimized away.

(defun assemble-if (branch-compiler then-compiler &optional else-compiler)
  (let ((else-sym    (gensym "ELSE"))
        (finally-sym (gensym "FINALLY")))
    (funcall branch-compiler (rel else-sym))
    (funcall then-compiler)
    (when else-compiler (jmp (mem (label finally-sym))))
    (set-label else-sym)
    (when else-compiler (funcall else-compiler))
    (set-label finally-sym)))

(defgeneric condition-to-branch (condition)
  (:documentation "Produce a closure capable of generating a branch to
 the given argument if the condition is not true." ))

(defmethod condition-to-branch ((condition (eql :positive)))    'bmi)
(defmethod condition-to-branch ((condition (eql :negative)))    'bpl)
(defmethod condition-to-branch ((condition (eql :carry)))       'bcc)
(defmethod condition-to-branch ((condition (eql :no-carry)))    'bcs)
(defmethod condition-to-branch ((condition (eql :zero)))        'bne)
(defmethod condition-to-branch ((condition (eql :not-zero)))    'beq)
(defmethod condition-to-branch ((condition (eql :equal)))       'bne)
(defmethod condition-to-branch ((condition (eql :not-equal)))   'beq)
(defmethod condition-to-branch ((condition (eql :overflow)))    'bvc)
(defmethod condition-to-branch ((condition (eql :no-overflow))) 'bvs)

(defmacro asif (condition &body statements)
  (let ((then statements)
        (else nil)
        (part (position :else statements)))
    (when part
      (setf then (subseq statements 0 part))
      (setf else (subseq statements (1+ part) nil)))
    `(assemble-if
      ',(condition-to-branch condition)
      (lambda () ,@then)
      ,(and else `(lambda () ,@else)))))

(defmacro with-label (label &body body)
  `(progn (set-label ',label) ,@body))

(defmacro as/until (condition &body body)
  (let ((sym (gensym)))
    `(with-label ,sym
       ,@body
       (funcall (condition-to-branch ',condition) (rel ',sym)))))

;;; Delays and timed sections

(defun emit-delay (delay-cycles)
  "Emit a delay of the specified number of CPU cycles. Kills the X register."
  (loop while (>= delay-cycles 11)
        as iterations = (min 256 (floor (- delay-cycles 5) 5))
        as n = (mod iterations 256) do
        #+NIL
        (format t "~&Inserting delay loop (~A cycles left), ~A iterations (should burn ~A cycles)~%"
                delay-cycles iterations (1+ (* 5 iterations)))
        (decf delay-cycles)
        (ldx (imm n))
        (unless (<= (lsb *origin*) 253) ; I could work around this..
          (error "Can't assemble a timed loop on a page crossing. Sorry."))
        (as/until :zero (dex))
        (decf delay-cycles (* 5 iterations)))
  (when (= 1 delay-cycles)
    (error "Not possible to delay for 1 cycle."))
  (when (oddp delay-cycles)
    ;;(format t "~&~A cycles to burn -- Inserting LDY instruction.~%" delay-cycles)
    (ldx (imm 0))
    (decf delay-cycles 3))
  (loop while (>= delay-cycles 6) do
        (ldx (imm 0))
        (ldx (imm 0))
        (decf delay-cycles 6))
  (unless (zerop delay-cycles)
    ;;(format t "~&~A cycles to burn -- Inserting ~A NOPs~%" delay-cycles (/ delay-cycles 2))
    (dotimes (i (/ delay-cycles 2)) (nop) (decf delay-cycles 2)))
  (assert (zerop delay-cycles)))

(defmacro timed-section ((cycle-count &key loop) &body body)
  `(let ((timed-section-head (set-label (gensym)))
         (cycles (counting-cycles ,@body))
         (cycle-count ,cycle-count)
         (loop-p ,loop))
     (when loop-p (decf cycle-count 3))
     (unless (> cycle-count 0) (error "Cycle count for timed section is too small."))

     (unless (>= ,cycle-count cycles)
       (error "Timed section takes ~D cycles, which is longer than ~D cycles."
              cycles ,cycle-count))
     (emit-delay (- cycle-count cycles))
     (when loop-p (jmp (mem timed-section-head)))
     (values)))
