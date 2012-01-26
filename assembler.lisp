(in-package :asm6502)

;;;; Delayed evaluation

(defvar *lazy-marker* '#:postponed)
(defstruct promise name fun (value *lazy-marker*))

(define-condition resolvable-condition ()
  ((path :initform nil :initarg :path :accessor path))
  (:report (lambda (condition stream)
             (format stream "~A" (path condition)))))

(defgeneric force (expression &optional force-p)
  (:documentation "Forces computing the value of a delayed expression"))

(defmethod force ((expression number) &optional force-p)
  (declare (ignore force-p))
  expression)

(defmethod force ((p promise) &optional (error-p t))
  (if (not (eq (promise-value p) *lazy-marker*))
      (promise-value p)
      (handler-case (setf (promise-value p) (funcall (promise-fun p)))
        (resolvable-condition (condition)
          (setf (path condition) (cons (promise-name p) (path condition)))
          (funcall (if error-p #'error #'signal) condition)
          p))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun parse-binding (spec)
   (etypecase spec
     (symbol (list spec spec))
     ((cons symbol (cons t null)) spec))))

(defmacro forcing (dependencies &body body)
  (let ((bindings (mapcar #'parse-binding dependencies)))
    `((lambda ,(mapcar #'first bindings) ,@body)
      ,@(loop for b in bindings
              collect `(force ,(second b))))))

(defmacro delay (name dependencies &body body)
  `(force
    (make-promise :name ,name :fun (lambda () (forcing ,dependencies ,@body)))
    nil))

;;;; Bits and bytes

(defgeneric msb (x)
  (:method ((x integer)) (ldb (byte 8 8) x))
  (:method ((value promise))
    (delay :MSB (value) (msb value))))

(defgeneric lsb (x)
  (:method ((x integer)) (ldb (byte 8 0) x))
  (:method ((value promise))
    (delay :LSB (value) (lsb value))))

(defun 8-bit-encodable (x)
  (etypecase x
    ((integer -128 255) x)))

(defun signed-octet (x)
  (etypecase x
    ((integer -128 127) x)))

(defun encode-byte (byte &optional (name "byte"))
  (vector (delay name (byte) (lsb (8-bit-encodable byte)))))

(defun encode-signed-byte (x &optional (name "signed-byte"))
  (vector (delay name (x) (lsb (signed-octet x)))))

(defun 16-bit-encodable (x)
  (etypecase x
    ((integer 0 65535) x)))

(defun encode-word (word &optional (name 'encode-word))
  (vector (delay name (word) (lsb word))
          (delay name (word) (msb word))))

(defun join-masks (x y)
  (unless (zerop (logand x y))
    (error "Bitmasks ~A and ~A overlap!" x y))
  (logior x y))

;;;; Files

(defun write-binary-file (filename vector &key
                          (if-exists :supersede)
                          (external-format :default)
                          (element-type '(unsigned-byte 8)))
  (with-open-file (out filename
                       :if-exists if-exists :element-type element-type
                       :external-format external-format :direction :output)
    (write-sequence vector out)))

(defun binary-file (filename &key (element-type '(unsigned-byte 8)))
  (with-open-file (in filename :element-type element-type)
    (let ((data (make-array (file-length in))))
      (read-sequence data in)
      data)))

(defsetf binary-file (filename &rest args) (sequence)
  `(write-binary-file ,filename ,sequence ,@args))

;;;; Assembly context protocol (symbol table, accumulated output)

(defgeneric context-emit (context vector)
  (:documentation "Emit a vector of bytes into the assembly context"))

(defgeneric context-address (context)
  (:documentation "Returns current virtual address of the context"))

(defgeneric (setf context-address) (address context)
  (:documentation "Set the current virtual address of the context"))

(defgeneric context-find-label (context symbol)
  (:documentation "Returns a promise for the address of a symbol, or nil."))

(defgeneric context-set-label (context symbol &optional address)
  (:documentation "Set the address of a label. If not supplied, the current address is used."))

(defgeneric context-emit-instruction (context vector)
  (:documentation "Emit an instruction into the assembly context. This
  exists to provide additional information to the context.")
  (:method (context vector) (context-emit context vector)))

(defgeneric link (context)
  (:documentation "Prepare and return final, assembled output.")
  (:method (context) (resolve-vector (context-code-vector context))))

(defvar *context* nil "Current assembly context")

;;; Basic implementation of assembly context

(defclass symbol-table ()
  ((symbol-table :initform (make-hash-table :test 'equal))))

(defmethod context-find-label ((context symbol-table) symbol)
  (with-slots (symbol-table) context
    (gethash symbol symbol-table)))

(defmethod context-set-label ((context symbol-table) symbol
                              &optional (address (context-address context)))
  (with-slots (symbol-table) context
    (setf (gethash symbol symbol-table) address)))

(defclass code-vector ()
  ((code-vector  :initarg :code-vector
                 :reader context-code-vector
                 :initform (make-array 0 :adjustable t :fill-pointer t))
   (address :initarg :address :accessor context-address :initform #x8000)))

(defmethod context-emit ((context code-vector) vector)
  (when (> (+ (context-address context) (length vector)) #x10000)
    (warn "Context emit of $~X bytes at ~X will overflow address space"
          (context-address context)
          (length vector)))
  (loop for x across vector do
        (unless (typep x '(or (integer 0 255) promise))
          (error "Attempt to emit garbage (~A) at ~X" x (context-address context)))
        (vector-push-extend x (context-code-vector context)))
  (incf (context-address context) (length vector)))

(defclass basic-context (code-vector symbol-table) ())

(defun resolve-vector (vector)
  (let (problems)
    (prog1
        (map 'vector (lambda (x)
                       (handler-case (force x t)
                         (resolvable-condition (c)
                           (push (path c) problems)
                           x)))
             vector)
      (when problems
        (error "Unable to resolve output due to the following:~%~A~%"
               problems)))))

;;; Note that context-code-vector isn't part of the context protocol,
;;; but defined on basic-contexts.

(defclass delegate-context ()
  ((parent :reader context-parent :initarg :parent)))

(defmethod context-address ((context delegate-context))
  (context-address (context-parent context)))

(defmethod (setf context-address) (address (context delegate-context))
  (setf (context-address (context-parent context)) address))

(defclass delegate-code-vector (delegate-context) ())

(defmethod context-emit ((context delegate-code-vector) vector)
  (context-emit (context-parent context) vector))

(defclass delegate-symbol-definition (delegate-context) ())
(defclass delegate-symbol-lookup     (delegate-context) ())

(defmethod context-find-label ((context delegate-symbol-lookup) symbol)
  (context-find-label (context-parent context) symbol))

(defmethod context-set-label ((context delegate-symbol-definition) symbol
                              &optional (address (context-address context)))
  (context-set-label (context-parent context) symbol address))

(defclass local-symbol-table (delegate-symbol-lookup symbol-table) ())

(defmethod context-find-label ((context local-symbol-table) symbol)
  (with-slots (symbol-table) context
    (gethash symbol symbol-table
             (context-find-label (context-parent context) symbol))))

;;; Local context, the base upon which to build local symbol scopes and
;;; special-purpose contexts.

(defclass local-context (delegate-code-vector local-symbol-table)
  ())

;;;; Helpers

(defun emit (bytes) (context-emit *context* bytes))

(defun label (name &key (offset 0) (context *context*))
  (assert (not (null context)))
  (delay name (offset)
    (+ offset
       (or (context-find-label context name)
           (error 'resolvable-condition
                  :path (format nil "Label ~A is undefined" name))))))

(defun set-label (name &optional (context *context*))
  (context-set-label context name)
  name)

;;;; Assembler Directives

(define-symbol-macro *origin* (context-address *context*))

(defun advance-to (offset &optional (fill-byte #xFF))
  (let ((delta (- offset (context-address *context*))))
    (when (< offset 0)
      (error "Cannot advance to ~X, it is less than the current assembly address (~X)"
	     offset (context-address *context*)))
    (context-emit *context* (make-array delta :initial-element fill-byte))))

(defun align (alignment &optional (fill-byte #xFF))
  (advance-to (* alignment (ceiling (context-address *context*) alignment)) fill-byte))

(defun db (&rest bytes)
  (dolist (byte bytes) (context-emit *context* (encode-byte byte))))

(defun dw (&rest words)
  (dolist (word words) (context-emit *context* (encode-word word))))

;;;;
;;;; Definition of Addressing Modes
;;;;

(defclass 6502-addressing-mode () ())
(defclass 6502-mode-with-param (6502-addressing-mode)
  ((parameter :reader parameter :initarg :parameter)))
(defclass 6502-mode-param-8  (6502-mode-with-param) ())
(defclass 6502-mode-param-16 (6502-mode-with-param) ())

(defgeneric operand-dwim (object parameter)
  (:method ((object 6502-mode-with-param) x) x))

;; define-addrress-mode: Macro to automate generation of addressing
;; mode classes and constructor functions.
(defmacro define-addressing-mode (name superclass-list)
  `(progn
    (defclass ,name ,superclass-list ())
    (defgeneric ,name (param))
    (defmethod ,name (param)
      (let ((object (make-instance ',name)))
        (setf (slot-value object 'parameter)
              (operand-dwim object param))
        object))))

;; Implicit address modes are currently specified by passing nil to #'assemble
;; We consider the accumulator address mode a special case of implicit addressing.

(defclass zero-page-mode (6502-mode-param-8)  ()) ;; Expressions containing an address in the zero page
(defclass absolute-mode  (6502-mode-param-16) ()) ;; Expressions containing a 16-bit literal address

(define-addressing-mode imm  (6502-mode-param-8))   ;; Immediate
(define-addressing-mode zp   (zero-page-mode))      ;; Zero Page
(define-addressing-mode zpx  (zero-page-mode))      ;; Zero Page, X
(define-addressing-mode zpy  (zero-page-mode))      ;; Zero Page, Y {for STX/LDX instructions}
(define-addressing-mode idxi (zero-page-mode))      ;; Indexed Indirect ($aa,X)
(define-addressing-mode indi (zero-page-mode))      ;; Indirect Indexed ($aa),Y
(define-addressing-mode mem  (absolute-mode))       ;; Absolute Address
(define-addressing-mode abx  (absolute-mode))       ;; Absolute, X
(define-addressing-mode aby  (absolute-mode))       ;; Absolute, Y {for LDX instruction}
(define-addressing-mode indirect (absolute-mode))   ;; Indirect

(define-addressing-mode relative (6502-mode-param-8)) ;; PC-Relative offset (for branch instructions)

(defun rel (label)
  (let ((addr (context-address *context*))
        (label (label label)))
    (relative (delay :relative (label) (- label addr 2)))))

;;; Instruction parameters, according to addressing mode

(defgeneric parameter-bytes (parameter)
  (:documentation "Generate byte vector for instruction parameter"))

(defmethod parameter-bytes ((x null)) #())  ; Implicit/accumulator operand
(defmethod parameter-bytes ((mode 6502-mode-param-8))
  (encode-byte (parameter mode)))
(defmethod parameter-bytes ((mode 6502-mode-param-16))
  (encode-word (parameter mode)))
(defmethod parameter-bytes ((mode relative))
  (encode-signed-byte (parameter mode)))

(defmethod parameter-bytes ((mode indirect))
  (encode-word
   (delay nil ((address (parameter mode)))
     (if (= #xFF (logand address #xFF))
         (error "Indirect jump through ~X tickles 6502 page wraparound bug." address)
         address))))

;;;;
;;;; The 6502 Instruction Set (see http://axis.llx.com/~nparker/a2/opcodes.html)
;;;;

(defgeneric assemble (mnemonic parameter)
  (:documentation "Assemble an instruction and its parameter, producing a vector of byte values.")
  (:method (mnemonic parameter)
    (error "Don't know how to assemble instruction ~A ~A" mnemonic parameter)))

(defgeneric choose-opcode (mnemonic parameter)
  (:documentation "Choose the correct opcode for an instruction according to addressing mode")
  (:method (mnemonic parameter)
    (error "Invalid addressing mode or instruction (~A,~A)" mnemonic parameter)))

(defmethod assemble ((instruction symbol) parameter)
  (concatenate 'vector
               (vector (choose-opcode instruction parameter))
               (parameter-bytes parameter)))

(defmacro def6502 (name encoder &rest args)
  `(progn
    (defmethod choose-opcode ((instruction (eql ',name)) parameter)
      (funcall #',encoder parameter ,@args))
    (defun ,name (&optional operand)
      (context-emit-instruction *context* (assemble ',name operand)))))

(defun invalid-operand-error (instr-description operand)
  (error "Invalid operand or addressing mode for ~A: ~A"
         (or instr-description "this instruction")
         operand))

;;; Group 1:
;;;        ORA     AND     EOR     ADC     STA     LDA     CMP     SBC
;;; (zp,X)  01      21      41      61      81      A1      C1      E1
;;; zp      05      25      45      65      85      A5      C5      E5
;;; #       09      29      49      69              A9      C9      E9
;;; abs     0D      2D      4D      6D      8D      AD      CD      ED
;;; (zp),Y  11      31      51      71      91      B1      D1      F1
;;; zp,X    15      35      55      75      95      B5      D5      F5
;;; abs,Y   19      39      59      79      99      B9      D9      F9
;;; abs,X   1D      3D      5D      7D      9D      BD      DD      FD

(defmethod choose-opcode ((instruction (eql 'sta)) (operand imm))
  (invalid-operand-error instruction operand))

(defun group-1-addr-code (x)
  (typecase x
    (idxi #b000)  ;   (zero page,X)
    (zp   #b001)  ;   zero page
    (imm  #b010)  ;   #immediate
    (mem  #b011)  ;   absolute
    (indi #b100)  ;   (zero page),Y
    (zpx  #b101)  ;   zero page,X
    (aby  #b110)  ;   absolute,Y
    (abx  #b111)  ;   absolute,X
    (t (invalid-operand-error nil x))))

(defun group-1-asm (parameter opcode)
  (join-masks
   (join-masks (ash opcode 5)
	       (ash (group-1-addr-code parameter) 2))
   #b01))

(def6502 ORA  group-1-asm #b000)
(def6502 ANDA group-1-asm #b001)
(def6502 EOR  group-1-asm #b010)
(def6502 ADC  group-1-asm #b011)
(def6502 STA  group-1-asm #b100)
(def6502 LDA  group-1-asm #b101)
(def6502 CMP  group-1-asm #b110)
(def6502 SBC  group-1-asm #b111)

;;; Group 2:
;;;                 ASL     ROL     LSR     ROR     STX     LDX     DEC     INC
;;; #                                                       A2
;;; zp              06      26      46      66      86      A6      C6      E6
;;; A               0A      2A      4A      6A
;;; abs             0E      2E      4E      6E      8E      AE      CE      EE
;;; zp,X/zp,Y       16      36      56      76      96      B6      D6      F6
;;; abs,X/abs,Y     1E      3E      5E      7E              BE      DE      FE

(defun group-2/3-addr-code (x types)
  (unless (typep x types) (invalid-operand-error nil x))
  (typecase x
    (imm  #b000)   ; #immediate
    (zp   #b001)   ; zero page
    (null #b010)   ; accumulator
    (mem  #b011)   ; absolute
    (zpx  #b101)   ; zero page,X
    (zpy  #b101)   ; zero page,Y {for STX, LDX}
    (abx  #b111)   ; absolute,X
    (aby  #b111))) ; absolute,Y  {for LDX}

(defun group-2-asm (parameter opcode &optional (types '(or zp null mem zpx abx)))
  (join-masks
   (join-masks (ash opcode 5)
	       (ash (group-2/3-addr-code parameter types) 2))
   #b10))

;; Default set of address modes is suitable for ASL, ROR, LSR, ROR
(def6502 ASL group-2-asm #b000)
(def6502 ROL group-2-asm #b001)
(def6502 LSR group-2-asm #b010)
(def6502 ROR group-2-asm #b011)
(def6502 STX group-2-asm #b100 '(or zp mem zpy))
(def6502 LDX group-2-asm #b101 '(or imm zp mem zpy aby))
(def6502 DEC group-2-asm #b110 '(or zp mem zpx abx))
(def6502 INC group-2-asm #b111 '(or zp mem zpx abx))

;;; Group 3:
;;         BIT     JMP     JMP()   STY     LDY     CPY     CPX
;; #                                       A0      C0      E0
;; zp      24                      84      A4      C4      E4
;; abs     2C      4C      6C      8C      AC      CC      EC
;; zp,X                            94      B4
;; abs,X                                   BC

(defun group-3-asm (parameter opcode types)
  (join-masks
   (join-masks (ash opcode 5)
	       (ash (group-2/3-addr-code parameter types) 2))
   #b00))

(def6502 BITA group-3-asm #b001 '(or zp mem))
(def6502 STY  group-3-asm #b100 '(or zp mem zpx))
(def6502 LDY  group-3-asm #b101 '(or imm zp mem zpx abx))
(def6502 CPY  group-3-asm #b110 '(or imm zp mem))
(def6502 CPX  group-3-asm #b111 '(or imm zp mem))

;;; Special case JMP, because the high bits are not fixed.

(defun asm-jmp (parameter)
  (typecase parameter
    (mem      #x4C)
    (indirect #x6C)
    (t (invalid-operand-error 'jmp parameter))))

(def6502 JMP asm-jmp)

;;; Conditional Branches:

(defun simple-instruction (operand value &optional (type 'null))
  (unless (typep operand type) (invalid-operand-error "simple instruction" operand))
  value)

;; The conditional branch instructions all have the form xxy10000. The flag
;; indicated by xx is compared with y, and the branch is taken if they are equal.

;; xx	flag
;; 00	negative
;; 01	overflow
;; 10	carry
;; 11	zero

;; This gives the following branches:
(def6502 BPL simple-instruction #x10 '(or imm relative))
(def6502 BMI simple-instruction #x30 '(or imm relative))
(def6502 BVC simple-instruction #x50 '(or imm relative))
(def6502 BVS simple-instruction #x70 '(or imm relative))
(def6502 BCC simple-instruction #x90 '(or imm relative))
(def6502 BCS simple-instruction #xB0 '(or imm relative))
(def6502 BNE simple-instruction #xD0 '(or imm relative))
(def6502 BEQ simple-instruction #xF0 '(or imm relative))

(defvar *branch-instructions* '(BPL BMI BVC BVS BCC BCS BNE BEQ))

;;; Miscellaneous Instructions:

;; The remaining instructions are probably best considered simply by listing
;; them. Here are the interrupt and subroutine instructions:

(def6502 BRK simple-instruction #x00)
(def6502 JSR simple-instruction #x20 'mem)
(def6502 RTI simple-instruction #x40)
(def6502 RTS simple-instruction #x60)

;; (JSR is the only absolute-addressing instruction that doesn't fit the aaabbbcc pattern.)

;; Other single-byte instructions:

;; PHP 	PLP 	PHA 	PLA 	DEY 	TAY 	INY 	INX
;; 08 	28 	48 	68 	88 	A8 	C8 	E8

;; CLC 	SEC 	CLI 	SEI 	TYA 	CLV 	CLD 	SED
;; 18 	38 	58 	78 	98 	B8 	D8 	F8

;; TXA 	TXS 	TAX 	TSX 	DEX 	NOP
;; 8A 	9A 	AA 	BA 	CA 	EA

(def6502 PHP simple-instruction #x08)
(def6502 PLP simple-instruction #x28)
(def6502 PHA simple-instruction #x48)
(def6502 PLA simple-instruction #x68)
(def6502 DEY simple-instruction #x88)
(def6502 TAY simple-instruction #xA8)
(def6502 INY simple-instruction #xC8)
(def6502 INX simple-instruction #xE8)

(def6502 CLC simple-instruction #x18)
(def6502 SEC simple-instruction #x38)
(def6502 CLI simple-instruction #x58)
(def6502 SEI simple-instruction #x78)
(def6502 TYA simple-instruction #x98)
(def6502 CLV simple-instruction #xB8)
(def6502 CLD simple-instruction #xD8)
(def6502 SED simple-instruction #xF8)

(def6502 TXA simple-instruction #x8A)
(def6502 TXS simple-instruction #x9A)
(def6502 TAX simple-instruction #xAA)
(def6502 TSX simple-instruction #xBA)
(def6502 DEX simple-instruction #xCA)
(def6502 NOP simple-instruction #xEA)

;;;; Syntactic sugar

;;; For absolute and absolute indexed modes, resolve labels automatically.
(defmethod operand-dwim ((op absolute-mode) (parameter symbol)) (label parameter))

;;; For the JSR instruction, accept a label directly as the parameter,
;;; because there's only one addressing mode.
(defmethod assemble ((mnemonic (eql 'JSR)) (parameter symbol))
  (assemble mnemonic (mem (label parameter))))

;;; Similarly for branch instructions..
(macrolet ((branch (mnemonic)
             `(defmethod assemble ((mnemonic (eql ',mnemonic)) (parameter symbol))
                (assemble mnemonic (rel parameter)))))
  (branch BPL)
  (branch BMI)
  (branch BVC)
  (branch BVS)
  (branch BCC)
  (branch BCS)
  (branch BNE)
  (branch BEQ))

