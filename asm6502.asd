(asdf:defsystem :asm6502
  :name "6502 Assembler"
  :description "6502 Assembler, and assorted utilities."
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "assembler")
               (:file "cycle-counting")
               (:file "6502-utils")
               (:file "nes")
               (:file "nesmus")))
