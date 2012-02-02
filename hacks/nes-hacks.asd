;;;; This system is just here for testing changes the assembler.

(asdf:defsystem :nes-hacks
  :name "NES Hacks"
  :description "Tests and hacks for the NES using asm6502."
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style licesnse"
  :serial t
  :depends-on (:asm6502 :ichr)
  :components ((:file "audio-test-1")
               (:file "music-demo")
               (:file "nes-hacklets")
               (:file "nes-test-1")
               (:file "nes-test-2")
               (:file "dollhouse")))
