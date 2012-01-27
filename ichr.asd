(asdf:defsystem :ichr
  :name "iCHR"
  :description "NES character bitmap converter"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:asm6502 :skippy)
  :serial t
  :components ((:file "ichr")))
