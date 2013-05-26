;;(defpackage #:rw-asd
;;  (:use :cl :asdf))

;;(in-package :rw-asd)

(asdf:defsystem rw
  :name "rw"
  :depends-on ("yacc" "cl-lex" "cl-ppcre")
  :components ((:file "rw-package")
               (:file "grammar" :depends-on ("rw-package"))))
