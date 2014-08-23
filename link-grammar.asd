(in-package :cl)
(defpackage link-grammar-asd (:use :cl :asdf))
(in-package :link-grammar-asd)

(defsystem link-grammar
  :version "0.1"
  :author ("Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Oleg Sivokon <olegsivokon@gmail.com>")
  :license "LGPL2"
  :depends-on (:alexandria :iterate :split-sequence :cffi :cl-containers)
  :serial t
  :components ((:file "package")
               (:file "clinkgrammar"))
  :description "Bindings for link-grammar parser:
                http://www.abisource.com/projects/link-grammar/")
