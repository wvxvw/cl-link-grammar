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
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "clinkgrammar")
                         (:file "clink-ojbects"))))
  :description "Bindings for link-grammar parser:
                http://www.abisource.com/projects/link-grammar/"
  :long-description
  #.(with-open-file
        (stream (merge-pathnames
                 #p"README.org" (or *load-pathname* *compile-file-pathname*))
                :if-does-not-exist nil :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream)) seq)))
  :in-order-to ((test-op (load-op :link-grammar-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) :link-grammar.test)
                             :link-grammar.test)))

(defsystem :link-grammar-test
  :author ("Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Oleg Sivokon <olegsivokon@gmail.com>")
  :description "Minimal test suite for testing link-grammar bindings"
  :license "LGPL2"
  :depends-on (:link-grammar :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-lg" :depends-on ("suite"))))))
