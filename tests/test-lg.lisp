;; -*- mode: lisp; package: link-grammar.test -*-

(in-package :link-grammar.test)

(in-suite :link-grammar.test)

(def-suite test-suite
    :description "Minimal testing suite for link-grammar bindings.")

(defvar *diagrams*
  (list "
    +-----------------------------------------------Xp----------------------
    +------------------------------------Xx---------------------------------
    |                         +-------------------B*d-------------------+   
    |                         +-----------------TOt----------------+    |   
    +--------WV--------+      |        +------CV------+            |    |   
    +----Wd----+---Ss--+--Pa--+---MVs--+---Cs--+-SFst-+-Ost-+      +--I-+   
    |          |       |      |        |       |      |     |      |    |   
LEFT-WALL Grammar[!] is.v useless.a because there.r is.v nothing to.r say.v 


-------------------------+
--+                      |
  |                      |
  |                      |
  +--------Wa-------+    |
  |       +----G----+    |
  |       |         |    |
--.r Gertrude.f Stein[!] . 

"
   "
    +-----------------------------------------------Xp-----------------------
    |                                 +-------------------Xx-----------------
    +----------------Xx---------------+---------WV---------+                 
    +---------WV--------+             |        +-----I-----+----Opn----+     
    +----Wd----+---Spx--+---Pa--+     +-Wd+-Sp-+     +--E--+-Ox-+      |     
    |          |        |       |     |   |    |     |     |    |      |     
LEFT-WALL computers.n are.v useless.a ; they can.v only give.v you answers.n 


------------------------+
--+                     |
  |                     |
  +-------Wa------+     |
  |     +----G----+     |
  |     |         |     |
--.r Pablo.m Picasso[!] . 

"))

(defvar *sentences*
  (list "Grammar is useless because there is nothing to say -- Gertrude Stein."
        "Computers are useless; they can only give you answers -- Pablo Picasso."))

(test test-hello-world
  "Prints two diagrams and makes sure that the results are ex expected"
  (is
   (every 'identity
          (lg:with-dictionary (en)
            (lg:with-options (opts)
              (iter
                (for saying :in *sentences*)
                (for dia :in *diagrams*)
                (collect
                    (lg:with-sentence (sent saying)
                      (lg:split sent opts)
                      (lg:with-linkage (link)
                        (string= dia (print-diagram link)))))))))))
