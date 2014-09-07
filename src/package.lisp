(defpackage :link-grammar
  (:nicknames :lg)
  (:use :cl :iterate :cffi)
  ;; macros
  (:export :with-dictionary
           :with-sentence
           :with-linkage
           :with-options
           :with-ith-link
           :with-ith-sentence
           ;; methods
           :timer-expired-p
           :memory-exhausted
           :resource-exhausted
           :parse
           :split
           :sentence-length
           :null-count
           :linkages
           :violations
           :link-cost
           :print-diagram
           :linkage-print-length
           :print-postscript
           :num-words
           :num-links
           :left-word
           :right-word
           :link-length
           :link-label
           :num-domains
           :domain-names
           :words
           :word
           :print-links-and-domains
           :print-senses
           :print-constituent-tree
           :unused-word-cost
           :disjunct-cost
           :corpus-cost
           :violation-name
           ;; classes
           :dictionary
           :sentence
           :linkage
           :parse-options
           ;; special variables
           :*link-index*
           :*sentence-index*
           :*options*
           :*sentence*
           :*linkage*
           ;; functions
           :data-dir)
  (:documentation
   "@a[http://www.abisource.com/projects/link-grammar/]{link-grammar}
    Provides Common Lisp bindings for link-grammar library.

    These bindings provide thin wrapper and a simple CLOS abstraction
    layer on top of it.

    @begin[Using the library]{section}
    The library provides these macros

    @aboutfun{with-sentence}
    @aboutfun{with-linkage}
    @aboutfun{with-options}
    @aboutfun{with-sentence}
    @aboutfun{with-ith-link}
    @aboutfun{with-ith-sentence}

    To make management of C++ objects easier.
    @end{section}

    @begin[Another section]{section}
    Dummy text.

    @end{section}"))
