;; -*- mode: lisp; package: link-grammar -*-

;; #include <locale.h>
;; #include <stdio.h>
;; #include "link-includes.h"

;; int main()
;; {
;;     Dictionary    dict;
;;     Parse_Options opts;
;;     Sentence      sent;
;;     Linkage       linkage;
;;     char *        diagram;
;;     int           i, num_linkages;
;;     char *        input_string[] = {
;;        "Grammar is useless because there is nothing to say -- Gertrude Stein.",
;;        "Computers are useless; they can only give you answers -- Pablo Picasso."};

;;     setlocale(LC_ALL, "");
;;     opts = parse_options_create();
;;     dict = dictionary_create_lang("en");
;;     if (!dict) {
;;         printf ("Fatal error: Unable to open the dictionary\n");
;;         return 1;
;;     }

;;     for (i=0; i<2; ++i) {
;;         sent = sentence_create(input_string[i], dict);
;;         sentence_split(sent, opts);
;;         num_linkages = sentence_parse(sent, opts);
;;         if (num_linkages > 0) {
;;             linkage = linkage_create(0, sent, opts);
;;             printf("%s\n", diagram = linkage_print_diagram(linkage));
;;             linkage_free_diagram(diagram);
;;             linkage_delete(linkage);
;;         }
;;         sentence_delete(sent);
;;     }

;;     dictionary_delete(dict);
;;     parse_options_delete(opts);
;;     return 0;
;; }

(in-package :link-grammar)

(defun test-dictionary ()
  (with-dictionary foo ()
    (format t "~&~s" foo)))

(defun test-hello-world ()
  (with-dictionary en ()
    (with-options opts
      (loop :for saying :in
         (list "Grammar is useless because there is nothing to say -- Gertrude Stein."
               "Computers are useless; they can only give you answers -- Pablo Picasso.")
         :do (with-sentence sent en saying
               (with-linkage link sent opts 0
                   (format t "~&linkage: ~s~&~s"
                           link (print-diagram link))))))))

(defmacro test-with-gensysm (foo &body body)
  (with-output-to-string
    
