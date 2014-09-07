;; -*- mode: lisp; package: link-grammar -*-

(in-package :link-grammar)

(defvar *link-index* 0)
(defvar *sentence-index* 0)
(defvar *dictionary* nil)
(defvar *options* nil)
(defvar *sentence* nil)
(defvar *linkage* nil)

(defclass sentence ()
  ((handle :initarg :handle :accessor handle)))

(defclass linkage ()
  ((handle :initarg :handle :accessor handle)))

(defclass dictionary ()
  ((language :initarg :lang :accessor language)))

(defclass parse-options ()
  ((handle :initarg :handle :accessor handle)
   (verbosity :initarg :verbosity :accessor verbosity)
   (linkage-limit :initarg :linkage-limit :accessor linkage-limit)
   (disjunct-cost :initarg :disjcunct-cost :accessor disjunct_cost)
   (min-null-count :initarg :min-null-count :accessor min-null-count)
   (max-null-count :initarg :max-null-count :accessor max-null-count)
   (islandsp :initarg :islandsp :accessor islandsp)
   (short-length :initarg :short-length :accessor short-length)
   (max-memory :initarg :max-memory :accessor max-memory)
   (max-parse-time :initarg :max-parse-time :accessor max-parse-time)
   (cost-model-type :initarg :cost-model-type :accessor cost-model-type)
   (display-morphology-p :initarg :display-morphology-p
                         :accessor display-morphology-p)
   (spell-guess-p :initarg :spell-guess-p :accessor spell-guess-p)
   (all-short-connectors-p :initarg :all-short-connectors-p
                           :accessor all-short-connectors-p)))

(defmethod initialize-instance :after ((this dictionary) &rest initargs)
  (destructuring-bind (&key lang) initargs
    ;; It looks like it's also might be necessary to call `setlocale'
    ;; to ensure the dictionary files are red properly.
    (setf (language this)
          (if lang
              (dictionary_create_lang lang)
              (dictionary_create_default_lang)))))

(defun data-dir () (dictionary_get_data_dir))

(defmacro with-dictionary ((dictionary &optional language data-dir) &body body)
  (alexandria:with-gensyms (dir)
    `(let (,dictionary)
       (unwind-protect
            (let ((,dir ,data-dir))
              (when ,dir
                (dictionary_set_data_dir
                 (translate-logical-pathname ,dir)))
              (setf ,dictionary (make-instance 'dictionary :lang ,language)
                    *dictionary* ,dictionary)
              ,@body)
         (when ,dictionary
           (dictionary_delete (language ,dictionary))
           (setf *dictionary* nil))))))

(defmacro with-sentence ((sentence raw-input &optional
                                   (dictionary '*dictionary*)) &body body)
  `(let (,sentence)
     (unwind-protect
          (progn
            (setf ,sentence
                  (make-instance
                   'sentence
                   :handle (sentence_create ,raw-input (language ,dictionary)))
                  *sentence* ,sentence)
            ,@body)
       (when ,sentence
         (sentence_delete (handle ,sentence))
         (setf *sentence* nil)))))

(defmacro with-linkage ((linkage &optional
                                 (index 0)
                                 (sentence '*sentence*)
                                 (opts '*options*)) &body body)
  `(let (,linkage)
     (unwind-protect
          (progn
            (setf ,linkage
                  (make-instance
                   'linkage
                   :handle (linkage_create
                            ,index (handle ,sentence) (handle ,opts)))
                  *linkage* ,linkage)
            ,@body)
       (when ,linkage
         (linkage_delete (handle ,linkage))
         (setf *linkage* nil)))))

(defmacro with-options ((opts) &body body)
  `(let (,opts)
     (unwind-protect
          (progn
            (setf ,opts
                  (make-instance
                   'parse-options
                   :handle (parse_options_create))
                  *options* ,opts)
            ,@body)
       (when ,opts
         (parse_options_delete (handle ,opts))
         (setf *options* nil)))))

(defmacro with-ith-link (index &body body)
  `(let ((*link-index* ,index))
     ,@body))

(defmacro with-ith-sentence (index &body body)
  `(let ((*sentence-index* index))
     ,@body))

(defmethod timer-expired-p ((this parse-options))
  (parse_options_timer_expired (handle this)))

(defmethod memory-exhausted ((this parse-options))
  (parse_options_memory_exhausted (handle this)))

(defmethod resource-exhausted ((this parse-options))
  (parse_options_resources_exhausted (handle this)))

(defmethod reset-resources ((this parse-options))
  (parse_options_reset_resources (handle this)))

(defmethod parse ((this sentence) (options parse-options))
  "Parses THIS sentence into a structured representation and returns
the number of linkages found in it."
  (sentence_parse (handle this) (handle options)))

(defmethod split ((this sentence) (options parse-options))
  "Parses THIS sentence into a structured representation and returns
the number of linkages found in it."
  (sentence_split (handle this) (handle options)))

(defmethod sentence-length ((this sentence))
  (sentence_length (handle this)))

(defmethod null-count ((this sentence))
  (sentence_null_count (handle this)))

(defmethod linkages ((this sentence) &key (linkage-type :none))
  (let ((handle (handle this)))
    (case linkage-type
      (:none (sentence_num_linkages_found handle))
      (:valid (sentence_num_valid_linkages handle))
      (:post-processed (sentence_num_linkages_post_processed handle)))))

(defmethod violations ((this sentence) &optional (index *sentence-index*))
  (sentence_num_violations (handle sentence) index))

(defmethod disjunct-cost ((this sentence) &optional (index *sentence-index*))
  (sentence_disjunct_cost (handle sentence) index))

(defmethod link-cost ((this sentence) &optional (index *sentence-index*))
  (sentence_link_cost (handle sentence) index))

(defmethod print-diagram ((this linkage)
                          &optional display-wallsp
                            (screen-wdith (linkage-print-length this)))
  "Creates a diagram which shows the parse of THIS linkage.
DISPLAY-WALLSP if true, will instruct to print the linkage margins (walls).
SCREEN-WDITH by default, will try to guess the space needed to print the linkage."
  (format t "~&screen-wdith: ~d" screen-wdith)
  (linkage_print_diagram (handle this) display-wallsp screen-wdith))

(defmethod linkage-print-length ((this linkage))
  (iter
    (for word :in (words this))
    (format t "~&word: ~s" word)
    (summing (1+ (length word)) :into result)
    (finally (return (1- result)))))

(defmethod print-postscript ((this linkage) display-wallsp screen-wdith)
  (linkage_print_postscript (handle this) display-wallsp screen-wdith))

(defmethod sentence ((this linkage))
  (linkage_get_sentence (handle this)))

(defmethod num-words ((this linkage))
  (linkage_get_num_words (handle this)))

(defmethod num-links ((this linkage))
  (linkage_get_num_links (handle this)))

(defmethod left-word ((this linkage) &optional (index *link-index*))
  (linkage_get_link_lword (handle this) index))

(defmethod right-word ((this linkage) &optional (index *link-index*))
  (linkage_get_link_rword (handle this)))

(defmethod link-length ((this linkage) &optional (index *link-index*))
  (linkage_get_link_length (handle this) index))

(defmethod link-label ((this linkage)
                       &optional (index *link-index*)
                       &key (direction :none))
  (ecase direction
    (:none (linkage_get_link_label (handle this) index))
    (:left (linkage_get_link_llabel (handle this) index))
    (:right (linkage_get_link_rlabel (handle this) index))))

(defmethod num-domains ((this linkage) &optional (index *link-index*))
  (linkage_get_link_num_domains (handle this)))

(defmethod domain-names ((this linkage) &optional (index *link-index*))
  (linkage_get_link_domain_names (handle this) index))

(defmethod words ((this linkage))
  "Returns a list of words of THIS linkage"
  (iter
    (with words := (linkage_get_words (handle this)))
    (for i :below (linkage_get_num_words (handle this)))
    (collect (mem-aref words :string i))))

(defmethod word ((this linkage) index)
  (linkage_get_word (handle this) index))

(defmethod print-links-and-domains ((this linkage))
  (linkage_print_links_and_domains (handle this)))

(defmethod print-senses ((this linkage))
  (linkage_print_senses (handle this)))

(defmethod print-constituent-tree ((this linkage) &key mode)
  (linkage_print_constituent_tree (handle this)))

(defmethod unused-word-cost ((this linkage))
  (linkage_unused_word_cost (handle this)))

(defmethod disjunct-cost ((this linkage) &optional index)
  (declare (ignore index))
  (linkage_disjunct_cost (handle this)))

(defmethod link-cost ((this linkage) &optional index)
  (declare (ignore index))
  (linkage_link_cost (handle this)))

(defmethod corpus-cost ((this linkage))
  (linkage_corpus_cost (handle this)))

(defmethod violation-name ((this linkage))
  (linkage_get_violation_name (handle this)))
