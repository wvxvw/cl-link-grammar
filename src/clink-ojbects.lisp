;; -*- mode: lisp; package: link-grammar -*-

(in-package :link-grammar)

(defvar *link-index* 0)
(defvar *sentence-index* 0)
(defvar *dictionary* nil)
(defvar *options* nil)
(defvar *sentence* nil)
(defvar *linkage* nil)

(closer-mop:defclass sentence (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle))
  (:metaclass virtual-metaclass))

(closer-mop:defclass linkage (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle))
  (:metaclass virtual-metaclass))

(closer-mop:defclass dictionary (closer-mop:standard-object)
  ((language :initarg :lang :accessor language))
  (:metaclass virtual-metaclass))

(defgeneric verbosity (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{the verbosity level of the parser, higher level means
      that the parser will be more verbose}
    @short{Default value is 0, seems like 2 is the highest value.}"))

(defgeneric linkage-limit (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The maximum number of links a parser will try to find in
      a sentence it parses.}
    @short{Default value is 10000}

    This parameter determines the @em{maximum} number of linkages that
    are considered in post-processing. If more than
    @code{linkage-limit} linkages found, then a random sample of
    @code{linkage-limit} is chosen for post-processing. When this
    happen a warning is displayed at verbosity levels bigger than 1."))

(defgeneric disjunct-cost (instance &optional index)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The maximum number of links a parser will try to find in
      a sentence it parses.}
    @short{Determines the maximum disjunct cost used during
    parsing, where the cost of a disjunct is equal to the maximum cost
    of all of its connectors. The default is that all disjuncts, no
    matter what their cost, are considered.}"))

(defgeneric max-null-count (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The maximum number of links a parser will try to find in
      a sentence it parses.}
    @short{Default value is 0}

    This determines the maximum number of null links that
    a parse might have. A call to @fun{parse} will find all
    linkages having the minimum number of null links within the range
    specified by this parameter in the @class{parse-options}.

    @see-slot{min-null-count}"))

(defgeneric min-null-count (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The lower bound on the range set by @code{max-null-count}.}
    @short{Default value is 0}

    @see-slot{max-null-count}"))

(defgeneric islandsp (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{@code{T} if parser is allowed to parse disconnected links.}
    @short{Default value is nil}

    This option determines whether or not \"islands\" of links are
    allowed. For example, the following linkage has an island:

@begin{pre}
    +------Wd-----+
    |     +--Dsu--+---Ss--+-Paf-+     +--Dsu--+---Ss--+--Pa-+
    |     |       |       |     |     |       |       |     |
  ///// this sentence.n is.v false.a this sentence.n is.v true.a
@end{pre}

    I.e. islands are the connected components of the link graph."))

(defgeneric short-length (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The lenght of the longest allowed links.}
    @short{This parameter determines how long the links are allowed to
    be. The intended use of this is to speed up parsing by not
    considering very long links for most connectors, since they are
    very rarely used in a correct parse. An entry for
    @code{UNLIMITED-CONNECTORS} in the dictionary will specify which
    connectors are exempt from the length limit.}"))

(defgeneric max-memory (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The memory limit for the parser (in bytes?).}
    @short{Determines the maximum memory allowed during parsing. This is used
    just as @code{max-parse-time} is, so that the parsing process is
    terminated as quickly as possible after the total memory
    (including that allocated to all dictionaries, etc.) exceeds the
    maximum allowed.}

    @see-slot{max-parse-time}"))

(defgeneric max-parse-time (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The time to strart to wrap up the parsing (in milliseconds?).}
    @short{Determines the @em{approximate} maximum time that parsing is
    allowed to take. The way it works is that after this time has
    expired, the parsing process is artificially forced to complete
    quickly by pretending that no further solutions (entries in the
    hash table) can be constructed. The actual parsing time might be
    slightly longer.}"))

(defgeneric cost-model-type (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{The type of the model used by the parser (I think it's a string).}
    @short{The cost model type for ranking linkages. Currently, there are two
    models: VDAL (1) and CORPUS (2). The VDAL model ranks parses from
    lowest to highest cost in and-cost, disjunct-cost,
    unused-word-cost and structure-violations-cost. The CORPUS model
    ranks parses according to the frequency of use of disjuncts, based
    on a statistical analysis of a collection of texts.}"))

(defgeneric all-short-connectors-p (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options}}
    @return{@code{T} if there is a limit on how far connectors may be.}
    @short{If true, then all connectors have length restrictions imposed on
    them -- they can be no farther than @code{short-length} apart. This is
    used when parsing in \"panic\" mode, for example.}"))

(closer-mop:defclass parse-options (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle)
   (verbosity :initarg :verbosity
              :accessor :verbosity
              :allocation :virtual
              :function 'virt-verbosity)
   (linkage-limit :initarg :linkage-limit
                  :accessor linkage-limit
                  :allocation :virtual
                  :function 'virt-linkage-limit)
   (disjunct-cost :initarg :disjcunct-cost
                  :accessor disjunct_cost
                  :allocation :virtual
                  :function 'virt-disjunct-cost)
   (min-null-count :initarg :min-null-count
                   :accessor min-null-count
                   :allocation :virtual
                   :function 'virt-min-null-count)
   (max-null-count :initarg :max-null-count
                   :accessor max-null-count
                   :allocation :virtual
                   :function 'virt-max-null-count)
   (islandsp :initarg :islandsp
             :accessor islandsp
             :allocation :virtual
             :function 'virt-islandsp)
   (short-length :initarg :short-length
                 :accessor short-length
                 :allocation :virtual
                 :function 'virt-short-length)
   (max-memory :initarg :max-memory
               :accessor max-memory
               :allocation :virtual
               :function 'virt-max-memory)
   (max-parse-time :initarg :max-parse-time
                   :accessor max-parse-time
                   :allocation :virtual
                   :function 'virt-max-parse-time)
   (cost-model-type :initarg :cost-model-type
                    :accessor cost-model-type
                    :allocation :virtual
                    :function 'virt-cost-model-type)
   (display-morphology-p :initarg :display-morphology-p
                         :accessor display-morphology-p
                         :allocation :virtual
                         :function 'virt-display-morphology-p)
   (spell-guess-p :initarg :spell-guess-p
                  :accessor spell-guess-p
                  :allocation :virtual
                  :function 'virt-spell-guess-p)
   (all-short-connectors-p :initarg :all-short-connectors-p
                           :accessor all-short-connectors-p
                           :allocation :virtual
                           :function 'virt-short-connectors-p))
  (:metaclass virtual-metaclass)
  (:documentation
   "@short{This class maps to @code{Parse_Options} C++ class. It uses
        @code{virtual-metaclass} metaclass for all slots but the
        pointer to the object in C++ environment. This means that it
        doesn't store the information in its slots, rather it
        delegates the calls to slots to C++ code.}

    @see-slot{verbosity}
    @see-slot{linkage-limit}
    @see-slot{disjunct-cost}
    @see-slot{nim-null-count}
    @see-slot{max-null-count}
    @see-slot{islandsp}
    @see-slot{short-length}
    @see-slot{max-memory}
    @see-slot{max-parse-time}
    @see-slot{cost-model-type}
    @see-slot{display-morphology-p}
    @see-slot{spell-guess-p}
    @see-slot{all-short-connectors-p}
"))

(defun virt-verbosity (opts key &optional value)
  (ecase key
    (:get (parse_options_get_verbosity (handle opts)))
    (:set (parse_options_set_verbosity (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-linkage-limit (opts key &optional value)
  (ecase key
    (:get (parse_options_get_linkage_limit (handle opts)))
    (:set (parse_options_set_linkage_limit (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-disjunct-cost (opts key &optional value)
  (ecase key
    (:get (parse_options_get_disjunct_cost (handle opts)))
    (:set (parse_options_set_disjunct_cost (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-min-null-count (opts key &optional value)
  (ecase key
    (:get (parse_options_get_min_null_count (handle opts)))
    (:set (parse_options_set_min_null_count (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-max-null-count (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_null_count (handle opts)))
    (:set (parse_options_set_max_null_count (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-islandsp (opts key &optional value)
  (ecase key
    (:get (parse_options_get_islands_ok (handle opts)))
    (:set (parse_options_set_islands_ok (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-short-length (opts key &optional value)
  (ecase key
    (:get (parse_options_get_short_length (handle opts)))
    (:set (parse_options_set_short_length (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-max-memory (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_memory (handle opts)))
    (:set (parse_options_set_max_memory (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-max-parse-time (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_parse_time (handle opts)))
    (:set (parse_options_set_max_parse_time (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-cost-model-type (opts key &optional value)
  (ecase key
    (:get (parse_options_get_cost_model_type (handle opts)))
    (:set (parse_options_set_cost_model_type (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-display-morphology-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_display_morphology (handle opts)))
    (:set (parse_options_set_display_morphology (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-spell-guess-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_spell_guess (handle opts)))
    (:set (parse_options_set_spell_guess (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun virt-short-connectors-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_all_short_connectors (handle opts)))
    (:set (parse_options_set_all_short_connectors (handle opts) value))
    (:setp t)
    (:unset (values))))

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
  "@arg[dictionary]{A symbol to bind to the @class{dictionary}, in addition to
      this symbol *dictionary* special variable will be also bound to the currently
      active dictionary}
   @arg[language]{Language code (optional)}
   @arg[data-dir]{Directory to look up for dictionary files (optional)}
   @arg[body]{Forms to bind the @code{dictionary} in}
   @return{The result of the last form of the @code{body}}

   @short{Use this macro to create and manage @class{dictionary} objects.}

   @see{dictionary}"
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
