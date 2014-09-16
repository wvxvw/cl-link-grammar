;; -*- mode: lisp; package: link-grammar; fill-column: 80 -*-

(in-package :link-grammar)

;; * special variables

(defvar *dictionary* nil)
(defvar *options* nil)
(defvar *sentence* nil)
(defvar *linkage* nil)

;; * sentence definition
;; ** slot documentation

(defgeneric length (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of words in the tokenized sentence, including the
    boundary words and punctuation.}
    @short{There needs to be an example here}
    @see{words}"))

(defgeneric null-count (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of null links that were used in parsing the sentence.}
    @short{Null-links are usually an indicator that the parsing misidentified
      something about the sentence.  Thus higher null-count would call indicate
      either a parsing problem, or that a sentence was originally
      ungrammatical.}"))

(defgeneric num-linkages-found (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The total number of linkages found when parsing the sentence.}
    @short{This also includes linkages which required post-processor attention
      and those which didn't.}
    @see{num-valid-linkages}
    @see{num-linkages-postprocessed}"))

(defgeneric num-valid-linkages (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of linkages found while parsing the sentence, which were
      unaltered during post-processing.}
    @short{The last step at parsing the sentence is the post-processing.  During
      this step linkages may be altered.  This counts only those linkages, which
      weren't altered by post-processor.}"))

(defgeneric num-linkages-post-processed (instance)
  (:documentation
   "@arg[instance]{a @class{sentence}}
    @returns{The number of linkages altered by the post-processing parsing step.}
    @short{This counts the linkages which had to be altered by the post-processor.
      This is not necessary an indication of a problem with a sentence, however an
      ungrammatic sentence is more likely to produce a higher count of
      post-processor corrections.}"))

;; ** defclass sentence

(closer-mop:defclass sentence (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle)
   (input :initarg :input)
   (length :reader length
           :allocation :virtual
           :function sent-length)
   (null-count :reader null-count
               :allocation :virtual
               :function sent-null-count)
   (num-linkages-found :reader num-linkages-found
                       :allocation :virtual
                       :function sent-num-linkages-found)
   (num-valid-linkages :reader num-valid-linkages
                       :allocation :virtual
                       :function sent-num-valid-linkages)
   (num-linkages-post-processed :reader num-linkages-post-processed
                                :allocation :virtual
                                :function sent-num-linkages-post-processed))
  (:metaclass virtual-metaclass))

;; ** virtual slot support functions

(defun sent-length (sent key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (sentence_length (handle sent)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun sent-null-count (sent key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (sentence_null_count (handle sent)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun sent-num-linkages-found (sent key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (sentence_num_linkages_found (handle sent)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun sent-num-valid-linkages (sent key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (sentence_num_valid_linkages (handle sent)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun sent-num-linkages-post-processed (sent key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (sentence_num_linkages_post_processed (handle sent)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

;; ** constructor

(defun make-sentence (input)
  (let ((result (make-instance 'sentence :input input)))
    (when (cffi:null-pointer-p (handle result))
      (error "Couldn't create sentence"))
    (tg:finalize
     result (lambda () (sentence_delete (handle result))))))

;; * linkage definition
;; ** slot documentation

(defgeneric sentence (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The @class{sentence} to which this linkage belongs.}
    @short{Once sentence is parsed successfully, there will be multiple
      @class{linkage} objects associated with the words of the sentence.  This
      is a reference back to the sentence which created this linkage.}"))

(defgeneric num-words (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The number of words in the @code{sentence}.}
    @short{The number of words in the sentence for which this is a linkage. Note
      that this function does @em{not} return the number of words used in the
      current sublinkage.}"))

(defgeneric num-links (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The number of links used in the current sublinkage.}
    @short{The number of links used in the current sublinkage.}"))

(defgeneric words (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{A list of inflected words of the sublinkage.}
    @short{Returns a list of word spellings for the current sublinkage. These
      are the @code{\"inflected\"} spellings, such as @code{\"dog.n\"}. The
      original spellings can be obtained by calls to @code{(word sent
      wordnum)}.}"))

(defgeneric unused-word-cost (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{A parameter used in post-processing.}
    @short{For more information see the dictionary documentation and source
      code.}"))

(defgeneric link-cost (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{A parameter used in post-processing.}
    @short{For more information see the dictionary documentation and source
      code.}"))

(defgeneric corpus-cost (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{The total cost of this particular linkage, based on the cost of
      disjuncts stored in the corpus-statistics database.}
    @short{For more information see the dictionary documentation and source
      code.}"))

(defgeneric violation-name (instance)
  (:documentation
   "@arg[instance]{a @class{linkage}}
    @returns{A name of the violated rule as specified in the post-process
      knowledge file.}
    @short{If the linkage violated any post-processing rules, this slot will
      contain the name of the violated rule in the post-process knowledge
      file.}"))

;; ** defclass linkage

(closer-mop:defclass linkage (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle)
   (sentence :reader sentence
             :allocation :virtual
             :function link-sentence)
   (num-words :reader num-words
              :allocation :virtual
              :function link-num-words)
   (num-links :reader num-links
              :allocation :virtual
              :function link-num-links)
   (words :reader words
          :allocation :virtual
          :function link-words)
   (unused-word-cost :reader unused-word-cost
                     :allocation :virtual
                     :function link-unused-word-cost)
   (disjunct-cost :reader disjunct-cost
                  :allocation :virtual
                  :function link-disjunct-cost)
   (link-cost :reader link-cost
              :allocation :virtual
              :function link-link-cost)
   (corpus-cost :reader corpus-cost
                :allocation :virtual
                :function link-corpus-cost)
   (violation-name :reader violation-name
                   :allocation :virtual
                   :function link-violation-name))
  (:metaclass virtual-metaclass)
  (:documentation
   "@short{This class maps to @code{Linkage} C++ class.}

    It uses @code{virtual-metaclass} metaclass for all slots but the pointer to
    the object in C++ environment. This means that it doesn't store the
    information in its slots, rather it delegates the calls to slots to C++
    code.

    @see-slot{sentence}
    @see-slot{num-words}
    @see-slot{num-links}
    @see-slot{words}
    @see-slot{unused-word-cost}
    @see-slot{disjunct-cost}
    @see-slot{link-cost}
    @see-slot{corpus-cost}
    @see-slot{violation-name}
"))

;; ** virtual slot support functions

(defun link-sentence (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_get_sentence (handle linkage)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-num-words (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_get_num_words (handle linkage)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-num-links (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_get_num_links (handle linkage)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-words (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get
     (iter
       (with words := (linkage_get_words (handle linkage)))
       (for i :below (linkage_get_num_words (handle linkage)))
       (collect (mem-aref words :string i))))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-unused-word-cost (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_unused_word_cost (handle linkage)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-disjunct-cost (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_disjunct_cost (handle opts)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-link-cost (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_link_cost (handle opts)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-corpus-cost (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_corpus_cost (handle opts)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

(defun link-violation-name (linkage key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (linkage_get_violation_name (handle opts)))
    (:set (error "slot is read-only"))
    (:setp t)
    (:unset (values))))

;; ** constructor

(defun make-linkage (index sentence options)
  (let ((result (make-instance
                 'sentence
                 :handle (linkage_create
                          index (handle sentence) (handle options)))))
    (when (cffi:null-pointer-p (handle result))
      (error "Couldn't create sentence"))
    (tg:finalize
     result (lambda () (sentence_delete (handle result))))))

;; * dictionary definition

;; ** defclass dictionary
(closer-mop:defclass dictionary (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle)
   (language :allocation :virtual :function dict-language))
  (:metaclass virtual-metaclass))

(defun dict-language (dict key &optional value)
  (declare (ignore value))
  (ecase key
    (:get (dictionary_get_lang (handle dict)))
    (:set (error "Slot is read-only"))
    (:setp t)
    (:unset (values))))

;; * constructor

(defun make-dictionary (&optional language)
  (let ((result (make-instance 'dictionary :language language)))
    (when (cffi:null-pointer-p (handle result))
      (error "Couldn't create dictionary"))
    (tg:finalize
     result (lambda () (dictionary_delete (handle result))))))

;; * parse-options definition
;; ** slot documentation

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

(defgeneric disjunct-cost (instance)
  (:documentation
   "@arg[instance]{a @class{parse-options} or a @class{linkage}}
    @return{If instance is a @class{parse-options} -- the maximum
      number of links a parser will try to find in a sentence it
      parses. If instance is a @class{linkage} -- a parameter used in
      post-processing.}
    @short{Determines the maximum disjunct cost used during
    parsing, where the cost of a disjunct is equal to the maximum cost
    of all of its connectors. The default is that all disjuncts, no
    matter what their cost, are considered.

    For more information see the dictionary documentation and source code.}"))

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

;; ** defclass parse-options

(closer-mop:defclass parse-options (closer-mop:standard-object)
  ((handle :initarg :handle :accessor handle)
   (verbosity :initarg :verbosity
              :accessor :verbosity
              :allocation :virtual
              :function opts-verbosity)
   (linkage-limit :initarg :linkage-limit
                  :accessor linkage-limit
                  :allocation :virtual
                  :function opts-linkage-limit)
   (disjunct-cost :initarg :disjcunct-cost
                  :accessor disjunct_cost
                  :allocation :virtual
                  :function opts-disjunct-cost)
   (min-null-count :initarg :min-null-count
                   :accessor min-null-count
                   :allocation :virtual
                   :function opts-min-null-count)
   (max-null-count :initarg :max-null-count
                   :accessor max-null-count
                   :allocation :virtual
                   :function opts-max-null-count)
   (islandsp :initarg :islandsp
             :accessor islandsp
             :allocation :virtual
             :function opts-islandsp)
   (short-length :initarg :short-length
                 :accessor short-length
                 :allocation :virtual
                 :function opts-short-length)
   (max-memory :initarg :max-memory
               :accessor max-memory
               :allocation :virtual
               :function opts-max-memory)
   (max-parse-time :initarg :max-parse-time
                   :accessor max-parse-time
                   :allocation :virtual
                   :function opts-max-parse-time)
   (cost-model-type :initarg :cost-model-type
                    :accessor cost-model-type
                    :allocation :virtual
                    :function opts-cost-model-type)
   (display-morphology-p :initarg :display-morphology-p
                         :accessor display-morphology-p
                         :allocation :virtual
                         :function opts-display-morphology-p)
   (spell-guess-p :initarg :spell-guess-p
                  :accessor spell-guess-p
                  :allocation :virtual
                  :function opts-spell-guess-p)
   (all-short-connectors-p :initarg :all-short-connectors-p
                           :accessor all-short-connectors-p
                           :allocation :virtual
                           :function opts-short-connectors-p))
  (:metaclass virtual-metaclass)
  (:documentation
   "@short{This class maps to @code{Parse_Options} C++ class.}

    It uses @code{virtual-metaclass} metaclass for all slots but the pointer to
    the object in C++ environment. This means that it doesn't store the
    information in its slots, rather it delegates the calls to slots to C++
    code.

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

;; ** virtual slot support functions

(defun opts-verbosity (opts key &optional value)
  (ecase key
    (:get (parse_options_get_verbosity (handle opts)))
    (:set (parse_options_set_verbosity (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-linkage-limit (opts key &optional value)
  (ecase key
    (:get (parse_options_get_linkage_limit (handle opts)))
    (:set (parse_options_set_linkage_limit (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-disjunct-cost (opts key &optional value)
  (ecase key
    (:get (parse_options_get_disjunct_cost (handle opts)))
    (:set (parse_options_set_disjunct_cost (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-min-null-count (opts key &optional value)
  (ecase key
    (:get (parse_options_get_min_null_count (handle opts)))
    (:set (parse_options_set_min_null_count (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-max-null-count (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_null_count (handle opts)))
    (:set (parse_options_set_max_null_count (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-islandsp (opts key &optional value)
  (ecase key
    (:get (parse_options_get_islands_ok (handle opts)))
    (:set (parse_options_set_islands_ok (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-short-length (opts key &optional value)
  (ecase key
    (:get (parse_options_get_short_length (handle opts)))
    (:set (parse_options_set_short_length (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-max-memory (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_memory (handle opts)))
    (:set (parse_options_set_max_memory (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-max-parse-time (opts key &optional value)
  (ecase key
    (:get (parse_options_get_max_parse_time (handle opts)))
    (:set (parse_options_set_max_parse_time (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-cost-model-type (opts key &optional value)
  (ecase key
    (:get (parse_options_get_cost_model_type (handle opts)))
    (:set (parse_options_set_cost_model_type (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-display-morphology-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_display_morphology (handle opts)))
    (:set (parse_options_set_display_morphology (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-spell-guess-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_spell_guess (handle opts)))
    (:set (parse_options_set_spell_guess (handle opts) value))
    (:setp t)
    (:unset (values))))

(defun opts-short-connectors-p (opts key &optional value)
  (ecase key
    (:get (parse_options_get_all_short_connectors (handle opts)))
    (:set (parse_options_set_all_short_connectors (handle opts) value))
    (:setp t)
    (:unset (values))))

;; ** constructor

(defun make-parse-options (&key verbosity
                             linkage-limit
                             disjunct-cost
                             min-null-count
                             max-null-count
                             islandsp
                             short-length
                             max-memory
                             max-parse-time
                             cost-model-type
                             display-morphology-p
                             spell-guess-p
                             all-short-connectors-p)
  (let ((result (make-instance 'parse-options  :verbosity verbosity
                               :linkage-limit linkage-limit
                               :disjunct-cost disjunct-cost
                               :min-null-count min-null-count
                               :max-null-count max-null-count
                               :islandsp islandsp
                               :short-length short-length
                               :max-memory max-memory
                               :max-parse-time max-parse-time
                               :cost-model-type cost-model-type
                               :display-morphology-p display-morphology-p
                               :spell-guess-p spell-guess-p
                               :all-short-connectors-p all-short-connectors-p)))
    (when (cffi:null-pointer-p (handle result))
      (error "Couldn't create parse-options"))
    (tg:finalize
     result (lambda () (parse_options_delete (handle result))))))

;; * utility macros

(defun data-dir () (dictionary_get_data_dir))

(defmacro with-dictionary ((dictionary &optional language data-dir) &body body)
  "@arg[dictionary]{A symbol to bind to the @class{dictionary}, in addition to
      this symbol *dictionary* special variable will be also bound to the
      currently active dictionary}
   @arg[language]{Language code (optional)}
   @arg[data-dir]{Directory to look up for dictionary files (optional)}
   @arg[body]{Forms to bind the @code{dictionary} in}
   @return{The result of the last form of the @code{body}}

   @short{Use this macro to create and manage @class{dictionary} objects.}

   @see{dictionary}"
  (alexandria:with-gensyms (dir lang)
    `(let ((,lang ,language) ,dictionary)
       (unwind-protect
            (let ((,dir ,data-dir))
              (when ,dir
                (dictionary_set_data_dir
                 (translate-logical-pathname ,dir)))
              (setf ,dictionary
                    (make-instance
                     'dictionary
                     :handle
                     (if ,lang
                         (dictionary_create_lang ,lang)
                         (dictionary_create_default_lang)))
                    *dictionary* ,dictionary)
              ,@body)
         (when ,dictionary
           (dictionary_delete (handle ,dictionary))
           (setf *dictionary* nil))))))

(defmacro with-sentence ((sentence raw-input &optional
                                   (dictionary '*dictionary*)) &body body)
  (alexandria:with-gensyms (input)
    `(let ((,input ,raw-input) ,sentence)
       (unwind-protect
            (progn
              (setf ,sentence
                    (make-instance
                     'sentence
                     :handle (sentence_create ,input (handle ,dictionary))
                     :input ,input)
                    *sentence* ,sentence)
              ,@body)
         (when ,sentence
           (sentence_delete (handle ,sentence))
           (setf *sentence* nil))))))

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

;; * utility methods

(defmethod timer-expired-p ((this parse-options))
  (parse_options_timer_expired (handle this)))

(defmethod memory-exhausted-p ((this parse-options))
  (parse_options_memory_exhausted (handle this)))

(defmethod resource-exhausted-p ((this parse-options))
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

(defmethod linkages ((this sentence) &key (linkage-type :none))
  (let ((handle (handle this)))
    (case linkage-type
      (:none (sentence_num_linkages_found handle))
      (:valid (sentence_num_valid_linkages handle))
      (:post-processed (sentence_num_linkages_post_processed handle)))))

(defmethod num-violations ((this sentence) index)
  (sentence_num_violations (handle sentence) index))

(defmethod nth-disjunct-cost ((this sentence) index)
  (sentence_disjunct_cost (handle sentence) index))

(defmethod nth-link-cost ((this sentence) index)
  (sentence_link_cost (handle sentence) index))

(defmethod left-word ((this linkage) index)
  (linkage_get_link_lword (handle this) index))

(defmethod right-word ((this linkage) index)
  (linkage_get_link_rword (handle this)))

(defmethod link-length ((this linkage) index)
  (linkage_get_link_length (handle this) index))

(defmethod link-label ((this linkage) index &key (direction :none))
  (ecase direction
    (:none (linkage_get_link_label (handle this) index))
    (:left (linkage_get_link_llabel (handle this) index))
    (:right (linkage_get_link_rlabel (handle this) index))))

(defmethod num-domains ((this linkage) index)
  (linkage_get_link_num_domains (handle this)))

(defmethod domain-names ((this linkage) index)
  (linkage_get_link_domain_names (handle this) index))

;; * printing

(defmethod print-object ((this dictionary) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (format stream "~A" (dictionary_get_lang (handle this))))
  this)

(defmethod print-object ((this parse-options) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (with-slots (verbosity linkage-limit disjunct-cost min-null-count
                           max-null-count islandsp short-length max-memory
                           max-parse-time cost-model-type display-morphology-p
                           spell-guess-p all-short-connectors-p) this
      
      (format stream "{~{~A: ~A~^, ~}}"
              (list :verbosity verbosity
                    :linkage-limit linkage-limit
                    :disjunct-cost disjunct-cost
                    :min-null-count min-null-count
                    :max-null-count max-null-count
                    :islandsp islandsp
                    :short-length short-length
                    :max-memory max-memory
                    :max-parse-time max-parse-time
                    :cost-model-type cost-model-type
                    :display-morphology-p display-morphology-p
                    :spell-guess-p spell-guess-p
                    :all-short-connectors-p all-short-connectors-p))))
  this)

(defmethod print-object ((this sentence) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (with-slots (length null-count num-linkages-found
                        num-valid-linkages num-linkages-post-processed) this
      (format stream "~{~A~^, ~}"
              (list :length length
                    :null-count null-count
                    :num-linkages-found num-linkages-found
                    :num-valid-linkages num-valid-linkages
                    :num-linkages-post-processed num-linkages-post-processed))))
  this)

(defmethod print-object ((this linkage) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (format stream "~{~A~^, ~}" (words (handle this))))
  this)

(defmethod print-diagram ((this linkage)
                          &optional display-wallsp
                            (screen-wdith (linkage-print-length this)))
  "Creates a diagram which shows the parse of THIS linkage.
DISPLAY-WALLSP if true, will instruct to print the linkage margins (walls).
SCREEN-WDITH by default, will try to guess the space needed to print the linkage."
  (linkage_print_diagram (handle this) display-wallsp screen-wdith))

(defmethod linkage-print-length ((this linkage))
  (iter
    (for word :in (words this))
    (summing (1+ (cl:length word)) :into result)
    (finally (return (1- result)))))

(defmethod print-postscript ((this linkage)
                             &optional display-wallsp
                               (screen-wdith (linkage-print-length this)))
  (linkage_print_postscript (handle this) display-wallsp screen-wdith))

(defmethod print-links-and-domains ((this linkage))
  (linkage_print_links_and_domains (handle this)))

(defmethod print-senses ((this linkage))
  (linkage_print_senses (handle this)))

;; TODO: Investigate what does this tree look like and how exactly am I supposed
;; to free it.

(defmethod print-constituent-tree ((this linkage) &key (mode :no-display))
  "The function linkage_constituent_tree returns a pointer to a tree; after
using it the space should be freed-up with a call to
linkage_free_constituent_tree."
  (linkage_print_constituent_tree
   (handle this)
   (cffi:foreign-enum-value 'ConstituentDisplayStyle mode)))

