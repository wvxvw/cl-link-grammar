;; -*- mode: lisp; package: link-grammar -*-

(in-package :link-grammar)

(defclass dictionary ()
  ((language :initarg :lang :accessor language)))

(defmethod initialize-instance :after ((this dictionary) &rest initargs)
  (destructuring-bind (&key lang) initargs
    ;; It looks like it's also might be necessary to call `setlocale'
    ;; to ensure the dictionary files are red properly.
    (setf (language this)
          (if lang
              (dictionary_create_lang lang)
              (dictionary_create_default_lang)))))

(defmethod data-dir () (dictionary_get_data_dir))

(defmacro with-dictionary (dic (&optional language data-dir) &body body)
  (let ((dic dic) (dir (gensym)))
    `(let ((,dir ,data-dir) ,dic)
       (unwind-protect
            (when ,dir
              (dictionary_set_data_dir
               (translate-logical-pathname ,dir)))
            (setf ,dic (make-instance 'dictionary :lang ,language))
         ,@body)
       (when ,dic (dictionary_delete (language ,dic))))))

(defun test-dictionary ()
  (with-dictionary foo ()
    (format t "~&~s" foo)))
