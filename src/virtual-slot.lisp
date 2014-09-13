;; -*- mode: lisp; package: link-grammar -*-
;; This code is copied from
;; http://www.lispworks.com/documentation/lw50/LWUG/html/lwuser-173.htm
;; with minor modifications.

(in-package :link-grammar)


(closer-mop:defclass virtual-metaclass (closer-mop:standard-class) ())

;; Mixin metaclass for virtual slots and methods to make them
;; appear virtual.

(closer-mop:defclass virtual-slot-definition
    (closer-mop:standard-slot-definition)
  ((function :initarg :function 
             :accessor virtual-slot-definition-function)))

(defmethod slot-definition-allocation ((slotd virtual-slot-definition))
  :virtual)

(defmethod (setf slot-definition-allocation) 
    (allocation (slotd virtual-slot-definition))
  (unless (eq allocation :virtual)
    (error "Cannot change the allocation of a ~S"
           'virtual-direct-slot-definition)) allocation)

;; Class of direct virtual slots and methods to construct them
;; when appropriate.

(closer-mop:defclass virtual-direct-slot-definition 
    (closer-mop:standard-direct-slot-definition
     virtual-slot-definition) ())

;; Called when the class is being made, to choose the metaclass of
;; a given direct slot. It should return the class of slot
;; definition required.

(defmethod closer-mop:direct-slot-definition-class
    ((class virtual-metaclass) &rest initargs)
  ;; Use virtual-direct-slot-definition if appropriate.
  (if (eq (getf initargs :allocation) :virtual)
      (find-class 'virtual-direct-slot-definition)
      (call-next-method)))

;; Class of effective virtual slots and methods to construct
;; them when appropriate.

(closer-mop:defclass virtual-effective-slot-definition 
    (closer-mop:standard-effective-slot-definition
     virtual-slot-definition) ())

;; Called when the class is being finalized, to choose the
;; metaclass of a given effective slot.  It should return the
;; class of slot definition required.

(defmethod closer-mop:effective-slot-definition-class 
    ((class virtual-metaclass) &rest initargs)
  ;; Use virtual-effective-slot-definition if appropriate.
  (let ((allocation (getf initargs :allocation)))
    (if (eql allocation :virtual)
        (find-class 'virtual-effective-slot-definition)
        (call-next-method))))

(defmethod closer-mop:compute-effective-slot-definition 
    ((class virtual-metaclass) name direct-slot-definitions)
  ;; Copy the function into the effective slot definition
  ;; if appropriate.
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd 'virtual-slot-definition)
        (setf (virtual-slot-definition-function effective-slotd) 
              (virtual-slot-definition-function slotd))
        (return)))
    effective-slotd))

;; Underlying access methods for invoking
;; virtual-slot-definition-function.

(defmethod closer-mop:slot-value-using-class 
    ((class virtual-metaclass) object slotd)
  (if (typep slotd 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slotd) object :get)
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class) 
    (value (class virtual-metaclass) object slotd)
  (if (typep slotd 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slotd)
               object :set value)
      (call-next-method)))

(defmethod closer-mop:slot-boundp-using-class 
    ((class virtual-metaclass) object slotd)
  (if (typep slotd 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slotd) object :setp)
      (call-next-method)))

(defmethod closer-mop:slot-makunbound-using-class 
    ((class virtual-metaclass) object slotd)
  (if (typep slotd 'virtual-slot-definition)
      (funcall (virtual-slot-definition-function slotd) object :unset)
      (call-next-method)))

(defmethod closer-mop:validate-superclass
    ((class virtual-metaclass)
     (superclass (eql (find-class 'closer-mop:standard-object)))) t)
