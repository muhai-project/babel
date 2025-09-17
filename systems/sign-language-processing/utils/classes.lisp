(in-package :slp)

(defclass signed-form-predicates ()
  ((predicates
    :type list
    :accessor predicates
    :initform nil
    :initarg :predicates)))

(defmethod copy-object-content ((source signed-form-predicates) (copy signed-form-predicates))
  ;; shallow copy
  (setf (predicates copy) (predicates source)))