(in-package :cle)

;; ----------------
;; + Construction +
;; ----------------
(defclass cxn ()
  ((id
    :initarg :id :accessor id :initform (make-id "CXN") :type symbol
    :documentation "Id of the construction.")
   (form
    :initarg :form :accessor form :initform nil :type string
    :documentation "Form of the construction.")
   (meaning
    :initarg :meaning :accessor meaning :initform nil :type concept
    :documentation "Meaning of the construction.")
   (score
    :initarg :score :accessor score :initform nil :type number))
  (:documentation "A construction is a mapping between a form and a meaning."))

(defgeneric make-cxn (agent object)
  (:documentation "Creates a new construction."))
  
(defmethod make-cxn (agent object form)
  (make-instance 'cxn
                 :form form
                 :meaning (make-concept agent object (get-configuration agent :concept-representation))
                 :score (get-configuration agent :initial-cxn-score)))


                                                                       
                                                                       

