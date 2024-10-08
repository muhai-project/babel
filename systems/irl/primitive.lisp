(in-package :irl)

;; ############################################################################
;; primitive definition:
;; ----------------------------------------------------------------------------

(export '(primitive-p slot-count defprimitive => *irl-primitives*))

(defparameter *irl-primitives* nil)

(defclass primitive ()
  ((id
    :documentation "The identifier"
    :type symbol :initarg :id :reader id
    :initform (error "primitive requires :id"))
   (slot-specs
    :type list :initarg :slot-specs :reader slot-specs
    :initform (error "primitive requires :slot-specs")
    :documentation "The list of <slot-spec> specified for the primitive.
                    Each of these specifies the details of one of the slots of
                    the primitive.")
   (evaluation-specs
    :type list :initarg :evaluation-specs :reader evaluation-specs
    :initform (error ":evaluation-specs are required")
    :documentation "The evaluation-spec instances associated with this type."))
   (:documentation "Represents the 'type' of a primitive, i.e. all the information that is passed to defprimitive"))


(defmethod print-object ((p primitive) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "(~~primitive~~~:_ id: ~(~a~))" (id p)))
      (call-next-method)))

(defmethod copy-object ((primitive primitive))
  (let ((copy (make-instance
               'primitive :id (id primitive)
               :slot-specs (copy-object (slot-specs primitive))
               :evaluation-specs (copy-object (evaluation-specs primitive)))))
    copy))

(defun primitive-p (obj)
  (typep obj 'primitive))


(defun slot-count (primitive)
  "Return the number of slots of the primitive."
  (declare (type primitive primitive))
  (length (slot-specs primitive)))


(defmacro defprimitive (id slot-spec-defs &body body)
  "Macro for defining a primitive. Specify the name (id) of the primitive,
   the slot spec definitions (i.e. arguments and data types)
   and the body of the primtive, specifying the evaluation
   spec definitions. After the evaluation spec defs, the keyword
   primitive-inventory may be used to specify in which inventory
   this primitive should be added. Primitives can also be added
   to multiple primitive inventories at once by specifying a
   list of inventories."
  (let* ((inventory
          (if (find :primitive-inventory body)
            (nth (1+ (position :primitive-inventory body)) body)
            '*irl-primitives*))
         (evaluation-spec-defs
          (if (find :primitive-inventory body)
            (subseq body 0 (- (length body) 2))
            body))
         (p (gensym)))
    `(let ((,p (make-instance 'primitive :id ',id
                             :slot-specs (make-slot-specs ,slot-spec-defs)
                             :evaluation-specs ,(expand-evaluation-specs
                                                 id evaluation-spec-defs
                                                 slot-spec-defs))))
       ;; if multiple inventories are specified,
       ;; generate a 'progn' that calls 'add-primitive'
       ;; for every inventory in the list
       ,(if (listp inventory)
          `(progn ,@(loop for i in inventory
                          collect `(add-primitive ,p ,i)))
          `(add-primitive ,p ,inventory)))))

