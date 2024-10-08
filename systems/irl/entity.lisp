
(in-package :irl)

;; ############################################################################
;; entity
;; ----------------------------------------------------------------------------

(export '(entity id equal-entity))

(defclass entity ()
  ((id
    :type symbol :initarg :id :accessor id
    :documentation "A symbol that should be unique within an ontology."))
  (:documentation "Everything that can be bound to a slot of a
                   primitive is an (semantic) entity."))

(defmethod initialize-instance :around ((entity entity) &rest initargs &key id)
  "when :id was not passed, make a new one based on the (sub)class of entity"
  (apply #'call-next-method entity :id (or id (internal-symb (make-id (type-of entity)))) initargs))

(defmethod print-object ((entity entity) stream)
  (format stream "<~(~a~) ~(~a~)>" (type-of entity) (id entity)))

(defmethod copy-object-content ((entity entity) (copy entity))
  (setf (id copy) (id entity)))

(defun irl-equal (a b)
  (or (eq a b) (eql a b)
      (eql (internal-symb a) (internal-symb b))))

(defun make-entity (id)
  (make-instance 'entity :id id))

(defgeneric equal-entity (value-1 value-2)
  (:documentation "Return true if the given entities are equal. This function is
      called while revising primitives in order to detect isomorphic values
      for re-use, which leads to increased efficiency."))

(defmethod equal-entity (entity-1 entity-2)
  (eq entity-1 entity-2))

(defmethod equal-entity ((entity-1 entity) (entity-2 entity))
  (irl-equal (id entity-1) (id entity-2)))

  

;; ############################################################################
;; find-entity-by-id
;; ----------------------------------------------------------------------------

(export 'find-entity-by-id)

(defgeneric find-entity-by-id (thing id &key &allow-other-keys)
  (:documentation "Finds an entity in thing by its id"))

(defmethod find-entity-by-id ((thing t) (id symbol) &key (type 'entity))
  (declare (ignore type))
  nil)

(defmethod find-entity-by-id ((entity entity) (id symbol) &key (type 'entity))
  (when (and (irl-equal (id entity) id)
             (typep entity type))
    entity))

(defmethod find-entity-by-id ((blackboard blackboard) (id symbol) &key (type 'entity))
  (loop for field in (data-fields blackboard)
        thereis (find-entity-by-id (cdr field) id :type type)))

(defmethod find-entity-by-id ((cons cons) (id symbol) &key (type 'entity))
  (if (and (typep (car cons) 'entity)
           (irl-equal (id (car cons)) id)
           (typep (car cons) type))
    (car cons)
    (or (find-entity-by-id (car cons) id :type type)
        (find-entity-by-id (cdr cons) id :type type))))




