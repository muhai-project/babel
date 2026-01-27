(in-package :concept-representations)

;; -----------
;; + Entity +
;; -----------

(defclass entity ()
  ((id
    :initarg :id :accessor id :initform (make-id "ENTITY") :type symbol
    :documentation "Id of the entity.")
   (features
     :initarg :features :accessor features :type hash-table
     :documentation "Numerical or continuous features of the entity.")
   (description
    :initarg :description :accessor description :type hash-table
    :documentation "Symbolic descriptions of the entity"))
  (:documentation "Class for entity."))

(defmethod create-entity (features description)
  "Instantiates an entity given a set of features and descriptions."
  ;; converts 'string feature-values to 'symbol for efficient comparisons
  (maphash (lambda (feature-name feature-value)
            (when (stringp feature-value)
              (setf (gethash feature-name features)
                    (parse-keyword (gethash feature-name features)))))
          features)
  (make-instance 'entity :features features :description description))

(defmethod get-feature-value ((entity entity) (feature-name symbol))
  "Getter for value of a feature in an entity given the name."
  (gethash feature-name (features entity)))

(defun get-entity-id (entity)
  (gethash :id (description entity)))

(defun find-entity-by-equality (entity entities)
  (find entity
        entities
        :test #'equalp))

(defun find-entity-by-id (entity entities)
  (find (get-entity-id entity)
        entities
        :test #'equalp
        :key #'get-entity-id))
