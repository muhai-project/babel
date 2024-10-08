;;;; same.lisp

(in-package :clevr-primitives)

;; ----------------
;; SAME primtive ;;
;; ----------------

;(export '(same))

(defgeneric same-set-by-object-attribute (set object attribute)
  (:documentation "Filter the given set by the attribute of the given object;
   also remove the object itself from this set."))

(defmethod same-set-by-object-attribute ((set clevr-object-set)
                                         (object clevr-object)
                                         (attribute-category attribute-category))
  (let* ((object-attribute (case (attribute attribute-category)
                             (shape (shape object))
                             (size (size object))
                             (color (color object))
                             (material (material object))))
         (attribute-fn (case (attribute attribute-category)
                         (shape #'shape)
                         (size #'size)
                         (color #'color)
                         (material #'material)))
         (consider-set (remove (id object) (objects set) :key #'id))
         (same-set (loop for obj in consider-set
                         when (eq object-attribute (funcall attribute-fn obj))
                         collect obj)))
    (when same-set
      (make-instance 'clevr-object-set :objects same-set))))


(defprimitive same ((target-set clevr-object-set)
                    (source-object clevr-object)
                    (segmented-scene clevr-object-set)
                    (scene pathname-entity)
                    (attribute attribute-category))
  ;; first case; given source-object and attribute, compute the target-set
  ((scene segmented-scene source-object attribute => target-set)
   (let ((same-set (same-set-by-object-attribute segmented-scene source-object attribute)))
     (if same-set
       (bind (target-set 1.0 same-set))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))

  ;; second case; given source-object and target-set, compute the attribute
  ((scene segmented-scene source-object target-set => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-set
                        (same-set-by-object-attribute segmented-scene source-object attr)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-set
  ((scene segmented-scene source-object => target-set attribute)
   (loop for attr in (get-data ontology 'attributes)
         for set = (same-set-by-object-attribute segmented-scene source-object attr)
         if set
         do (bind (target-set 1.0 set)
                  (attribute 1.0 attr))
         else
         do (bind (target-set 1.0 (make-instance 'clevr-object-set
                                                 :id (make-id 'empty-set)))
                  (attribute 1.0 attr))))

  ;; fourth case; given source-object, attribute and target set,
  ;; check for consistency
  ((scene segmented-scene source-object attribute target-set =>)
   (equal-entity target-set (same-set-by-object-attribute
                             segmented-scene
                             source-object
                             attribute)))
  :primitive-inventory *clevr-primitives*)

