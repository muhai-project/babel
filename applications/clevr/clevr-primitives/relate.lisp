;;;; relate.lisp

(in-package :clevr-primitives)

;; ------------------
;; RELATE primtive ;;
;; ------------------

;(export '(relate))

(defgeneric apply-spatial-relation (object spatial-relation-category context)
  (:documentation "Apply the spatial relation to the object"))

(defmethod apply-spatial-relation ((object clevr-object)
                                   (spatial-relation-category spatial-relation-category)
                                   (context clevr-object-set))
  (let* ((related-ids (rest
                       (assoc (spatial-relation spatial-relation-category)
                              (relationships object))))
         (related-objects (loop for id in related-ids
                                for found = (find-entity-by-id context id)
                                when found
                                collect found)))
    (when related-objects
      (make-instance 'clevr-object-set :objects related-objects))))


(defprimitive relate ((target-set clevr-object-set)
                      (source-object clevr-object)
                      (segmented-scene clevr-object-set)
                      (scene pathname-entity)
                      (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((scene segmented-scene source-object spatial-relation => target-set)
   (let ((related-set (apply-spatial-relation
                       source-object
                       spatial-relation
                       segmented-scene)))
     (if related-set
       (bind (target-set 1.0 related-set))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))

  ;; second case; given source-object and target set, compute the spatial relation
  ((scene segmented-scene source-object target-set => spatial-relation)
   (let ((computed-relation
          (find-if #'(lambda (relation)
                       (equal-entity
                        target-set
                        (apply-spatial-relation source-object relation segmented-scene)))
                   (get-data ontology 'spatial-relations))))
     (when computed-relation
       (bind (spatial-relation 1.0 computed-relation)))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((scene segmented-scene source-object => target-set spatial-relation)
   (loop for relation in (get-data ontology 'spatial-relations)
         for set = (apply-spatial-relation source-object relation segmented-scene)
         when set
         do (bind (target-set 1.0 set)
                  (spatial-relation 1.0 relation))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((scene segmented-scene source-object target-set spatial-relation =>)
   (equal-entity target-set (apply-spatial-relation source-object spatial-relation segmented-scene)))
  :primitive-inventory *clevr-primitives*)
