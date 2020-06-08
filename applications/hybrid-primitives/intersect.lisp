;;;; intersect.lisp

(in-package :hybrid-primitives)

;; ---------------------
;; INTERSECT primtive ;;
;; ---------------------

(defprimitive interact ((target-attn attention)
                        (source-attn-1 attention)
                        (source-attn-2 attention))
              )

(defprimitive intersect ((target-set clevr-object-set)
                         (source-set-1 clevr-object-set)
                         (source-set-2 clevr-object-set))
  ;; first case; given both source sets, compute the target set
  ((source-set-1 source-set-2 => target-set)
   (let ((intersected (intersection (objects source-set-1)
                                    (objects source-set-2)
                                    :key #'id)))
     (if intersected
       (bind (target-set 1.0 (make-instance 'clevr-object-set :objects intersected)))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))

  ;; second case; given a source and target set, compute the other source set
  ((source-set-1 target-set => source-set-2)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for intersected = (intersection (objects source-set-1)
                                           possible-set
                                           :key #'id)
           for intersected-set = (make-instance 'clevr-object-set :objects intersected)
           when (equal-entity target-set intersected-set)
           do (bind (source-set-2 1.0 intersected-set)))))

  ;; third case; given a source and target set, compute the other source set
  ((source-set-2 target-set => source-set-1)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for possible-set in (all-subsets (objects context))
           for intersected = (intersection (objects source-set-2)
                                           possible-set
                                           :key #'id)
            for intersected-set = (make-instance 'clevr-object-set :objects intersected)
           when (equal-entity target-set intersected-set)
           do (bind (source-set-1 1.0 intersected-set)))))

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-set-1 source-set-2 target-set =>)
   (let ((intersected (intersection (objects source-set-1)
                                    (objects source-set-2)
                                    :key #'id)))
     (equal-entity target-set
                   (make-instance 'clevr-object-set
                                  :objects intersected)))))