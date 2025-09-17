(ql:quickload :concept-representations)
(in-package :concept-representations)

;; This script demonstrates how to use to
;;   create, compare, update and visualise
;;   multidimensional concepts.

;; DATALOADER => instantiating entities from a dataset
(defun create-path (dataset-name dataset-split)
  (merge-pathnames
   (make-pathname :directory `(:relative
                               "concept-emergence2" ;; ADAPT
                               "split-by-entities"  ;; ADAPT
                               ,dataset-name)
                  :name (format nil
                                "~a-~a"
                                dataset-name
                                dataset-split)
                  :type "jsonl")
   cl-user:*babel-corpora*))

(setf entities (load-dataset (create-path "winery" "train")))

(setf entity1 (first entities)
      entity2 (second entities)
      entity3 (third entities))


;; alternatively create mock entities, example:
#|
(progn
  (loop with features-ht = (make-hash-table)
        for feature-name in (list :height :width :color)
        ;; continuous features are floats between 0 and 1, 
        ;; categorical features are strings
        for feature-value in (list 0.0 0.0 "red") 
        do (setf (gethash feature-name features-ht) feature-value)
        finally (setf entity1 (create-entity features-ht (make-hash-table :test #'eq))))

  (loop with features-ht = (make-hash-table)
        for feature-name in (list :height :width :color)
        for feature-value in (list 1.0 1.0 "blue") 
        do (setf (gethash feature-name features-ht) feature-value)
        finally (setf entity2 (create-entity features-ht (make-hash-table :test #'eq))))

  (loop with features-ht = (make-hash-table)
        for feature-name in (list :height :width :color)
        for feature-value in (list 1.0 0.0 "blue") 
        do (setf (gethash feature-name features-ht) feature-value)
        finally (setf entity3 (create-entity features-ht (make-hash-table :test #'eq)))))
|#

;; CREATE => instantiating concepts based on an entity
(setf concept1 (create-concept-representation entity1 :weighted-multivariate-distribution))
(setf concept2 (create-concept-representation entity2 :weighted-multivariate-distribution))

;; COMPARE => calculating the similarity between two concepts
(concept-similarity concept1 concept1)
(concept-similarity concept1 concept2)

;; COMPARE => calculating the similarity between a concept and an entity
(concept-entity-similarity concept1 entity1)
(concept-entity-similarity concept1 entity2)

;; UPDATE => updating a concept towards an entity (that is embedded in a context of other entities)
(update-concept concept1 entity2 (list entity1 entity2 entity3))

;; VISUALISE => visualising a concept
(add-concept-to-interface concept1 :weight-threshold 0.1)

;; COPY-OBJECT
(copy-object concept1)