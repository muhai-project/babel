(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;
;; Extraction     ;;
;;;;;;;;;;;;;;;;;;;;

(defun extract-frames (transient-structure &key (include-frames-with-var-frame-name? nil))
  "Extracting a frameset from a transient-structure."
  (loop with unit-list = (left-pole-structure transient-structure)
        for unit in unit-list
        when (and (find '(frame-evoking +) (unit-body unit) :test #'equalp)
                  (or include-frames-with-var-frame-name?
                      (null (variable-p (find-frame-name unit)))))
        collect (make-instance 'frame
                               :frame-name (find-frame-name unit)
                               :frame-evoking-element (find-frame-evoking-element unit)
                               :frame-elements (find-frame-elements unit unit-list))
        into frames
        finally
        (return (make-instance 'frame-set :frames frames))))

(defun find-frame-name (unit)
  "Find frame name in unit."
  (let ((meaning (find 'meaning (unit-body unit) :key #'feature-name)))
    (loop for predicate in (feature-value meaning)
          when (equalp (first predicate) 'frame)
          return (second predicate))))

(defun find-frame-evoking-element (unit)
  "Find frame evoking element in unit."
  (let ((fee-indices (second (find 'span (unit-body unit) :key #'feature-name))))
    (make-instance 'frame-evoking-element
                   :fel-string (second (find 'string (unit-body unit) :key #'feature-name))
                   :indices (if (= (reduce #'- (reverse fee-indices)) 1)
                              (list (first fee-indices))
                              (loop for i from 0
                                    for index in fee-indices
                                    collect (if (= i 0)
                                              index
                                              (- index 1)))
                              )
                 :lemma (second (find 'lemma (unit-body unit) :key #'feature-name)))))

(defun find-frame-elements (unit unit-list)
  "Find frame elements in transient structure."
  (let ((fe-predicates (loop for predicate in (feature-value (find 'meaning (unit-body unit) :key #'feature-name))
                             when (equalp (first predicate) 'frame-element)
                             collect predicate)))
    (loop for predicate in fe-predicates
          for fe-consituent-unit = (find (fourth predicate) unit-list :key #'feature-name)
          collect
          (make-instance 'frame-element
                 :fe-name (second predicate)
                 :fe-role (second predicate)
                 :fe-string (second (find 'string (unit-body fe-consituent-unit) :key #'feature-name))
                 :fe-lemmas (if (find 'constituents (unit-body fe-consituent-unit) :key #'feature-name)
                              (find-constituents-lemmas (second (find 'constituents (unit-body fe-consituent-unit) :key #'feature-name)) unit-list)
                              (second (find 'lemma (unit-body fe-consituent-unit) :key #'feature-name)))
                 :indices (when (second (second (find 'span (unit-body fe-consituent-unit) :key #'feature-name)))
                            (loop for i
                                  from (first (second (find 'span (unit-body fe-consituent-unit) :key #'feature-name)))
                                  to (- (second (second (find 'span (unit-body fe-consituent-unit) :key #'feature-name))) 1)
                                  collect i))))))

(defun find-constituents-lemmas (unit-names unit-list)
  (loop for unit-name in unit-names
        for unit = (find unit-name unit-list :key #'first :test #'equal)
        for lemma = (find 'lemma unit :key #'feature-name)
        if lemma
          collect (second (find 'lemma unit :key #'feature-name))
        else
          append (find-constituents-lemmas (second (find 'constituents (find unit-name unit-list :key #'feature-name) :key #'feature-name)) unit-list)))

#|(loop for unit-name in (second (find 'constituents (unit-body fe-consituent-unit) :key #'feature-name))
                                  for unit = (find unit-name unit-list :key #'first :test #'equal)
                                  collect (second (find 'lemma unit :key #'feature-name)))|#