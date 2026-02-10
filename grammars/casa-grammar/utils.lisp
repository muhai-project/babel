(in-package :fcg)

(defun extract-frames-from-utterance (utterance &key (cxn-inventory *fcg-constructions*)  (silent nil))
  "Extract the frame semantic analysis from the solution node of the comprehension process."

  (multiple-value-bind (meaning cip-solution)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent)
    (declare (ignore meaning))

    (let ((unit-structure (loop for unit in (left-pole-structure (fcg-get-transient-structure cip-solution))
                                 when (equal (unit-feature-value unit 'node-type) 'leaf)
                                   collect unit)))
      (extract-frames-from-ts unit-structure))))

(defun reconstruct-utterance-from-unit-structure-with-spans (unit-structure)
  (let ((ordered-units (sort unit-structure #'< :key #'(lambda (unit) (first (unit-feature-value unit 'span))))))
    (mapcar #'(lambda (unit) (unit-feature-value unit 'string)) ordered-units)))
  

(defun extract-frames-from-ts (unit-structure)
  (loop for unit in unit-structure
        when (unit-feature unit 'frame-evoking)
        collect (extract-frame-from-ts unit unit-structure)))


(defun extract-frame-from-ts (fee-unit unit-structure)
  (multiple-value-bind (slots frame-name)
      (loop with frame-name = 'TBD
            for predicate in (unit-feature-value fee-unit 'meaning)
            when (search "ARG" (symbol-name (first predicate)) :end1 3)
              collect (cons (first predicate) (last-elt predicate)) into slots
            else do (setf frame-name (first predicate))
            finally (return (values slots frame-name)))
    (loop for slot in slots
          collect (list (first slot)
                        (reconstruct-frame-element (cdr slot) unit-structure))
          into frame-slots
          finally (return (cons frame-name
                                (append (list (cons 'fee (reconstruct-utterance-from-unit-structure-with-spans
                                                    (reconstruct-frame-evoking-element fee-unit unit-structure))))
                                        (mapcar #'(lambda(slot)
                                                    (cons (first slot)
                                                          (reconstruct-utterance-from-unit-structure-with-spans (second slot))))
                                                frame-slots)))))))


(defun reconstruct-frame-evoking-element (fee-unit unit-structure)
  (let* ((frame-var (first (unit-feature-value fee-unit 'meaning-args)))
         (other-fee-units (loop for u in (remove fee-unit unit-structure :test #'equal)
                                if (and (unit-feature-value u 'part-of-fee)
                                        (string= (unit-feature-value u 'part-of-fee) frame-var))
                                  collect u)))
    (append (list fee-unit) other-fee-units)))

(defun reconstruct-frame-element (referent unit-structure)
  (let* ((fe-units (loop for unit in unit-structure
                         for referent-var =  (first (unit-feature-value unit 'meaning-args))
                         if (and referent-var (string= referent-var referent))
                         do (return (collect-non-referent-dependents unit unit-structure :top-unit t)))))
    fe-units))

    
(defun collect-non-referent-dependents (unit unit-structure &key (top-unit t))
  "Returns all subunits of a given unit that do not have a referent feature (including the unit itself)"
  (cond ((and (not (unit-feature-value unit 'dependents))
              (not (unit-feature-value unit 'part-of-fee)))
             ;; (not (and (unit-feature-value unit 'meaning-args)
              ;;          (null top-unit))))
         (list unit))
        ((not (or (unit-feature-value unit 'part-of-fee)
                  (and (unit-feature-value unit 'meaning-args)
                       (null top-unit))))
         (append (list unit) (loop for unit-name in (unit-feature-value unit 'dependents)
                                   for dep-unit = (assoc unit-name unit-structure)
                                   append (collect-non-referent-dependents dep-unit unit-structure
                                                                            :top-unit nil))))))