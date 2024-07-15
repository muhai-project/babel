(in-package :fcg)

(export '(anti-unify-sequences))

(defun anti-unify-sequences (pattern source &rest sequence-alignment-keyword-arguments)
  "Anti-unify sets of sequence predicates.
   1. Render all pattern sequences
   2. Render all source sequences
   3. Anti-unify all combinations of pattern-strings and source-strings
   4. Remove duplicate results
   5. Transform the result back to sequence predicates"
  ;; to do: improve anti-unify sequences to have full reversibility, up to the original variables.
  ;; note: the &rest argument sequence-alignment-keyword-arguments can be used to specify
  ;;       keyword arguments that will be passed on to #'maximal-sequence-alignments

  (multiple-value-bind (pattern-renders all-pattern-boundaries) (render-all pattern :render-sequences)
    (multiple-value-bind (source-renders all-source-boundaries) (render-all source :render-sequences)  
      (let* ((all-anti-unification-results
              (loop for pattern-render in pattern-renders
                    for pattern-boundaries in all-pattern-boundaries
                    for pattern-string = (list-of-strings->string pattern-render :separator "_")
                    append (loop for source-render in source-renders
                                 for source-string = (list-of-strings->string source-render :separator "_")
                                 for source-boundaries in all-source-boundaries
                                 append (loop with possible-alignments = (apply #'maximal-sequence-alignments
                                                                                pattern-string source-string
                                                                                pattern-boundaries source-boundaries
                                                                                sequence-alignment-keyword-arguments)
                                              for alignment in possible-alignments
                                              for pattern-in-alignment = (aligned-pattern alignment)
                                              for source-in-alignment = (aligned-source alignment)
                                              for pattern-boundaries = (aligned-pattern-boundaries alignment)
                                              for source-boundaries = (aligned-source-boundaries alignment)
                                              collect (multiple-value-bind (resulting-generalisation
                                                                            resulting-pattern-delta
                                                                            resulting-source-delta
                                                                            resulting-pattern-bindings
                                                                            resulting-source-bindings)
                                                          (anti-unify-aligned-sequences pattern-in-alignment source-in-alignment pattern-boundaries source-boundaries)
                                                        (let* ((simplified-sequences-generalisation (merge-adjacent-sequence-predicates resulting-generalisation))
                                                               (simplified-sequences-pattern-delta (merge-adjacent-sequence-predicates resulting-pattern-delta))
                                                               (simplified-sequences-source-delta (merge-adjacent-sequence-predicates resulting-source-delta))
                                                               (precedes-generalisation (calculate-precedes-predicates simplified-sequences-generalisation))
                                                               (precedes-pattern-delta (calculate-precedes-predicates simplified-sequences-pattern-delta))
                                                               (precedes-source-delta (calculate-precedes-predicates simplified-sequences-source-delta))
                                                               (au-result (make-instance 'sequences-au-result
                                                                                        :pattern pattern
                                                                                        :source source
                                                                                        :generalisation (append simplified-sequences-generalisation precedes-generalisation)
                                                                                        :pattern-delta (append simplified-sequences-pattern-delta precedes-pattern-delta)
                                                                                        :source-delta (append simplified-sequences-source-delta precedes-source-delta)
                                                                                        :pattern-bindings (remove-bindings-not-in-generalisation
                                                                                                           resulting-pattern-bindings
                                                                                                           simplified-sequences-generalisation)
                                                                                        :source-bindings (remove-bindings-not-in-generalisation
                                                                                                          resulting-source-bindings
                                                                                                          simplified-sequences-generalisation)
                                                                                        :alignment-cost (cost alignment))))
                                                          (setf (cost au-result) (anti-unification-cost au-result))
                                                          au-result))))))
             (unique-sorted-results
              (sort all-anti-unification-results #'< :key #'cost)))
        unique-sorted-results))))

(defun calculate-precedes-predicates (sequence-predicates)
  "Return precedes constraints on the sequence predicates based on the order of the predicates"
  (let ((left-boundaries (mapcar #'third sequence-predicates))
        (right-boundaries (mapcar #'fourth sequence-predicates)))
    (loop for left-boundary in (rest left-boundaries)
          for right-boundary in  right-boundaries
          collect `(precedes ,right-boundary ,left-boundary))))



;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the cube" ?l1 ?r1)) '((sequence "what color is the cube" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "onelittle" ?l1 ?r1)) '((sequence "twolittle" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the red cube" ?l1 ?r1)) '((sequence "the blue cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the cube" ?l1 ?r1)) '((sequence "the red cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "the red cube" ?l1 ?r1)) '((sequence "the cube" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "ABA" ?l1 ?r1)) '((sequence "A" ?l2 ?r2))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "A" ?l1 ?r1) (sequence "BA" ?l2 ?r2)) '((sequence "A" ?l3 ?r3))))

;;(print-anti-unification-results (anti-unify-sequences '((sequence "A" ?l1 ?r1) (sequence "BA" ?l2 ?r2)) '((sequence "AB" ?l3 ?r3))))

;; (print-anti-unification-results (anti-unify-sequences '((sequence "AA" ?l1 ?r1)) '((sequence "ABA" ?l2 ?r2))))

;; what do we want from reversibility check in this case? 
;; (print-anti-unification-results (anti-unify-sequences '((sequence "the " ?l1 ?r1) (sequence "orange" ?r1 ?l2) (sequence " cube" ?l2 ?r2)) '((sequence "the yellow cube" ?l3 ?r3))))


;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the block?" ?l6 ?r6)) '((sequence "what size is the sphere?" ?l7 ?r7))))
;;(print-anti-unification-results (anti-unify-sequences '((sequence "what size is the cube?" ?l6 ?r6)) '((sequence "what color is the cube?" ?l7 ?r7))))

(defun anti-unify-aligned-sequences (pattern source pattern-boundaries source-boundaries)
  "Takes Needleman-Wunsch ALIGNED sequences and outputs a generalisation with bindings (deltas)"
  (let (generalisation pattern-delta source-delta
        pattern-bindings source-bindings)
    (loop for pattern-char in pattern
          for source-char in source
          for pattern-bounds in pattern-boundaries
          for source-bounds in source-boundaries
          do (if (eql pattern-char source-char)
               ;; same chars, put them in generalisation, rename variables
               (let* ((renamed-left-gen-boundary (rename-boundary (car pattern-bounds) (car source-bounds) pattern-bindings source-bindings))
                      (renamed-right-gen-boundary (rename-boundary (cdr pattern-bounds) (cdr source-bounds) pattern-bindings source-bindings))
                      (gen `(sequence ,(mkstr pattern-char) ,renamed-left-gen-boundary ,renamed-right-gen-boundary)))
                 (if (not (equal pattern-char #\_)) (push gen generalisation))
                 (setf pattern-bindings
                       (adjoin (cons (car pattern-bounds) renamed-left-gen-boundary) pattern-bindings :test 'equal))
                 (setf pattern-bindings
                       (adjoin (cons (cdr pattern-bounds) renamed-right-gen-boundary) pattern-bindings :test 'equal))
                 (setf source-bindings
                       (adjoin (cons (car source-bounds) renamed-left-gen-boundary) source-bindings :test 'equal))
                 (setf source-bindings
                       (adjoin (cons (cdr source-bounds) renamed-right-gen-boundary) source-bindings :test 'equal)))
               ;; different characters, push to delta, only if char is not #'_
               (let ((pattern-delta-pred `(sequence ,(mkstr pattern-char) ,(car pattern-bounds) ,(cdr pattern-bounds)))
                     (source-delta-pred `(sequence ,(mkstr source-char) ,(car source-bounds) ,(cdr source-bounds))))
                 (if (not (equal pattern-char #\_)) (push pattern-delta-pred pattern-delta))
                 (if (not (equal source-char #\_)) (push source-delta-pred source-delta)))))
    (setf pattern-bindings (loop for binding in pattern-bindings if (car binding) collect binding))
    (setf source-bindings (loop for binding in source-bindings if (car binding) collect binding))
    (values (reverse generalisation)
            (reverse pattern-delta)
            (reverse source-delta)
            (reverse pattern-bindings)
            (reverse source-bindings))))


(defun rename-boundary (pattern-boundary source-boundary pattern-bindings source-bindings)
  (let ((gen-var-pattern (cdr (assoc pattern-boundary pattern-bindings)))
        (gen-var-source (cdr (assoc source-boundary source-bindings))))
  (if (and gen-var-pattern gen-var-source
       (equal
        gen-var-pattern
        gen-var-source))
    (cdr (assoc pattern-boundary pattern-bindings))
    (make-var 'gb))))

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defun remove-bindings-not-in-generalisation (bindings generalisation-list)
  (let ((generalisation-vars (loop for seq in generalisation-list
                                   append (list (third seq) (fourth seq)))))
  (loop for binding in bindings
          if (find (cdr binding) generalisation-vars)
          collect binding)))

(defun make-boundary-vars (position boundaries current-left &key (gap nil))
  (let ((right-boundary-var (if position  (car (rassoc position  boundaries))))
        (left-boundary-var (if position (car (rassoc (- position 1) boundaries)))))
    (cond ((and (not right-boundary-var) (not current-left))
           (setf right-boundary-var (make-var 'rb)))
          ((and (not right-boundary-var) current-left)
           (setf right-boundary-var current-left)))
    (if (not left-boundary-var)
      (setf left-boundary-var (if gap current-left (make-var 'lb))))
    (cons left-boundary-var right-boundary-var)))

(defun print-sequence-alignments (list-of-sequence-alignment-states &optional (stream t))
  (format t "~%~%~%### Maximal Sequence Alignment ###~%")
  (format stream "----------------------------------~%~%")
  (loop for alignment-state in list-of-sequence-alignment-states
        for i from 1
        for symbols = nil
        do (loop for x in (aligned-pattern alignment-state)
                 for y in (aligned-source alignment-state)
                 do (cond ((eql x #\_) (push #\Space symbols))
                          ((eql y #\_) (push #\Space symbols))
                          ((eql x y) (push #\| symbols))
                          ((not (eql x y)) (push #\. symbols))))
           (format stream "--- Result ~a (cost: ~a) ---~%~%" i (cost alignment-state))
           (format t "~s~%" (coerce (aligned-pattern alignment-state) 'string))
           (format t "~s~%" (coerce (reverse symbols) 'string))
           (format t "~s~%~%" (coerce (aligned-source alignment-state) 'string))))

;;;;;;;;;;
;; Cost ;;
;;;;;;;;;;

(defmethod anti-unification-cost ((au-result sequences-au-result))
  "Cost of a string anti-unification result."
  ;; TO DO: determine cost
  (+ (length (pattern-delta au-result))
     (length (source-delta au-result))))


#|
(defmethod anti-unification-cost ((au-result sequences-au-result))
  "Cost of a string anti-unification result."
  ;; a sequence generalisation is better when
  ;; (i) there are fewer predicates in the delta (== fewer gaps in the generalisation)
  ;; (ii) there are more characters in the generalisation
  (let ((chars-in-pattern (loop for seq-pred in (pattern au-result)
                                sum (length (second seq-pred))))
        (chars-in-source (loop for seq-pred in (source au-result)
                               sum (length (second seq-pred))))
        (chars-in-generalisation (loop for seq-pred in (generalisation au-result)
                                       sum (length (second seq-pred)))))
    (+ (length (pattern-delta au-result))
       (length (source-delta au-result))
       (- chars-in-pattern chars-in-generalisation)
       (- chars-in-source chars-in-generalisation))))


(defmethod anti-unification-cost ((au-result sequences-au-result))
  "Cost of a string anti-unification result."
  ;; bigger chunks of text in the generalisation is better
  ;; so the cost is inversely proportional to the length of the strings?
  ;; also, we want as few gaps as possible, so the length of the deltas
  (float
   (+ (/ (loop for seq-pred in (generalisation au-result)
               sum (/ 1 (length (second seq-pred))))
         (length (generalisation au-result)))
      (/ (loop for seq-pred in (pattern-delta au-result)
               sum (/ 1 (length (second seq-pred))))
         (length (pattern-delta au-result)))
      (/ (loop for seq-pred in (source-delta au-result)
               sum (/ 1 (length (second seq-pred))))
         (length (source-delta au-result)))
      (length (generalisation au-result))
      (length (pattern-delta au-result))
      (length (source-delta au-result)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reversibility check ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

#|(defun merge-adjacent-sequence-predicates (sequence-predicates)
  "When there are adjacent sequence predicates, merge them together."
  (loop for predicate in sequence-predicates
        for left-bound = (third predicate)
        for right-bound = (fourth predicate)
        for adjacent-left = (find left-bound sequence-predicates :key #'fourth)
        for adjacent-right = (find right-bound sequence-predicates :key #'third)
        if adjacent-left
          return (let* ((new-predicate `(sequence ,(mkstr (second adjacent-left) (second predicate))
                                                  ,(third adjacent-left) ,(fourth predicate)))
                        (updated-predicates
                         (cons new-predicate (remove adjacent-left (remove predicate sequence-predicates :test #'equal) :test #'equal))))
                   (merge-adjacent-sequence-predicates updated-predicates))
        else if adjacent-right
           return (let* ((new-predicate `(sequence ,(mkstr (second predicate) (second adjacent-right))
                                                   ,(third predicate) ,(fourth adjacent-right)))
                         (updated-predicates
                          (cons new-predicate (remove adjacent-right (remove predicate sequence-predicates :test #'equal) :test #'equal))))
                    (merge-adjacent-sequence-predicates updated-predicates))
        finally (return sequence-predicates)))|#


(defun merge-adjacent-sequence-predicates (sequence-predicates)
  "Merge individual sequence predicates where they share boundaries."

  (loop with current-simplified-string = nil
        with current-simplified-sequence-predicate = nil
        with result = nil
        for (nil string left-boundary right-boundary) in sequence-predicates
        for i from 1 
        for next-predicate = (when (<= i (length sequence-predicates))
                               (nth i sequence-predicates))
        if next-predicate
          do
            (cond ((and (equalp right-boundary (third next-predicate)) ;; boundaries coincide
                        (null current-simplified-string)) ;;start new merged string
                   (setf current-simplified-string string)
                   (setf current-simplified-sequence-predicate `(sequence ,current-simplified-string ,left-boundary ,right-boundary)))
                  
                  ((and (equalp right-boundary (third next-predicate))  ;; boundaries coincide
                        current-simplified-string)
                   (setf current-simplified-string (string-append current-simplified-string string))
                   (setf current-simplified-sequence-predicate `(sequence ,current-simplified-string ,(third current-simplified-sequence-predicate) ,right-boundary)))
                  
                  ((null (equalp right-boundary (third next-predicate)))  ;; boundaries do not coincide
                   (if current-simplified-string
                     (progn 
                       (setf result (append result (list `(sequence ,(string-append current-simplified-string string)
                                                                ,(third current-simplified-sequence-predicate) ,right-boundary))))
                       (setf current-simplified-string nil)
                       (setf current-simplified-sequence-predicate nil))
                     (setf result (append result (list `(sequence ,string ,left-boundary ,right-boundary)))))))

        else ;; we reached the end of the sequence-predicates list
          do (if current-simplified-string
                  (setf result (append result (list `(sequence ,(string-append current-simplified-string string)
                                                               ,(third current-simplified-sequence-predicate) ,right-boundary))))
                  (setf result (append result (list `(sequence ,string ,left-boundary ,right-boundary)))))
                
        finally (return result))
        )

;;(merge-adjacent-sequence-predicates '((SEQUENCE "A" #:?GB-19759597 #:?GB-19759598) (SEQUENCE "r" #:?GB-19759598 #:?GB-19759599) (SEQUENCE "e" #:?GB-19759599 #:?GB-19759600) (SEQUENCE " " #:?GB-19759600 #:?GB-19759601) (SEQUENCE "t" #:?GB-19759601 #:?GB-19759602) (SEQUENCE "h" #:?GB-19759602 #:?GB-19759603) (SEQUENCE "e" #:?GB-19759603 #:?GB-19759604) (SEQUENCE "r" #:?GB-19759604 #:?GB-19759605) (SEQUENCE "e" #:?GB-19759605 #:?GB-19759606) (SEQUENCE " " #:?GB-19759606 #:?GB-19759607) (SEQUENCE "a" #:?GB-19759607 #:?GB-19759608) (SEQUENCE "n" #:?GB-19759608 #:?GB-19759609) (SEQUENCE "y" #:?GB-19759609 #:?GB-19759610) (SEQUENCE " " #:?GB-19759610 #:?GB-19759611) (SEQUENCE "s" #:?GB-19759612 #:?GB-19759613) (SEQUENCE "?" #:?GB-19759613 #:?GB-19759614)))


#|
  (loop for predicate in sequence-predicates
        for left-bound = (third predicate)
        for right-bound = (fourth predicate)
        for adjacent-left = (find left-bound sequence-predicates :key #'fourth)
        for adjacent-right = (find right-bound sequence-predicates :key #'third)
        if adjacent-left
          return (let ((new-predicate `(sequence ,(mkstr (second adjacent-left) (second predicate))
                                                 ,(third adjacent-left) ,(fourth predicate)))
                       (copied-sequence-predicates (copy-list sequence-predicates)))
                   (setf (nth (position predicate copied-sequence-predicates) copied-sequence-predicates) new-predicate)
                   (merge-adjacent-sequence-predicates (remove adjacent-left copied-sequence-predicates :test #'equal)))
        else if adjacent-right
           return (let ((new-predicate `(sequence ,(mkstr (second predicate) (second adjacent-right))
                                                  ,(third predicate) ,(fourth adjacent-right)))
                        (copied-sequence-predicates (copy-list sequence-predicates)))
                    (setf (nth (position predicate copied-sequence-predicates) copied-sequence-predicates) new-predicate)
                    (merge-adjacent-sequence-predicates (remove adjacent-right copied-sequence-predicates :test #'equal)))
        finally (return sequence-predicates))) |# 
                  

;;



(defmethod compute-network-from-anti-unification-result ((au-result sequences-au-result) pattern-or-source)
  "Returns original network based on generalisation, bindings-list and delta."  
  (let* ((generalisation (generalisation au-result))
         (bindings-key (case pattern-or-source
                         (pattern #'pattern-bindings)
                         (source #'source-bindings)))
         (delta-key (case pattern-or-source
                      (pattern #'pattern-delta)
                      (source #'source-delta)))
         (bindings-list (funcall bindings-key au-result))
         (delta (funcall delta-key au-result)))
    (merge-adjacent-sequence-predicates
     (append (substitute-bindings (reverse-bindings bindings-list) generalisation)
             delta))))