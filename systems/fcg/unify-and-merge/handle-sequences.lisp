(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;; Functionality implementing sequence-based form representation ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fcg-expand ((type (eql :handle-regex-sequences))
                       &key value source bindings merge? cxn-inventory)
  "Regex match cxn form sequences in root form sequences and merge new bindings into bindings list."
  (if merge?
    (let ((precedes-predicates (find-all-if #'(lambda (predicate)
                                                  (eql (first predicate) 'precedes)) (rest value))))
      (if precedes-predicates
        (if (loop for (nil left right) in precedes-predicates
                  when (and (numberp left)
                            (numberp right)
                            (> left right))
                    do (return nil)
                  finally (return t))
          (values value bindings)
          (values nil +fail+))
        (values value bindings)))
    ;; Regex match pattern in source and return all possible new bindings lists
    (let ((sequence-predicates (remove-if-not #'(lambda (predicate)
                                                  (eql (first predicate) 'sequence)) (rest value))))
      (if sequence-predicates
        (let ((sequence-bindings-lists (match-pattern-sequence-predicates-in-source-sequence-predicates sequence-predicates source bindings cxn-inventory)))
          (if sequence-bindings-lists
            ;; If sequence predicates could be matched, add bindings  
            (values source (merge-bindings-lists bindings sequence-bindings-lists))
            ;; Fail if sequence predicates could not be matched
            nil))
        ;; If there are no sequence predicates (but only e.g. precedes constraints), continue and deal with it in merge (above).
        (values source bindings)))))


(export '(filter-valid-sequence-bindings-lists))

(defun match-pattern-sequence-predicates-in-source-sequence-predicates (pattern-sequence-predicates source-sequence-predicates bindings &optional cxn-inventory)
  "Matches two sets of sequence-predicates, returns validated bindings-lists with all options. Takes into account existing bindings."
  (when source-sequence-predicates
    (loop for pattern-sequence-predicate in pattern-sequence-predicates
          collect (match-pattern-sequence-predicate-in-source-sequence-predicates pattern-sequence-predicate source-sequence-predicates cxn-inventory)
            into possible-bindings-lists-per-pattern-predicate
          finally (return (loop for list-of-lr-pairs in (filter-valid-sequence-bindings-lists
                                                         (apply #'cartesian-product possible-bindings-lists-per-pattern-predicate) bindings)
                                collect (loop for lr-pair in  list-of-lr-pairs
                                              append lr-pair))))))

(defun match-pattern-sequence-predicate-in-source-sequence-predicates (pattern-sequence-predicate source-sequence-predicates cxn-inventory)
  "Matches a sequence-predicate against a list of sequence-predicates, returns unfiltered potential bindings lists."
  (loop for source-sequence-predicate in source-sequence-predicates
        append (match-sequence-predicates pattern-sequence-predicate source-sequence-predicate cxn-inventory)))

(defun escape-re-string (re-string)
  "Escapes regex special characters from literal string that will be used as regex."
  (loop with pattern-string = (copy-seq re-string)
        for special-string in '("?" "(" ")" "/" "+" "*")
        when (search special-string pattern-string)
          do (setf pattern-string (replace-all pattern-string special-string (string-append "\\" special-string)))
        finally (return pattern-string)))

(defun retrieve-or-create-re-scanner (re-string cxn-inventory)
  "If a regex scanner for re-string exists in the blackboard of the construction-inventory, retrieve it.
Otherwise create a scanner and store it."
  (let* ((re-scanners (or ;; retrieve re-scanners from cxn-inventory if the data-field exists
                          (find-data (blackboard cxn-inventory) :re-scanners)
                          ;; otherwise create the data-field
                          (progn
                            (add-data-field (blackboard cxn-inventory) :re-scanners (make-hash-table :test #'equal))
                            (get-data (blackboard cxn-inventory) :re-scanners)))))
    
    (or ;; Return the scanner if it is exists
        (gethash re-string re-scanners)
        ;; Create, add and return if it doesn't
        (setf (gethash re-string re-scanners) (cl-ppcre:create-scanner (escape-re-string re-string))))))
                      
(defun match-sequence-predicates (pattern-predicate source-predicate cxn-inventory)
  "Matches two sequence-predicates, returns unfiltered potential bindings lists."
  (let* ((pattern-string (second pattern-predicate))
         (regex-scanner (when cxn-inventory (retrieve-or-create-re-scanner pattern-string cxn-inventory)))
         (source-string (second source-predicate))
         (index-offset (third source-predicate)))
    (when pattern-string
      (let* ((all-match-positions (cl-ppcre:all-matches (or regex-scanner (escape-re-string pattern-string)) source-string))
             (possible-bindings (loop for (left right) on all-match-positions by #'cddr
                                      collect (list (+ left index-offset) (+ right index-offset)))))
        (loop for pb in possible-bindings
              collect (list (make-binding (third pattern-predicate) (first pb))
                            (make-binding (fourth pattern-predicate) (second pb))))))))


(defun filter-valid-sequence-bindings-lists (list-of-possible-bindings-lists bindings)
  "Only returns bindings-lists that contain no overlaps and are consistent with the bindings provided."
  (loop with filtered-bindings-lists = (remove-if-not #'non-overlapping-lr-pairs-list-p list-of-possible-bindings-lists)
        for list-of-lr-pairs in filtered-bindings-lists
        for valid-list-of-lr-pairs = (loop for lr-pair in list-of-lr-pairs
                                           if (lr-pair-consistent-with-bindings-p lr-pair bindings)
                                             collect lr-pair
                                           else return nil)
        when valid-list-of-lr-pairs
          collect valid-list-of-lr-pairs))


(defun lr-pair-consistent-with-bindings-p (lr-pair bindings-lists)
  "Checks whether the lr-pair is consistent with the bindings already present in bindings-lists."
  (loop for bindings-list in bindings-lists
        always (merge-bindings lr-pair bindings-list)))


(defun non-overlapping-lr-pairs-list-p (lr-pairs-list)
  "Returns t if all lr-pairs are non-overlapping."
  (loop for lr-pair in lr-pairs-list
        always (loop for other-lr-pair in (remove lr-pair lr-pairs-list :test #'equal)
                     always (non-overlapping-lr-pairs-p lr-pair other-lr-pair))))


(defun non-overlapping-lr-pairs-p (lr-pair-1 lr-pair-2)
  "Two left-right pairs do not overlap if the left index of one is larger than or equal to the right-index of the other."
  (let ((l-1-index (cdr (first lr-pair-1)))
        (r-1-index (cdr (second lr-pair-1)))
        (l-2-index (cdr (first lr-pair-2)))
        (r-2-index (cdr (second lr-pair-2))))
    (or (>= l-1-index r-2-index)
        (>= l-2-index r-1-index))))


(defun overlapping-intervals-p (interval-1 interval-2)
  "Two left-right pairs overlap if the left index of one is larger than or equal to the right-index of the other."
  (let ((l-1-index (first interval-1))
        (r-1-index (second interval-1))
        (l-2-index (first interval-2))
        (r-2-index (second interval-2)))
    (null (or (>= l-1-index r-2-index)
              (>= l-2-index r-1-index)))))
  


;; Recomputing sequence predicates that will be put in the updated form feature of the root unit ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun recompute-root-sequence-features-based-on-bindings (pattern-form-predicates source-form-predicates bindings)
  "Makes new set of sequence predicates based on the indices that are present in the bindings."

  (let ((matched-positions (retrieve-matched-positions pattern-form-predicates bindings)))

    (if matched-positions
      (let* ((matched-intervals (loop for i from 1 to (- (length matched-positions) 1)
                                      for interval = (list (nth1 i matched-positions) (nth1 (+ i 1) matched-positions))
                                      do (setf i (+ i 1))
                                      collect interval))
             (non-matched-intervals (calculate-unmatched-intervals matched-intervals (mapcar #'(lambda (feat)
                                                                                                 (list (third feat) (fourth feat)))
                                                                                             source-form-predicates))))

        ;; Based on the non-matched intervals (e.g. '((0 4) (12 28))), create sequence new features to add to the root
        (when non-matched-intervals
          (loop for (feat-name string start end) in source-form-predicates ;;(sequence "what is the color of the cube?" 12 18)
                for offset = (abs (- 0 start))
                append (loop for (left right) in non-matched-intervals
                             for normalised-left = (- left offset)
                             for normalised-right = (- right offset)
                             if (overlapping-intervals-p (list start end) (list left right))
                               collect (let ((unmatched-substring (subseq string normalised-left normalised-right)))
                                         `(,feat-name ,unmatched-substring ,left ,right))) into new-sequence-features
                finally (return (sort new-sequence-features #'< :key #'third)))))
      ;;If there were no sequence predicates, nothing changes in the source
      source-form-predicates
      )))


(defun retrieve-matched-positions (pattern-sequence-predicates bindings)
  "Retrieves the matched positions of the left and right boundaries in pattern-sequence-predicates from the bindings. "
  (let* ((list-of-matched-indices (flatten (mapcar #'(lambda (x) (when (equal (feature-name x) 'SEQUENCE) ;;why flatten??
                                                                   (rest (rest x))))
                                                   pattern-sequence-predicates)))
         (matched-positions (loop with matched-positions = nil
                                  for index in list-of-matched-indices
                                  for binding = (cdr (assoc index bindings :test #'equalp))
                                  when binding
                                    if (variable-p binding)
                                      do (push (lookup-binding binding bindings) matched-positions)
                                    else do (push binding matched-positions)
                                  finally (return matched-positions))))
    
    (sort (remove nil matched-positions) #'<)))


(defun lookup-binding (target-var bindings)
  "Recursively lookup binding of target-var in bindings."
  (let ((binding (cdr (assoc target-var bindings))))
    (if (variable-p binding)
      (lookup-binding binding bindings)
      binding)))

(defun calculate-unmatched-intervals (matched-intervals root-intervals)
  "Based on matched intervals and root intervals (before matching), calculate intervals in root that are unmatched and should stay in root."
  (loop with final-intervals = nil
        for root-interval in root-intervals
        for expanded-interval = (expand-interval root-interval)
        for car-equality-position = nil
        for cdr-equality-position = nil
        do (loop for matched-interval in matched-intervals ;; we set car-equality-position and cdr-equality-position
                 do (loop for i in expanded-interval
                          when (equal (car i) (car matched-interval))
                            do (setf car-equality-position (position i expanded-interval))
                          when (equal (cdr i) (first (cdr matched-interval)))
                            do (setf cdr-equality-position (position i expanded-interval)))
                 when (and car-equality-position cdr-equality-position)
                   do (let* ((excluded-positions (loop for i from car-equality-position to cdr-equality-position collect i))
                             (excluded-items (loop for i in excluded-positions collect (nth i expanded-interval))))
                        (loop for excluded-item in excluded-items
                              do (setf expanded-interval (remove excluded-item expanded-interval)))
                        (setf car-equality-position nil)
                        (setf cdr-equality-position nil)))
        when expanded-interval 
          do (loop with collapsed-intervals = (collapse-intervals expanded-interval)
                   for collapsed-interval in collapsed-intervals
                   do (pushend collapsed-interval final-intervals))
        finally (return final-intervals)))

(defun expand-interval (interval)
  "From a given interval expands it into a list of adjacent cons cells"
  (loop with interval-cons-cells = nil
        for i from (first interval) to (- (first (last interval)) 1)
        do (pushend (cons i (+ i 1)) interval-cons-cells)
        finally (return interval-cons-cells)))


;; (expand-interval '(0 30))
;; (expand-interval '(12 25))

(defun collapse-intervals (interval-cons-cells)
  "From a list of cons-cells, renders a list or multiple lists of adjacent cons cells"
  (let ((intervals '())
        (provisory-cons-cells-list '())
        (interval '()))
    (loop for i in interval-cons-cells
          for pos-i = (position i interval-cons-cells) ;; position of the current element
          for i+1 = nil ;; the next element, for now it is nil
          do (pushend i provisory-cons-cells-list)
             (if (not (eq i (first (last interval-cons-cells)))) ;; when we are not considering the last element of the list
               (progn
                 (setf i+1 (nth (+ pos-i 1) interval-cons-cells)) ;; we set the next-element
                 (when (not (= (cdr i) (car i+1))) ;; if the cdr of the current element is not equal to the car of the next element
                   ;; then we make an interval out of the previous considered cons cells that are stored in the provisory-cons-cells-list
                   (pushend (car (first provisory-cons-cells-list)) interval) ;; left elem of the interval
                   (pushend (cdr (first (last provisory-cons-cells-list))) interval) ;; right elem of the interval
                   (pushend interval intervals) ;; we push the interval to the list of intervals
                   (setf interval nil) ;; we set the interval to nil
                   (setf provisory-cons-cells-list nil))) ;; we also set the provisory-cons-cells-list to nil
               ;; and for the last element of the list:
               (progn
                 (pushend i provisory-cons-cells-list)
                 (pushend (car (first provisory-cons-cells-list)) interval) ;; left elem of the interval
                 (pushend (cdr (first (last provisory-cons-cells-list))) interval) ;; right elem of the interval
                 (pushend interval intervals)))) ;; we push the interval to the list of intervals
    intervals))




;; (collapse-intervals '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6) (6 . 7) (7 . 8) (8 . 9) (9 . 10) (10 . 11) (11 . 12) (25 . 26) (26 . 27) (27 . 28) (28 . 29) (29 . 30)))
;; expected: '((0 12) (25 30))

;; (collapse-intervals '((12 . 13) (24 . 25)))
;; expected: '((12 13) (24 25))

;; (collapse-intervals '((7 . 8) (9 . 10)))


                   
;; using cons cells of the intervals
;; (calculate-unmatched-intervals '((8 9)) '((3 4) (7 10) (17 18)))
;; expected: '((3 4) (7 8) (9 10) (17 18))


;; (calculate-unmatched-intervals '((29 30) (0 4)) '((0 12) (17 30)))
;; expected: '((4 12) (17 29))

;; (calculate-unmatched-intervals '((0 4) (29 30)) '((0 12) (17 30)))
