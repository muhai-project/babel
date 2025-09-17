(in-package :slp)

(defun make-hamnosys-predicates (elan-intervals)
  "makes a set of hamnosys-predicates for elan-intervals"
  (loop with predicates = '()
        for interval in elan-intervals
        do
          ;; check whether the interval has a type (rule out non-dominant hand of two-handed signs)
          (when (interval-type interval)
             ;; create the hamnosys-predicate and push it to predicates
             (push `(,(interval-type interval) ,(fcg-id interval) ,(hamnosys interval)) predicates))
        ;; return the reverse list of predicates
        finally (return (reverse predicates))))


(defun sort-elan-intervals (elan-intervals)
  "returns a chronologically sorted version of the input list of elan-intervals"
  (loop with output = '()
        for interval in elan-intervals
        when output
          do (loop with item-added = nil
                   for item in output
                   when (< (cdr (begin interval))(cdr (begin item)))
                     do (insert-after output (- (position item output) 1) interval)
                        (setf item-added t)
                   when item-added
                     do (return t)
                   finally (pushend interval output))
        when (not output)
          do (push interval output)
        finally (return output)))



(defun make-adjacent-predicates (elan-intervals)
  "makes adjacent predicates for intervals in elan-intervals"
    (loop with relations = '()
          with sorted-intervals = (sort-elan-intervals elan-intervals)
          with current-interval = (first sorted-intervals)
          for interval in (rest sorted-intervals)
          when (equal-relaxed
                (cdr (begin interval))
                (cdr (end current-interval))
                100)
            do (push
                `(adjacent
                  ,(fcg-id current-interval)
                  ,(fcg-id interval))
                relations)
          do (setf current-interval interval)
          finally (return (reverse relations))))


(defun make-alignment-predicates (dominant-intervals non-dominant-intervals)
  "Makes a set of temporal alignments between intervals in non-dominant-intervals and dominant-intervals"
  (let ((relations '()))
    (loop for non-dominant-interval in non-dominant-intervals
          for preceding-dominant-interval = nil
          for following-dominant-interval = nil
          do (loop for dominant-interval in dominant-intervals
                   ;; check whether the dominant-interval and non-dominant-interval overlap
                   when (>
                         (measure-overlap non-dominant-interval dominant-interval)
                         0)
                     do (cond
                         
                         ;;-------------;;
                         ;; Condition A ;;
                         ;;-------------;;
                         ;; if the hamnosys of the interval starts with a symmetry operator it
                         ;; is a two-handed sign so we set the type of the dominant interval to two-hand-articulation
                         ;; the type of the non-dominant interval is set to nil as no predicate should be created for it
                         ((member
                           (char-name
                            (char
                             (hamnosys dominant-interval)
                             0))
                           (list "U+E0E9" "U+E0E8")
                           :test #'string=)
                          (setf (interval-type
                                 dominant-interval)
                                'two-hand-articulation)
                          (setf (interval-type
                                 non-dominant-interval)
                                nil))
                         
                         ;;-------------;;
                         ;; Condition B ;;
                         ;;-------------;;
                         ;; when both the end and the begin of the dominant-interval and non-dominant-interval are the same,
                         ;; a start-coincides and end-coincides predicate is created linking the dominant and non-dominant
                         ;; intervals
                         ((and
                           ;; check whether begin of intervals is equal (relaxation of 100 ms)
                           (equal-relaxed
                            (cdr (begin dominant-interval))
                            (cdr (begin non-dominant-interval))
                            100)
                           ;; check whether end of intervals is equal (relaxation of 100 ms)
                           (equal-relaxed
                            (cdr (end dominant-interval))
                            (cdr (end non-dominant-interval))
                            100))
                          ;; create start-coincides predicate
                          (push
                           `(start-coincides
                             ,(fcg-id dominant-interval)
                             ,(fcg-id non-dominant-interval))
                           relations)
                          ;; create end-coincides predicate
                          (push
                           `(end-coincides
                             ,(fcg-id dominant-interval)
                             ,(fcg-id non-dominant-interval))
                           relations))
                         
                         ;;-------------;;
                         ;; Condition C ;;
                         ;;-------------;;
                         ;; when only the begin of the dominant-interval and non-dominant-interval coincide, only
                         ;; a start-coincides predicate is created linking the dominant and non-dominant intervals
                         ((equal-relaxed
                           (cdr (begin dominant-interval))
                           (cdr (begin non-dominant-interval))
                           100)
                          (push
                           `(start-coincides
                             ,(fcg-id dominant-interval)
                             ,(fcg-id non-dominant-interval))
                           relations))

                         ;;-------------;;
                         ;; Condition D ;;
                         ;;-------------;;
                         ;; when only the end of the dominant-interval and non-dominant-interval coincide, only
                         ;; an end-coincides predicate is created linking the dominant and non-dominant intervals
                         ((equal-relaxed
                           (cdr (end dominant-interval))
                           (cdr (end non-dominant-interval))
                           100)
                          (push
                           `(end-coincides
                             ,(fcg-id dominant-interval)
                             ,(fcg-id non-dominant-interval))
                           relations))

                         ;;-------------;;
                         ;; Condition E ;;
                         ;;-------------;;
                         ;; when the beginning and end of the dominant-interval lie within the interval of
                         ;; the non-dominant interval, a during predicate is created linking the dominant and non-dominant
                         ;; intervals
                         ((and
                           (>
                            (cdr (begin dominant-interval))
                            (cdr (begin non-dominant-interval)))
                           (<
                            (cdr (end dominant-interval))
                            (cdr (end non-dominant-interval))))
                          (push
                           `(during
                             ,(fcg-id dominant-interval)
                             ,(fcg-id non-dominant-interval))
                           relations))))

             ;; when the type of the non-dominant interval is not nil (i.e. it is not part of a two handed sign)
             (when (interval-type non-dominant-interval)
               
               ;; if present find the dominant interval that directly precedes the non-dominant interval
               (setf preceding-dominant-interval
                     (find-preceding-interval
                      non-dominant-interval
                      dominant-intervals))
               
               ;; if present find the dominant interval that directly follows the non-dominant interval
               (setf following-dominant-interval
                     (find-following-interval
                      non-dominant-interval
                      dominant-intervals))
               
               ;; when there is a directly preceding dominant interval, create an adjacent predicate
               (when preceding-dominant-interval
                 (push
                  `(adjacent
                    ,(fcg-id preceding-dominant-interval)
                    ,(fcg-id non-dominant-interval))
                  relations))
               
               ;; when there is a directly following dominant interval, create an adjacent predicate
               (when following-dominant-interval
                 (push
                  `(adjacent
                    ,(fcg-id non-dominant-interval)
                    ,(fcg-id following-dominant-interval))
                  relations))))
            ;; return the list of all alignment predicates
            (reverse relations)))