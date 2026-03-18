(in-package :fcg)

(export '(anti-unify-predicate-network anti-unification-result
          generalisation pattern-bindings source-bindings
          pattern-delta source-delta cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unifying sets of predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Jens - 04/01/2024
;;;; The global variable *alpahbet* is implemented as a circular list
;;;; by having the cdr of the last cons-cell in the list point to the
;;;; start of the list instead of to NIL. This allows to cycle through
;;;; the alphabet by each time calling 'pop'. However, this makes it
;;;; impossible to print the variable *alphabet* (as it would be
;;;; infinitely long). So do not try this, unless you want LispWorks
;;;; to be blocked...
;;;; You _can_ print *alphabet* when setting the system variable
;;;; *print-circle* to T, but this has other effects that are not
;;;; desirable. For instance, the predicate (F ?B ?B) would be printed
;;;; as (F #1=?B #1#). 

(defun make-circular-list (list)
  ;(setf *print-circle* t)
  (setf (cdr (last list)) list)
  list)

(defparameter *alphabet* (make-circular-list (mapcar #'mkstr '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))))

(defun next-au-var ()
  (pop *alphabet*))


(defclass anti-unification-result ()
  ((pattern
    :accessor pattern :initarg :pattern :initform nil
    :documentation "The original pattern")
   (source
    :accessor source :initarg :source :initform nil
    :documentation "The original source")
   (generalisation
    :accessor generalisation :initarg :generalisation :type list :initform nil
    :documentation "The generalisation")
   (pattern-bindings
    :accessor pattern-bindings :initarg :pattern-bindings :type list :initform nil
    :documentation "Bindings to go back to the original pattern")
   (source-bindings
    :accessor source-bindings :initarg :source-bindings :type list :initform nil
    :documentation "Bindings to go back to the original source")
   (pattern-delta
    :accessor pattern-delta :initarg :pattern-delta :type list :initform nil
    :documentation "Part of pattern not in generalisation")
   (source-delta
    :accessor source-delta :initarg :source-delta :type list :initform nil
    :documentation "Part of source not in generalisation")
   (cost
    :accessor cost :initarg :cost :type number :initform 0
    :documentation "Cost of the generalisation"))
  (:documentation "Result of anti-unification consists of a generalisation,
                   delta's, bindings lists, and a cost."))

(defclass predicate-network-au-result (anti-unification-result) ()
  (:documentation "Subclass of anti-unification result for predicate networks"))

(defclass string-au-result (anti-unification-result) ()
  (:documentation "Subclass of anti-unification result for strings"))

(defclass sequences-au-result (anti-unification-result)
  ((alignment-cost
    :accessor alignment-cost :initarg :alignment-cost :type number :initform 0
    :documentation "The cost of the maximal string alignment"))
  (:documentation "Subclass of anti-unification result for sequence predicates"))


(defun anti-unify-predicate-network (pattern source &key allow-generalisation-over-constants)
  "Anti-unifies pattern with source. Returns 5 values:
   generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with possible-alignments = (identify-possible-alignments pattern source
                                                                 :allow-generalisation-over-constants
                                                                 allow-generalisation-over-constants)
        for alignment in possible-alignments
        for pattern-in-alignment = (pattern-predicates alignment)
        for source-in-alignment = (source-predicates alignment)
        for pattern-delta = (pattern-delta alignment)
        for source-delta = (source-delta alignment)
        collect (multiple-value-bind (resulting-generalisation
                                      resulting-pattern-bindings
                                      resulting-source-bindings
                                      resulting-pattern-delta
                                      resulting-source-delta)
                    (anti-unify-predicate-sequence pattern-in-alignment source-in-alignment nil nil nil pattern-delta source-delta)
                  (let ((au-result (make-instance 'predicate-network-au-result
                                                  :pattern pattern
                                                  :source source
                                                  :generalisation resulting-generalisation
                                                  :pattern-bindings resulting-pattern-bindings
                                                  :source-bindings resulting-source-bindings
                                                  :pattern-delta resulting-pattern-delta
                                                  :source-delta resulting-source-delta)))
                    (setf (cost au-result) (anti-unification-cost au-result))
                    au-result))
          into results
        ;; Sort results based on increasing cost.
        finally (return (sort results #'< :key #'cost))))



(defun anti-unify-predicate (pattern
                             source
                             &optional
                             pattern-bindings
                             source-bindings
                             pattern-delta
                             source-delta)
  "Generalises over pattern and source and returns 5 things:
generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  (cond
   ;; Case: pattern equals source -> simply return pattern = source as generalisation
   ((and (atom pattern)
         (atom source)
         (equalp pattern source))
    (values pattern
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Case: pattern and source are different, but already share a binding in the bindingslist -> return binding
   ;; as generalisation
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Case: pattern and source are predicates with the same name and the same arity -> return predicate with
   ;; anti-unified arguments
   ((and (listp pattern) (listp source)
         (atom (feature-name source)) (atom (feature-name pattern))
         (equalp (feature-name source) (feature-name pattern))
         (= (length pattern) (length source)))
    (anti-unify-predicate-sequence pattern source nil pattern-bindings source-bindings pattern-delta source-delta))
   ;; None of the above (pattern and source are different and no binding is available yet) -> introduce new
   ;; binding as generalisation
   (t
    (let ((var (make-var (next-au-var))))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              pattern-delta
              source-delta)))))




(defun anti-unify-predicate-sequence (pattern
                                      source
                                      &optional
                                      generalisation
                                      pattern-bindings
                                      source-bindings
                                      pattern-delta
                                      source-delta)
  "Generalises over two sequences of predicates of the same length, returns generalisation, bindingslists and deltas."

  ;; Assert that the length of the pattern and the source are equal
  (assert (= (length pattern) (length source)))
  (cond
   ;; Base case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values generalisation
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Recursive case: still elements left, anti-unify first and then rest,
   ;; every time passing the new bindings and deltas, accumulating the generalisation
   (t
    (multiple-value-bind (resulting-generalisation
                          resulting-pattern-bindings
                          resulting-source-bindings
                          resulting-pattern-delta
                          resulting-source-delta)
        (anti-unify-predicate (first pattern) (first source) pattern-bindings source-bindings pattern-delta source-delta)
      (anti-unify-predicate-sequence (rest pattern)
                                     (rest source)
                                     (append generalisation (list resulting-generalisation))
                                     resulting-pattern-bindings
                                     resulting-source-bindings
                                     resulting-pattern-delta
                                     resulting-source-delta)))))

; (anti-unify-predicate-sequence '(a b c) '(a d c))
; (anti-unify-predicate-sequence '(a b c b) '(a d c e))



(defgeneric anti-unification-cost (au-result)
  (:documentation "Compute the cost of the anti-unification result"))

(defmethod anti-unification-cost ((au-result predicate-network-au-result))
  "The anti-unification cost is the sum of the number of predicates in the deltas and the number of variables that have
   been bound to more than 1 variable in the generalisation."
  (with-slots (pattern-bindings source-bindings
               pattern-delta source-delta) au-result          
    (let ((nr-of-predicates-in-pattern-delta (length pattern-delta))
          (nr-of-predicates-in-source-delta (length source-delta))
          (nr-of-bindings-to-multiple-vars-in-pattern
           (loop for (binding . rest) on  pattern-bindings
                 count (find (car binding) rest :key #'car :test #'equalp)))
          (nr-of-bindings-to-multiple-vars-in-source
           (loop for (binding . rest) on  source-bindings
                 count (find (car binding) rest :key #'car :test #'equalp))))
      (+ nr-of-predicates-in-pattern-delta
         nr-of-predicates-in-source-delta
         nr-of-bindings-to-multiple-vars-in-pattern
         nr-of-bindings-to-multiple-vars-in-source))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identify possible alignments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass predicate-alignment-state ()
  ((pattern-predicates
    :accessor pattern-predicates :initarg :pattern-predicates :initform nil)
   (source-predicates
    :accessor source-predicates :initarg :source-predicates :initform nil)
   (pattern-delta
    :accessor pattern-delta :initarg :pattern-delta :initform nil)
   (source-delta
    :accessor source-delta :initarg :source-delta :initform nil)
   (pattern-remaining
    :accessor pattern-remaining :initarg :pattern-remaining :initform nil)
   (source-remaining
    :accessor source-remaining :initarg :source-remaining :initform nil)))


(defun make-initial-alignment-state (pattern source)
  (make-instance 'predicate-alignment-state
                 :pattern-remaining pattern
                 :source-remaining source))


(defun identify-possible-alignments (pattern source &key allow-generalisation-over-constants)
  "Returns a list of :pattern :source :pattern-delta :source-delta alists, which list all possibilities
   to align pattern-predicates with source predicates, and the resulting delta's (= non-aligned predicates)."
  (loop with solutions = nil
        with queue = (list (make-initial-alignment-state pattern source))
        while queue
        for state = (pop queue)
        do (with-slots (pattern-predicates source-predicates
                        pattern-delta source-delta
                        pattern-remaining source-remaining) state
             (cond ; no predicates remaining in pattern and source: we have a solution!
              ((and (null pattern-remaining) (null source-remaining))
               (push state solutions))
              
              ; no pattern-predicates remaining: rest of source goes to source-delta
              ((null pattern-remaining)
               (push (make-instance 'predicate-alignment-state
                                    :pattern-predicates pattern-predicates
                                    :source-predicates source-predicates
                                    :pattern-delta pattern-delta
                                    :source-delta (append source-delta source-remaining))
                     queue))
            
              ; no source-predicates remaining: rest of pattern goes to pattern-delta
              ((null source-remaining)
               (push (make-instance 'predicate-alignment-state
                                    :pattern-predicates pattern-predicates
                                    :source-predicates source-predicates
                                    :pattern-delta (append pattern-delta pattern-remaining)
                                    :source-delta source-delta)
                     queue))
            
              ; pattern and source predicates remaining: consume first pattern predicate and combine with possible source-predicates
              ; if more occurrences in pattern than source also keep option that it goes into the pattern-delta
              (t
               (let* ((first-pattern-predicate (first pattern-remaining))
                      (matching-source-predicates
                       (find-all first-pattern-predicate source-remaining
                                 :test (lambda (pattern-predicate source-predicate)
                                         (matching-predicates pattern-predicate source-predicate
                                                              :allow-generalisation-over-constants
                                                              allow-generalisation-over-constants)))))
                 (when matching-source-predicates
                   (loop for matching-source-predicate in matching-source-predicates
                         do (push (make-instance 'predicate-alignment-state
                                                 :pattern-predicates (append pattern-predicates (list first-pattern-predicate))
                                                 :source-predicates (append source-predicates (list matching-source-predicate))
                                                 :pattern-delta pattern-delta
                                                 :source-delta source-delta
                                                 :pattern-remaining (rest pattern-remaining)
                                                 :source-remaining (remove matching-source-predicate source-remaining))
                                  queue)))
                 (when (or (null matching-source-predicates)
                           (>= (length (find-all first-pattern-predicate (rest pattern-remaining)
                                                 :test (lambda (pattern-predicate remaining-pattern-predicate)
                                                         (matching-predicates pattern-predicate remaining-pattern-predicate
                                                                              :allow-generalisation-over-constants
                                                                              allow-generalisation-over-constants))))
                               (length matching-source-predicates)))
                   (push (make-instance 'predicate-alignment-state
                                        :pattern-predicates pattern-predicates
                                        :source-predicates source-predicates
                                        :pattern-delta (append pattern-delta (list first-pattern-predicate))
                                        :source-delta source-delta
                                        :pattern-remaining (rest pattern-remaining)
                                        :source-remaining source-remaining)
                         queue))))))
          
        finally (return solutions)))

#|
(identify-possible-alignments '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat) (count ?dogs))
                              '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat)
                                (bind character-cat ?mean-cat mean) (filter ?mean-dogs ?dogs ?mean-cat) (count ?mean-dogs)))
|#



(defun matching-predicates (predicate-1 predicate-2 &key allow-generalisation-over-constants)
  "Returns t if predicate-1 and predicate-2 can be aligned: same feature name and same arity.
   By default, returns nil if a constant occurs in one predicate and the same constant does
   not appear at the same position in the other predicate."
  (and
   ;; predicate names are equal
   (equalp (first predicate-1) (first predicate-2))
   ;; arity is equal
   (= (length predicate-1) (length predicate-2))
   ;; allow or disallow generalisation over constants
   (if allow-generalisation-over-constants
     t
     (loop for arg-1 in (rest predicate-1)
           for arg-2 in (rest predicate-2)
           always (or (equalp arg-1 arg-2)
                      (and (variable-p arg-1)
                           (variable-p arg-2)))))))

;; (matching-predicates '(p ?a b ?c) '(p ?x b ?y) :allow-generalisation-over-constants t)
;; (matching-predicates '(p ?a b ?c) '(p ?x c ?y) :allow-generalisation-over-constants t)
;; (matching-predicates '(p ?a b ?c) '(p ?x b ?y) :allow-generalisation-over-constants nil)
;; (matching-predicates '(p ?a b ?c) '(p ?x c ?y) :allow-generalisation-over-constants nil)
;; (matching-predicates '(p ?a b ?c) '(f ?x b ?y) :allow-generalisation-over-constants nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print anti-unification results ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-anti-unification-results (list-of-anti-unification-results &optional (stream t))
  "Prints a list of anti-unification results."

  ;; Check reversibility of generalisation
  (assert (loop for a-u-result in list-of-anti-unification-results
                for ori-pattern = (pattern a-u-result)
                for ori-source = (source a-u-result)
                for this-ori-pattern = (compute-network-from-anti-unification-result a-u-result 'pattern)
                for this-ori-source = (compute-network-from-anti-unification-result a-u-result 'source)
                always (and (equivalent-predicate-networks this-ori-pattern ori-pattern)
                            (equivalent-predicate-networks this-ori-source ori-source))))
  
  (format t "~%~%~%### Anti-unifying ###~%~%")
  (format stream "------------------------------------------------------------~%")
  (format stream "- Pattern:~%~%")
  (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" (pattern (first list-of-anti-unification-results))))
  (format stream "- Source:~%~%")
  (let ((*print-pretty* t))
    (format stream "~(~a~)~%~%" (source (first list-of-anti-unification-results))))
  (format stream "------------------------------------------------------------~%~%")
  (loop for a-u-result in list-of-anti-unification-results
        for i from 1 upto (length list-of-anti-unification-results)
        do (with-slots (generalisation pattern-bindings source-bindings pattern-delta source-delta cost) a-u-result
             (format stream "--- Result ~a (cost: ~a) ---~%~%" i cost)
             (format stream "- Generalisation:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" generalisation))
             (format stream "- Pattern bindings:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" pattern-bindings))
             (format stream "- Source bindings:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" source-bindings))
             (format stream "- Pattern delta:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" pattern-delta))
             (format stream "- Source delta:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%~%" source-delta))
             (format stream "- Computed Pattern:~%~%")
             (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result a-u-result 'pattern)))
             (format stream "- Computed Source:~%~%")
             (let ((*print-pretty* t))
               (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result a-u-result 'source))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reversibility check ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric compute-network-from-anti-unification-result (au-result pattern-or-source)
  (:documentation "Returns original network based on generalisation, bindings-list and delta."))

(defmethod compute-network-from-anti-unification-result ((au-result predicate-network-au-result) pattern-or-source)
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
    (append (substitute-bindings (reverse-bindings bindings-list) generalisation)
            delta)))



(defmethod copy-object-content ((au-result anti-unification-result) (copy anti-unification-result))
  ;; shallow copy
  (setf (generalisation copy) (generalisation au-result)
        (pattern-bindings copy) (pattern-bindings au-result)
        (source-bindings copy) (source-bindings au-result)
        (pattern-delta copy) (pattern-delta au-result)
        (source-delta copy) (source-delta au-result)
        (cost copy) (cost au-result)))
                           
