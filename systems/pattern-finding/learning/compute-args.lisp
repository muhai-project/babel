(in-package :pf)

;;;;;;;;;;;;;;;;;;
;; compute args ;;
;;;;;;;;;;;;;;;;;;

(defgeneric compute-form-args (anti-unification-result cxn source-args)
  (:documentation "Compute the form-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defgeneric compute-meaning-args (anti-unification-result cxn source-args)
  (:documentation "Compute the meaning-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defmethod compute-form-args (anti-unification-result cxn source-args)
  (let ((form-representation (get-configuration (cxn-inventory cxn) :form-representation-formalism)))
    (compute-form-args-aux anti-unification-result cxn source-args form-representation)))

;;;;;;;;;;;;;;;
;; sequences ;;
;;;;;;;;;;;;;;;

(defun sequence-string (sequence-predicate)
  (second sequence-predicate))
(defun left-boundary (sequence-predicate)
  (third sequence-predicate))
(defun right-boundary (sequence-predicate)
  (fourth sequence-predicate))

(defun compute-string-from-generalisation (generalisation delta)
  (let ((complete-string
         (list-of-strings->string
          (loop for el in generalisation
                if (consp el)
                collect (rest (assoc el delta :test #'equal))        
                else collect el)
          :separator "")))
    (list (list 'sequence complete-string (make-var) (make-var)))))

(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :sequences)))
  (with-slots (generalisation
               pattern-delta
               source-delta) anti-unification-result
    (let* (;; make sequence predicates from the generalisation and delta's
           (generalisation-predicates
            (loop for elem in generalisation
                  when (stringp elem)
                  collect (let* ((pos (position elem generalisation :test #'equal))
                                 (prev-elem (unless (= pos 0) (nth (1- pos) generalisation)))
                                 (next-elem (nth (1+ pos) generalisation))
                                 (left-boundary (if prev-elem (cdr prev-elem) (make-var 'lb)))
                                 (right-boundary (if next-elem (car next-elem) (make-var 'rb))))
                            (list 'sequence elem left-boundary right-boundary))))
           (pattern-delta-predicates
            (loop for (boundaries . seq-str) in pattern-delta
                  collect (list 'sequence seq-str (car boundaries) (cdr boundaries))))
           (source-delta-predicates
            (loop for (boundaries . seq-str) in source-delta
                  collect (list 'sequence seq-str (car boundaries) (cdr boundaries))))
           ;; slot/top-args of pattern and soource
           (pattern-slot-args
            (sort (cxn-form-slot-args anti-unified-cxn :by-category-p t) #'string< :key #'car))
           (pattern-top-args (cxn-form-top-args anti-unified-cxn))
           (source-slot-args (find-data source-args :slot-form-args))
           (source-top-args (or (find-data source-args :top-lvl-form-args)
                                ;; compute entirely new top args, like for a holophrase cxn,
                                ;; by putting together the source delta and the generalisation
                                (holistic-form-top-args
                                 (compute-string-from-generalisation generalisation source-delta)
                                 (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism))))
           ;; start/end with slot?
           (starts-with-slot (consp (first generalisation)))
           (ends-with-slot (consp (last-elt generalisation)))
           ;; blackboard for args
           (args (make-blackboard)))
      ;; generalisation top lvl args + pattern/source slot args
      (loop with len = (length generalisation-predicates)
            for predicate in generalisation-predicates
            for index from 1
            do (cond ((and (= index 1) starts-with-slot)
                      (let ((boundaries (first generalisation)))
                        (push-data args :generalisation-top-lvl-args (car boundaries))
                        (push-data args :generalisation-top-lvl-args (cdr boundaries))))
                     ((= index 1)
                      (push-data args :generalisation-top-lvl-args (fourth predicate)))
                     ((and (= index len) ends-with-slot)
                      (let ((boundaries (last-elt generalisation)))
                        (push-data args :generalisation-top-lvl-args (car boundaries))
                        (push-data args :generalisation-top-lvl-args (cdr boundaries))))
                     ((= index len)
                      (push-data args :generalisation-top-lvl-args (third predicate)))
                     (t
                      (push-data args :generalisation-top-lvl-args (third predicate))
                      (push-data args :generalisation-top-lvl-args (fourth predicate)))))
      (set-data args :generalisation-top-lvl-args
                (reverse (get-data args :generalisation-top-lvl-args)))
      (set-data args :pattern-slot-args
                (append
                 (list (loop for predicate in (reverse pattern-delta-predicates)
                             append (list (third predicate) (fourth predicate))))
                 (mapcar #'rest pattern-slot-args)))
      (set-data args :source-slot-args
                (append
                 (list (loop for predicate in (reverse source-delta-predicates)
                             append (list (third predicate) (fourth predicate))))
                 source-slot-args))
      ;; pattern/source top args
      (set-data args :pattern-top-lvl-args
                (holistic-form-top-args
                 (append generalisation-predicates pattern-delta-predicates)
                 (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism)))
      (set-data args :source-top-lvl-args
                (holistic-form-top-args
                 (append generalisation-predicates source-delta-predicates)
                 (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism)))
      ;; cleanup
      (setf generalisation generalisation-predicates)
      (setf pattern-delta (remove-if #'(lambda (p) (string= (second p) "")) pattern-delta-predicates))
      (setf source-delta (remove-if #'(lambda (p) (string= (second p) "")) source-delta-predicates))
      ;; done!
      args)))

;;;;;;;;;;;;;;;;;;
;; string+meets ;;
;;;;;;;;;;;;;;;;;;

(defun restore-original-input (generalisation bindings delta)
  (append (substitute-bindings (fcg::reverse-bindings bindings) generalisation) delta))

(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :string+meets)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args  ; group per category and alphabetically
             (sort (cxn-form-slot-args anti-unified-cxn :by-category-p t) #'string< :key #'car))
            (pattern-top-args (cxn-form-top-args anti-unified-cxn))
            (source-slot-args  ; grouped per category, sorted alphabetically
             (find-data source-args :slot-form-args))
            (source-top-args (or (find-data source-args :top-lvl-form-args)
                                 (holistic-form-top-args
                                  (restore-original-input generalisation source-bindings source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism))))
            (push-meets (get-configuration (cxn-inventory  anti-unified-cxn) :push-meets-to-deltas)))
        (when push-meets
          (setf anti-unification-result
                (push-meets-to-deltas anti-unification-result pattern-top-args source-top-args)))
        (multiple-value-bind (gen-binding-vars pattern-binding-vars source-binding-vars)
            (loop for (pattern-var . generalisation-var) in (reverse pattern-bindings)
                  for (source-var . nil) in (reverse source-bindings)
                  when (or (find-anywhere pattern-var pattern-delta)
                           (find-anywhere source-var source-delta)
                           (find-anywhere pattern-var pattern-slot-args)
                           (find-anywhere source-var source-slot-args)
                           (find pattern-var pattern-top-args)
                           (find source-var source-top-args)
                           (> (count pattern-var pattern-bindings :key #'car) 1)
                           (> (count source-var source-bindings :key #'car) 1))
                    collect generalisation-var into gen-vars
                    and collect pattern-var into pattern-vars
                    and collect source-var into source-vars
                  finally (return (values gen-vars pattern-vars source-vars)))
          (set-data args :generalisation-top-lvl-args gen-binding-vars)
          (set-data args :pattern-slot-args (append (list pattern-binding-vars) (mapcar #'rest pattern-slot-args)))
          (set-data args :source-slot-args (append (list source-binding-vars) source-slot-args))
          (set-data args :pattern-top-lvl-args pattern-top-args)
          (set-data args :source-top-lvl-args source-top-args))))
    args))
        
(defmethod compute-meaning-args (anti-unification-result
                                 (anti-unified-cxn fcg-construction)
                                 (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args  ; grouped per category, sorted alphabetically
             (sort (cxn-meaning-slot-args anti-unified-cxn :by-category-p t) #'string< :key #'car))
            (pattern-top-args (cxn-meaning-top-args anti-unified-cxn))
            (source-slot-args  ; grouped per category, sorted alphabetically
             (find-data source-args :slot-meaning-args))
            (source-top-args (or (find-data source-args :top-lvl-meaning-args)
                                 (holistic-meaning-top-args
                                  (restore-original-input generalisation source-bindings source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :meaning-representation-formalism)))))
        (multiple-value-bind (gen-binding-vars pattern-binding-vars source-binding-vars)
            (loop for (pattern-var . generalisation-var) in (reverse pattern-bindings)
                  for (source-var . nil) in (reverse source-bindings)
                  when (or (find-anywhere pattern-var pattern-delta)
                           (find-anywhere source-var source-delta)
                           (find-anywhere pattern-var pattern-slot-args)
                           (find-anywhere source-var source-slot-args)
                           (find pattern-var pattern-top-args)
                           (find source-var source-top-args)
                           (> (count pattern-var pattern-bindings :key #'car) 1)
                           (> (count source-var source-bindings :key #'car) 1))
                  collect generalisation-var into gen-vars
                  and collect pattern-var into pattern-vars
                  and collect source-var into source-vars
                  finally (return (values gen-vars pattern-vars source-vars)))
          (set-data args :generalisation-top-lvl-args gen-binding-vars)
          (set-data args :pattern-slot-args (append (list pattern-binding-vars) (mapcar #'rest pattern-slot-args)))
          (set-data args :source-slot-args (append (list source-binding-vars) source-slot-args))
          (set-data args :pattern-top-lvl-args pattern-top-args)
          (set-data args :source-top-lvl-args source-top-args))))
    args))
            
              
#|
(defmethod compute-form-args-aux (anti-unification-result
                                  (anti-unified-cxn fcg-construction)
                                  (source-args blackboard)
                                  (mode (eql :string+meets)))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args (extract-slot-form-args anti-unified-cxn))
            (pattern-top-args (extract-top-lvl-form-args anti-unified-cxn))
            (source-slot-args (find-data source-args :slot-form-args))
            (source-top-args (or (find-data source-args :top-lvl-form-args)
                                 ;; compute entirely new top args, like for a holophrase cxn,
                                 ;; by putting together the source delta and the generalisation
                                 (holistic-form-top-args
                                  (append (substitute-bindings (fcg::reverse-bindings source-bindings) generalisation) source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :form-representation-formalism)))))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              ;when (or (find-anywhere pattern-var pattern-delta)
              ;         (find-anywhere source-var source-delta)
              ;         (find pattern-var pattern-slot-args)
              ;         (find source-var source-slot-args)
              ;         (> (count pattern-var pattern-bindings :key #'car) 1)
              ;         (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn) pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) source-top-args)
                        ((item-based-cxn-p anti-unified-cxn) source-slot-args)))
        (set-data args :generalisation-top-lvl-args
                  (loop for arg in pattern-top-args
                        collect (or (rest (assoc arg pattern-bindings)) arg)))))
    args))

(defmethod compute-meaning-args (anti-unification-result (anti-unified-cxn fcg-construction) (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let (;; pattern slot args are meaning-args that are used in the units that
            ;; represent slots in the pattern (anti-unified-cxn)
            (pattern-slot-args (extract-slot-meaning-args anti-unified-cxn))
            ;; pattern top args are meaning-args that are used on the contributing
            ;; part of the pattern (anti-unified cxn)
            (pattern-top-args (extract-top-lvl-meaning-args anti-unified-cxn))
            ;; for now, this is always empty... but it will be used when adding recursion
            (source-slot-args (find-data source-args :slot-meaning-args))
            (source-top-args (or (find-data source-args :top-lvl-meaning-args)
                                 ;; compute entirely new top args, like for a holophrase cxn
                                 ;; by putting together the source delta and the generalisation
                                 (holistic-meaning-top-args
                                  (append (substitute-bindings (fcg::reverse-bindings source-bindings) generalisation) source-delta)
                                  (get-configuration (cxn-inventory anti-unified-cxn) :meaning-representation-formalism)))))
        ;; loop through both bindings lists (pattern and source) at the same time
        ;; whenever a variable from the bindings list is used in the delta
        ;; OR that variable is a slot-arg
        ;; OR that variable occurs more than once in the bindings (decoupled link)
        ;; THEN use that variable as an arg 
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              ;when (or (find-anywhere pattern-var pattern-delta)
              ;         (find-anywhere source-var source-delta)
              ;         (find pattern-var pattern-slot-args)
              ;         (find source-var source-slot-args)
              ;         (> (count pattern-var pattern-bindings :key #'car) 1)
              ;         (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        ;; when anti-unifying with a holistic cxn, the pattern delta cxn will be item-based
        ;; and the top-args/slot-args are reversed, so pass on the top-lvl args of the
        ;; previous pattern as top-lvl args for the next pattern

        ;; when anti-unifying with an item-based cxn, the pattern delta cxn will also be
        ;; item-based, so pass on the slot args of the previous pattern as the slot args
        ;; for the next pattern
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn) pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn) source-top-args)
                        ((item-based-cxn-p anti-unified-cxn) source-slot-args)))
        (set-data args :generalisation-top-lvl-args
                  (loop for arg in pattern-top-args
                        collect (or (rest (assoc arg pattern-bindings)) arg)))))
    args))
|#
