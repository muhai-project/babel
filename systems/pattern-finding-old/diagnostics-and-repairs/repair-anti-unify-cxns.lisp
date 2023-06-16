(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair anti-unify cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass anti-unify-cxns (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cxns)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by anti-unifying the observation with the subset of cxns
   that results in the smallest generalisation."
  (let ((cxns-and-categorial-links (create-cxns-by-anti-unification problem node)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defun create-cxns-by-anti-unification (problem node)
  (do-repair
   (get-data problem :utterance)
   (get-data problem :meaning)
   (make-blackboard)
   (construction-inventory node)
   node
   'anti-unify-cxns))


(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'anti-unify-cxns)))
  (when (constructions cxn-inventory)
    (let ((new-cxns-and-links (find-cxns-and-anti-unify observation-form observation-meaning args (original-cxn-set cxn-inventory))))
      (when new-cxns-and-links
        (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add cat-links-to-add) new-cxns-and-links
          (apply-fix observation-form
                     cxns-to-apply
                     cat-links-to-add
                     cxns-to-consolidate
                     cats-to-add
                     (extract-contributing-lex-class (last-elt cxns-to-apply))
                     t
                     node
                     repair-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cxns and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-cxns-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set))
  "Given form and meaning of an observation and a cxn inventory,
   find the cxn that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (let* (;; 1) select cxns by hasing the observation
         ;;    only form is provided since we are learning in comprehension
         (hash-compatible-cxns
          (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
         
         ;; 2) filter hash-compatible cxns for routine cxns with a positive score
         (filtered-hash-compatible-cxns
          (remove-if-not #'non-zero-cxn-p
                         (remove-if-not #'routine-cxn-p
                                        hash-compatible-cxns)))
           
         ;; 3) find the least general generalisation through anti-unification
         (least-general-generalisation
          (loop with sorted-cxns = (sort filtered-hash-compatible-cxns #'> :key #'get-cxn-score)
                with max-au-cost = (get-configuration cxn-inventory :max-au-cost)
                for cxn in sorted-cxns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cxn args max-au-cost)
                ;; returns all valid meaning anti unification results
                for meaning-anti-unification-results
                  = (anti-unify-meaning observation-meaning cxn args max-au-cost)
                ;; make all combinations and filter for valid combinations
                for all-anti-unification-combinations
                  = (remove-if-not #'valid-au-combination-p
                                   (combinations meaning-anti-unification-results
                                                 form-anti-unification-results))
                when all-anti-unification-combinations
                ;; store all valid combinations with the cxn used for anti unification
                append (loop for combo in all-anti-unification-combinations
                             collect (cons cxn combo))
                into anti-unification-results
                ;; return the best anti unification combination (costs and cxn score)
                finally (return (first (sort-anti-unification-combinations anti-unification-results))))))
    ;; 4) learn cxn(s) from the anti-unification results
    (when least-general-generalisation
      (cond ((and (au-all-parts-present-p (second least-general-generalisation))
                  (au-all-parts-present-p (third least-general-generalisation)))
             (make-cxns-from-generalisations least-general-generalisation cxn-inventory))
            ((and (au-partial-analysis-p (second least-general-generalisation))
                  (au-partial-analysis-p (third least-general-generalisation)))
             (make-cxns-from-partial-analysis least-general-generalisation cxn-inventory))))))

;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify utils ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun remove-arg-predicates (set-of-predicates)
  "Remove both top-args and slot-args predicates
   from the set of predicates."
  (remove 'top-arg (remove 'slot-arg set-of-predicates :key #'first) :key #'first))

(defun au-all-parts-present-p (au-result)
  "All parts of the anti-unification result
   (i.e. generalisation and both delta's)
   are non-empty!"
  (and (generalisation au-result)
       (source-delta au-result)
       (remove-arg-predicates (pattern-delta au-result))))

(defun au-partial-analysis-p (au-result)
  "The anti-unification result can be used as a partial
   analysis when the generalisation and the source-delta
   are non-empty, the pattern delta is empty (excluding args),
   and the pattern bindings list is a renaming."
  (and (generalisation au-result)
       (source-delta au-result)
       (null (remove-arg-predicates (pattern-delta au-result)))
       (renamingp (pattern-bindings au-result))))

(defun valid-au-result-p (au-result)
  ;; valid when all parts are filled in
  ;; or when the pattern delta is empty (excluding args)
  ;; and the pattern bindings is a renaming
  (or
   (au-all-parts-present-p au-result)
   (au-partial-analysis-p au-result)))

(defun add-form-arg-predicates-from-cxn (set-of-predicates cxn)
  (loop for arg in (remove-duplicates (extract-top-lvl-form-args cxn))
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (remove-duplicates (extract-slot-form-args cxn))
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defun add-form-arg-predicates-from-args (set-of-predicates args)
  (loop for arg in (or (find-data args :top-lvl-form-args)
                       (get-unconnected-vars set-of-predicates))
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-form-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defun anti-unify-form (source-form cxn args &optional max-au-cost)
  "Anti-unify the observation with the given cxn on the form side."
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-form and pattern-form! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-form (extract-form-predicates cxn))
         (pattern-form-with-args (add-form-arg-predicates-from-cxn pattern-form cxn))
         (source-form-with-args (add-form-arg-predicates-from-args source-form args))
         (anti-unification-results
          (anti-unify-predicate-network (fresh-variables pattern-form-with-args) source-form-with-args))
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))

(defun add-meaning-arg-predicates-from-cxn (set-of-predicates cxn)
  (loop for arg in (remove-duplicates (extract-top-lvl-meaning-args cxn))
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (remove-duplicates (extract-slot-meaning-args cxn))
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defun add-meaning-arg-predicates-from-args (set-of-predicates args)
  (loop for arg in (or (find-data args :top-lvl-meaning-args)
                       (get-unconnected-vars set-of-predicates))
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-meaning-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defun anti-unify-meaning (source-meaning cxn args &optional max-au-cost)
  "Anti-unify the observation with the given cxn on the meaning side."
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-meaning and pattern-meaning! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-meaning (extract-meaning-predicates cxn))
         (pattern-meaning-with-args (add-meaning-arg-predicates-from-cxn pattern-meaning cxn))
         (source-meaning-with-args (add-meaning-arg-predicates-from-args source-meaning args))
         (anti-unification-results
          (anti-unify-predicate-network (fresh-variables pattern-meaning-with-args) source-meaning-with-args))
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))

(defun valid-au-combination-p (au-combination)
  ;; valid when both au results have both parts filled in
  ;; or both au results have both empty pattern delta's
  (or
   (and (au-all-parts-present-p (first au-combination))
        (au-all-parts-present-p (second au-combination)))
   (and (au-partial-analysis-p (first au-combination))
        (au-partial-analysis-p (second au-combination)))))

(defun sort-anti-unification-combinations (list-of-anti-unification-combinations)
  "Sort the anti-unifcation results based on cost (of both form- and
   meaning-anti-unification) and avg cxn score as a tie breaker."
  (sort list-of-anti-unification-combinations
        #'(lambda (combo-1 combo-2)
            (let ((combined-cost-1 (+ (cost (second combo-1)) (cost (third combo-1))))
                  (combined-cost-2 (+ (cost (second combo-2)) (cost (third combo-2))))
                  (cxn-score-1 (get-cxn-score (first combo-1)))
                  (cxn-score-2 (get-cxn-score (first combo-2))))
              (if (= combined-cost-1 combined-cost-2)
                (> cxn-score-1 cxn-score-2)
                (< combined-cost-1 combined-cost-2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-generalisations (anti-unification-results cxn-inventory)
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-args form-anti-unification))
           (meaning-args (compute-args meaning-anti-unification))
           ;; dispatch to helper functions to make generalisation-cxn and delta cxns
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (remove-arg-predicates (generalisation form-anti-unification))
                                     (remove-arg-predicates (generalisation meaning-anti-unification))
                                     (get-data form-args :generalisation-top-lvl-args)
                                     (get-data meaning-args :generalisation-top-lvl-args)
                                     (get-data form-args :generalisation-slot-args)
                                     (get-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           (source-delta-cxns-and-categories
            (make-holistic-cxn (source-delta form-anti-unification)
                               (source-delta meaning-anti-unification)
                               (get-data form-args :source-top-lvl-args)
                               (get-data meaning-args :source-top-lvl-args)
                               cxn-inventory))
           ;; after anti-unifying with an item-based cxn
           ;; the cxn for the pattern delta should also be
           ;; an item-based cxn with a slot that takes
           ;; the same fillers as the slot of the item-based
           ;; cxn used for anti-unification!
           (pattern-delta-cxns-and-categories
            (if (holistic-cxn-p anti-unified-cxn)
              (make-holistic-cxn (remove-arg-predicates (pattern-delta form-anti-unification))
                                 (remove-arg-predicates (pattern-delta meaning-anti-unification))
                                 (get-data form-args :pattern-top-lvl-args)
                                 (get-data meaning-args :pattern-top-lvl-args)
                                 cxn-inventory)
              (make-generalisation-cxn (remove-arg-predicates (pattern-delta form-anti-unification))
                                       (remove-arg-predicates (pattern-delta meaning-anti-unification))
                                       (get-data form-args :pattern-top-lvl-args)
                                       (get-data meaning-args :pattern-top-lvl-args)
                                       (get-data form-args :pattern-slot-args)
                                       (get-data meaning-args :pattern-slot-args)
                                       cxn-inventory)))
           ;; build result
           (cxns-to-apply
            (append (first source-delta-cxns-and-categories)
                    (list (first generalisation-cxns-and-categories))))
           (cxns-to-consolidate
            (append (list (second generalisation-cxns-and-categories))
                    (second source-delta-cxns-and-categories)
                    (if (holistic-cxn-p anti-unified-cxn)
                      (append (first pattern-delta-cxns-and-categories)
                              (second pattern-delta-cxns-and-categories))
                      (list (first pattern-delta-cxns-and-categories)
                            (second pattern-delta-cxns-and-categories)))))
           (categories-to-add
            (append (list (third generalisation-cxns-and-categories))
                    (list (fourth generalisation-cxns-and-categories))
                    (third source-delta-cxns-and-categories)
                    (if (holistic-cxn-p anti-unified-cxn)
                      (third pattern-delta-cxns-and-categories)
                      (list (third pattern-delta-cxns-and-categories)
                            (fourth pattern-delta-cxns-and-categories)))))
           (links-to-add
            (if (holistic-cxn-p anti-unified-cxn)
              (append (fourth pattern-delta-cxns-and-categories)
                      (fourth source-delta-cxns-and-categories)
                      (list (cons (fourth generalisation-cxns-and-categories)
                                  (first (third pattern-delta-cxns-and-categories)))
                            (cons (fourth generalisation-cxns-and-categories)
                                  (first (third source-delta-cxns-and-categories)))))
              (append (fourth source-delta-cxns-and-categories)
                      (list (cons (fourth generalisation-cxns-and-categories)
                                  (first (third source-delta-cxns-and-categories)))
                            (cons (fourth generalisation-cxns-and-categories)
                                  (third pattern-delta-cxns-and-categories)))
                      ;; !!! for the moment, always 1 slot, so only 1 lex class!!
                      (loop for n in (neighbouring-categories (first (extract-lex-class-slot-item-based-cxn anti-unified-cxn))
                                                              (categorial-network cxn-inventory))
                            collect (cons n (fourth pattern-delta-cxns-and-categories)))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make generalisation cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-generalisation-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; find an identical existing item-based cxn
         (existing-routine-item-based-cxn
          (find-identical-item-based-cxn form meaning top-lvl-form-args top-lvl-meaning-args
                                         slot-form-args slot-meaning-args cxn-inventory))
         (existing-meta-item-based-cxn
          (when existing-routine-item-based-cxn
            (alter-ego-cxn existing-routine-item-based-cxn cxn-inventory)))
         ;; lex classes
         (lex-class-item-based
          (if existing-routine-item-based-cxn
            (extract-lex-class-item-based-cxn existing-routine-item-based-cxn)
            (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t)))
         (lex-class-slot
          (if existing-routine-item-based-cxn
            (first (extract-lex-class-slot-item-based-cxn existing-routine-item-based-cxn))  ;; !!!
            (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t :slotp t)))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; build cxns!
         (item-based-cxn-apply-last
          (or existing-routine-item-based-cxn
              (item-based-cxn-apply-last-skeleton bare-cxn-name cxn-name-apply-last
                                                  lex-class-item-based lex-class-slot
                                                  form meaning
                                                  top-lvl-form-args top-lvl-meaning-args
                                                  slot-form-args slot-meaning-args
                                                  (get-configuration cxn-inventory :initial-cxn-score)
                                                  cxn-inventory-copy)))
         (item-based-cxn-apply-first
          (or existing-meta-item-based-cxn
              (item-based-cxn-apply-first-skeleton bare-cxn-name cxn-name-apply-first
                                                   lex-class-item-based lex-class-slot
                                                   form meaning
                                                   top-lvl-form-args top-lvl-meaning-args
                                                   slot-form-args slot-meaning-args
                                                   (get-configuration cxn-inventory :initial-cxn-score)
                                                   cxn-inventory-copy))))
    ;; done!
    (list item-based-cxn-apply-last
          item-based-cxn-apply-first
          lex-class-item-based
          lex-class-slot)))

;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;
        
(defun make-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  (let* (;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :holistic-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn form meaning form-args meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; lex class
         (lex-class-holistic-cxn
          (if existing-routine-holistic-cxn
            (extract-lex-class-holistic-cxn existing-routine-holistic-cxn)
            (make-lex-class cxn-name :trim-cxn-suffix t :numeric-suffix t)))
         ;; temp cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first lex-class-holistic-cxn
                                                 form meaning form-args meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 nil cxn-inventory-copy)))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last lex-class-holistic-cxn
                                                form meaning form-args meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                nil cxn-inventory-copy)))
         ;; build the result
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn))
         (links-to-add nil))
    ;; done!
    (list cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-partial-analysis (anti-unification-results cxn-inventory)
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (break)))