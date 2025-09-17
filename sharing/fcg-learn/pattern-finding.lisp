(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;; Anti-unification-based grammar induction       ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Learning through anti-unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun learn-through-anti-unification (speech-act cip)
  "Learn constructions through anti-unification."

  ;; To each branch, try to apply the holphrase-cxns of the grammar.
  (extend-cip-with-holophrase-cxns cip)
  
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (applicable-non-linking-cxns (remove-duplicates (mapcar #'original-cxn (mappend #'applied-constructions (remove-if-not
                                                                                                                  #'(lambda (node)
                                                                                                                      (find 'cxn-applied (statuses node)))
                                                                                                                  (children (top-node cip)))))
                                                         :test #'other-cxn-w-same-form-and-meaning-p))
         (all-non-linking-non-applicable-cxns (remove-duplicates (remove-if #'(lambda (cxn)
                                                                                (or (eql (type-of cxn) 'linking-cxn)
                                                                                    (find (name cxn) applicable-non-linking-cxns :key #'name)
                                                                                    (find cxn applicable-non-linking-cxns
                                                                                          :test #'other-cxn-w-same-form-and-meaning-p)))
                                                                            (constructions-list cxn-inventory))
                                                                 :test #'other-cxn-w-same-form-and-meaning-p)))
    
    ;; returns list anti-unification solutions, each with its own fix cxn-inventory
    (search-anti-unification-fix speech-act applicable-non-linking-cxns all-non-linking-non-applicable-cxns cxn-inventory)))


(defun search-anti-unification-fix (speech-act applicable-non-linking-cxns all-non-linking-cxns cxn-inventory)
  "Searches for possible anti-unification fixes based on speech-act,
applicable non-linking constructions and all non-linking
non-applicable constructions."
  (loop with initial-state = (make-instance 'au-repair-state
                                            :all-cxns all-non-linking-cxns
                                            :remaining-applicable-cxns applicable-non-linking-cxns
                                            :remaining-form-speech-act (list (list 'sequence (form speech-act) 0 (length (form speech-act))))
                                            :remaining-meaning-speech-act (fresh-variables (pn::variablify-predicate-network
                                                                                            (meaning speech-act)
                                                                                            (get-configuration cxn-inventory :meaning-representation-format)))
                                            :fix-cxn-inventory (copy-fcg-construction-set-without-cxns cxn-inventory)
                                            :created-at 1)
        with au-repair-processor = (make-instance 'au-repair-processor
                                                  :top-state initial-state
                                                  :queue (list initial-state)
                                                  :all-states (list initial-state))
        initially (setf (au-repair-processor initial-state) au-repair-processor)
        while (queue au-repair-processor)
        for current-state = (pop (queue au-repair-processor))
        for new-states = (extend-au-repair-state current-state)
        do (loop for new-state in new-states
                 unless (find new-state (all-states au-repair-processor) :test #'equivalent-state)
                   do (push new-state (children current-state))
                      (setf (all-parents new-state) (cons current-state (all-parents current-state)))
                      (push new-state (all-states au-repair-processor))
                      (setf (new-cxns new-state) (loop for cxn in (constructions-list (fix-cxn-inventory new-state))
                                                       unless (find (name cxn) (constructions-list (fix-cxn-inventory current-state)) :key #'name)
                                                         collect cxn))
                      (setf (created-at new-state) (incf (state-counter au-repair-processor)))
                      (setf (au-repair-processor new-state) au-repair-processor)
                   and
                   if (and (remaining-form-speech-act new-state)
                          (remaining-meaning-speech-act new-state))
                     do (push new-state (queue au-repair-processor))
                   else
                     unless (or (remaining-form-speech-act new-state)
                                (remaining-meaning-speech-act new-state))
                       do (push new-state (succeeded-states au-repair-processor)))
        finally (return (values (succeeded-states au-repair-processor) au-repair-processor))))


(defun extend-au-repair-state (current-state)
  "Extend current state with possible expansions resulting from anti-unification results."
  (let* ((form-au-meaning-au-combinations (loop for applicable-cxn in (remove-duplicates (remaining-applicable-cxns current-state) :test #'eql :key #'name)
                                                for au-form-results = (anti-unify-form (attr-val applicable-cxn :form)
                                                                                       (remaining-form-speech-act current-state) 
                                                                                       :regex-subsequence
                                                                                       (second (get-configuration (fix-cxn-inventory current-state)
                                                                                                          :form-generalisation-mode)))
                                                for au-meaning-results = (anti-unify-meaning (attr-val applicable-cxn :meaning)
                                                                                             (remaining-meaning-speech-act current-state)
                                                                                             (get-configuration (fix-cxn-inventory current-state)
                                                                                                                :meaning-generalisation-mode)
                                                                                             :cxn-inventory (fix-cxn-inventory current-state))
                                                append (loop for combination in (cartesian-product au-form-results au-meaning-results)
                                                             collect (cons applicable-cxn combination))))
         (valid-au-combinations (loop for au-combination in form-au-meaning-au-combinations
                                      when (and (generalisation (second au-combination))
                                                (generalisation (third au-combination)))
                                        collect au-combination)))
    
    (loop for (cxn au-form-result au-meaning-result) in valid-au-combinations
          for fix-cxn-inventory = (copy-object (fix-cxn-inventory current-state))
          for (resulting-integration-cat
               resulting-integration-form-args
               resulting-integration-meaning-args) = (multiple-value-list
                                                      (learn-cxns-from-au-result au-form-result au-meaning-result fix-cxn-inventory
                                                                                 :integration-cat (integration-cat current-state)
                                                                                 :integration-form-args (integration-form-args current-state)
                                                                                 :integration-meaning-args (integration-meaning-args current-state)))
          when (> (size fix-cxn-inventory) (size (fix-cxn-inventory current-state)))
            collect (make-instance 'au-repair-state
                                   :all-cxns (all-cxns current-state)
                                   :remaining-applicable-cxns (remove (name cxn) (remaining-applicable-cxns current-state) :key #'name)
                                   :remaining-form-speech-act (source-delta au-form-result)
                                   :remaining-meaning-speech-act (source-delta au-meaning-result)
                                   :integration-cat resulting-integration-cat
                                   :integration-form-args resulting-integration-form-args
                                   :integration-meaning-args resulting-integration-meaning-args
                                   :base-cxn cxn
                                   :au-result-form au-form-result
                                   :au-result-meaning au-meaning-result
                                   :fix-cxn-inventory fix-cxn-inventory)
              into new-states
          finally (let ((states-from-cxn-inventory (learn-from-cxn-inventory (remaining-form-speech-act current-state)
                                                                             (remaining-meaning-speech-act current-state)
                                                                             (integration-cat current-state) (integration-form-args current-state)
                                                                             (integration-meaning-args current-state)
                                                                             (fix-cxn-inventory current-state)
                                                                             (all-cxns current-state))))
                    (setf new-states (append new-states states-from-cxn-inventory)))
                  (return new-states))))

(defun learn-from-cxn-inventory (form-predicates-speech-act
                                 meaning-predicates-speech-act
                                 integration-cat integration-form-args
                                 integration-meaning-args parent-cxn-inventory
                                 all-cxns)
  "Learn cxns based on cxns from cxn-inventory which could not apply to the root."
  (loop for cxn in (find-all-if #'(lambda (cxn) (= (length (attr-val cxn :form)) 1)) all-cxns) ;; only keep cxns with single form predicate
        ;;FIRST check meaning au result!!
        for au-meaning-results = (anti-unify-meaning (attr-val cxn :meaning)
                                                     meaning-predicates-speech-act
                                                     (get-configuration parent-cxn-inventory :meaning-generalisation-mode)
                                                     :cxn-inventory parent-cxn-inventory)
        for valid-au-meaning-results = (loop for au-meaning-result in au-meaning-results
                                             for  nr-of-chunks = (second (multiple-value-list
                                                                          (connected-semantic-network (generalisation au-meaning-result))))
                                             when (or (= nr-of-chunks 1)
                                                      (= nr-of-chunks 2))
                                               collect au-meaning-result)
        when valid-au-meaning-results
          append (let* ((au-form-results (anti-unify-form (attr-val cxn :form)
                                                          form-predicates-speech-act 
                                                          (first (get-configuration parent-cxn-inventory :form-generalisation-mode))
                                                          (second (get-configuration parent-cxn-inventory
                                                                                     :form-generalisation-mode))))
                        (valid-au-form-results (loop for au-form-result in au-form-results
                                                     when (and (generalisation au-form-result)
                                                               (<= (length (find-all-if #'(lambda (p)
                                                                                            (eql 'sequence (first p)))
                                                                                        (generalisation au-form-result)))
                                                                   2)) ;; max one gap allowed, i.e. two sequence predicates
                                                       collect au-form-result)))
                   (when valid-au-form-results
                     (loop for (au-form au-meaning) in (cartesian-product valid-au-form-results valid-au-meaning-results)
                           for fix-cxn-inventory = (copy-object parent-cxn-inventory)
                           do 
                             (learn-cxns-from-au-result au-form au-meaning fix-cxn-inventory
                                                        :integration-cat integration-cat
                                                        :integration-form-args integration-form-args
                                                        :integration-meaning-args integration-meaning-args
                                                        :learn-cxns-from-deltas t)
                           when (> (size fix-cxn-inventory) (size parent-cxn-inventory)) ;; new cxns were learnt
                             collect (make-instance 'au-repair-state
                                                    :base-cxn cxn
                                                    :fix-cxn-inventory fix-cxn-inventory
                                                    :au-result-form au-form
                                                    :au-result-meaning au-meaning)))) into new-states
        finally (return (cons (make-instance 'au-repair-state
                                             :base-cxn nil
                                             :fix-cxn-inventory (learn-cxn-from-form-and-meaning-predicates
                                                                 form-predicates-speech-act meaning-predicates-speech-act integration-cat
                                                                 integration-form-args integration-meaning-args parent-cxn-inventory))
                              new-states))))
      

(defun equivalent-state (state-1 state-2)
  "Checks whether two au-repair-states are equivalent."
  (let ((cxn-inventory-1 (fix-cxn-inventory state-1))
        (cxn-inventory-2 (fix-cxn-inventory state-2)))

    (and (permutation-of? (remaining-meaning-speech-act state-1) (remaining-meaning-speech-act state-2) :test #'equalp)
         (pn::equivalent-predicate-networks-p (remaining-form-speech-act state-1) (remaining-form-speech-act state-2))
         (permutation-of? (constructions-list cxn-inventory-1) (constructions-list cxn-inventory-2) :test #'equivalent-cxn))))


(defun learn-cxns-from-au-result (au-form au-meaning fix-cxn-inventory &key integration-cat integration-form-args integration-meaning-args
                                          learn-cxns-from-deltas)
  "Learns cxns from anit-unification result."
  ;; Delta's can be empty, but this should be consistent at both sides (e.g. if source-delta is empty on the form side, it should also be empty
  ;; on the meaning side).
  (if (and (if (source-delta au-form)
             (source-delta au-meaning)
             (not (source-delta au-meaning)))
           (if (pattern-delta au-form)
             (pattern-delta au-meaning)
             (not (pattern-delta au-meaning))))
  ;; Compute form args for filler-cxns (generalisation, pattern and source)
  (multiple-value-bind (generalisation-form-args pattern-form-args source-form-args)
      (compute-filler-args au-form)
    ;; Compute meaning args for filler-cxns (generalisation, pattern and source)
    (multiple-value-bind (generalisation-meaning-args pattern-meaning-args source-meaning-args)
        (compute-filler-args au-meaning)
      (let* (;; Relate passed integration form args to current au-result
             (integration-form-args-slot-1 (compute-linking-args-slots integration-form-args au-form 1))
             (integration-form-args-slot-2 (compute-linking-args-slots integration-form-args au-form 2))
             (integration-meaning-args-slot-1 (compute-linking-args-slots integration-meaning-args au-meaning 1))
             (integration-meaning-args-slot-2 (compute-linking-args-slots integration-meaning-args au-meaning 2))
             
             ;; Compute args for contributing unit of linking-cxn
             (linking-cxn-contributing-meaning-args (compute-linking-args-contributing integration-meaning-args integration-meaning-args-slot-1
                                                                                       integration-meaning-args-slot-2 au-meaning))
             (linking-cxn-contributing-form-args (compute-linking-args-contributing integration-form-args integration-form-args-slot-1
                                                                                    integration-form-args-slot-2 au-form))
             ;; Compute args for conditional units of linking-cxn

             (linking-cxn-index-renamings (loop for arg in linking-cxn-contributing-form-args
                                                unless (variable-p arg)
                                                  collect (cons arg (make-var "LR"))))

             (linking-cxn-contributing-form-args-without-indices (substitute-bindings-including-constants linking-cxn-index-renamings linking-cxn-contributing-form-args))
             (linking-cxn-form-args-slot-1 (append generalisation-form-args (substitute-bindings-including-constants linking-cxn-index-renamings integration-form-args-slot-1)))
             (linking-cxn-meaning-args-slot-1 (append generalisation-meaning-args integration-meaning-args-slot-1))
             (linking-cxn-form-args-slot-2 (append generalisation-form-args (substitute-bindings-including-constants linking-cxn-index-renamings integration-form-args-slot-2)))
             (linking-cxn-meaning-args-slot-2 (append generalisation-meaning-args integration-meaning-args-slot-2))

             ;; Compute args for filler constructions (based on generalisation, source and pattern)
             (generalisation-filler-cxn-form-args (append generalisation-form-args integration-form-args-slot-1))
             (generalisation-filler-cxn-meaning-args (append generalisation-meaning-args integration-meaning-args-slot-1))
             (source-filler-cxn-form-args (append source-form-args integration-form-args-slot-2))
             (source-filler-cxn-meaning-args (append source-meaning-args integration-meaning-args-slot-2))
             (pattern-filler-cxn-form-args (append pattern-form-args integration-form-args-slot-2))
             (pattern-filler-cxn-meaning-args (append pattern-meaning-args integration-meaning-args-slot-2))
             
             ;; Variables that will hold the constructions
             (linking-cxn nil)
             (generalisation-filler-cxn nil)
             (pattern-filler-cxn nil)
             (source-filler-cxn nil))

        ;; If the contributing form args don't map on the integration-form-args (in which case they will be nil), stop and don't learn cxns
        (when (and (if integration-form-args linking-cxn-contributing-form-args-without-indices t)
                   (if integration-meaning-args linking-cxn-contributing-meaning-args t))

          ;; Learn linking-cxn
          (setf linking-cxn (create-linking-cxn :cxn-inventory fix-cxn-inventory
                                                :contributing-form-args linking-cxn-contributing-form-args-without-indices
                                                :contributing-meaning-args linking-cxn-contributing-meaning-args
                                                :form-args-slot-1 linking-cxn-form-args-slot-1
                                                :form-args-slot-2 linking-cxn-form-args-slot-2
                                                :meaning-args-slot-1 linking-cxn-meaning-args-slot-1
                                                :meaning-args-slot-2 linking-cxn-meaning-args-slot-2))

          ;; Learn filler-cxn from generalisation
          (setf generalisation-filler-cxn (if linking-cxn
                                            (create-filler-cxn (generalisation au-form) (generalisation au-meaning)
                                                               generalisation-filler-cxn-form-args generalisation-filler-cxn-meaning-args
                                                               fix-cxn-inventory)
                                            (create-filler-cxn (generalisation au-form) (generalisation au-meaning)
                                                               integration-form-args-slot-1 integration-meaning-args-slot-1
                                                                fix-cxn-inventory)))

          ;; Learn filler-cxn from pattern-delta
          (setf pattern-filler-cxn (when (and learn-cxns-from-deltas linking-cxn)
                                     (create-filler-cxn (pattern-delta au-form) (pattern-delta au-meaning)
                                                        pattern-filler-cxn-form-args pattern-filler-cxn-meaning-args fix-cxn-inventory)))

          ;; Learn filler-cxn from source-delta
          (setf source-filler-cxn (when (and learn-cxns-from-deltas linking-cxn)
                                    (create-filler-cxn (source-delta au-form) (source-delta au-meaning)
                                                       source-filler-cxn-form-args source-filler-cxn-meaning-args fix-cxn-inventory))))
          

        ;; Adding new constructions, categories and links to fix-cxn-inventory
        (when linking-cxn
          (add-cxn linking-cxn fix-cxn-inventory)
          (when (attr-val linking-cxn :cxn-cat)
            (add-category (attr-val linking-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
            (when integration-cat
              (add-link (attr-val linking-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil)))
          (add-categories (attr-val linking-cxn :slot-cats) fix-cxn-inventory :recompute-transitive-closure nil))
    
        (when generalisation-filler-cxn
          (add-cxn generalisation-filler-cxn fix-cxn-inventory)
          (add-category (attr-val generalisation-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (if linking-cxn
            (add-link (attr-val generalisation-filler-cxn :cxn-cat) (first (attr-val linking-cxn :slot-cats))
                      fix-cxn-inventory :recompute-transitive-closure nil)
            (when integration-cat
              (add-link (attr-val generalisation-filler-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil))))

        (when (and pattern-filler-cxn linking-cxn)
          (add-cxn pattern-filler-cxn fix-cxn-inventory)
          (add-category (attr-val pattern-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (add-link (attr-val pattern-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory :recompute-transitive-closure nil))

        (when (and source-filler-cxn linking-cxn)
          (add-cxn source-filler-cxn fix-cxn-inventory)
          (add-category (attr-val source-filler-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
          (add-link (attr-val source-filler-cxn :cxn-cat) (second (attr-val linking-cxn :slot-cats)) fix-cxn-inventory :recompute-transitive-closure nil))
    
        (values (when linking-cxn (second (attr-val linking-cxn :slot-cats))) ;;return values??
                (or (when (source-delta au-form) source-filler-cxn-form-args)
                    (when (pattern-delta au-form) pattern-filler-cxn-form-args))
                (or (when (source-delta au-form) source-filler-cxn-meaning-args)
                    (when (pattern-delta au-form) pattern-filler-cxn-meaning-args))))))
  (values nil nil nil)))

  
(defun compute-filler-args (au-result)
  "Computes the args to be used in the filler constructions based on au-result."
  (loop for (delta-var-source . gen-var-source) in (source-bindings au-result)
        for (delta-var-pattern . nil) in (pattern-bindings au-result)
        when (or (find delta-var-source (source-delta au-result) :test (lambda (x y) (member x y)))
                 (find delta-var-pattern (pattern-delta au-result) :test (lambda (x y) (member x y))))
          collect gen-var-source into generalisation-args
          and
          collect delta-var-pattern into pattern-args
          and
          collect delta-var-source into source-args
        finally (return (values generalisation-args pattern-args source-args ))))


(defun compute-linking-args-slots (integration-args au-result slot-id)
  "Computes args for slots of linking cxns."
  (case slot-id
    (1
     (loop for arg in integration-args
           for corresponding-arg-in-generalisation = (or (cdr (assoc arg (pattern-bindings au-result)))
                                                         (cdr (assoc arg (source-bindings au-result))))
           when (and corresponding-arg-in-generalisation
                     (find corresponding-arg-in-generalisation (generalisation au-result) :test #'member)) ;; is this test needed?
             collect corresponding-arg-in-generalisation))
    (2
     (loop for arg in integration-args
           when (find arg (or (pattern-delta au-result) (source-delta au-result)) :test #'member)
             collect arg))))


(defun compute-linking-args-contributing (integration-args integration-args-slot-1 integration-args-slot-2 au-result)
  "Computes args for contributing unit of linking cxns."
  (loop for arg in integration-args
        collect (cond ((or (find arg integration-args-slot-1)
                           (find arg integration-args-slot-2))
                       arg)
                      ((assoc arg (source-bindings au-result))
                       (cdr (assoc arg (source-bindings au-result))))
                      ((assoc arg (pattern-bindings au-result))
                       (cdr (assoc arg (pattern-bindings au-result))))
                      (t
                       (return nil)))))


(defun extend-cip-with-holophrase-cxns (cip)
  "Add applications of all holophrase-cxns as children to initial node of cip."
  (let* ((cxn-inventory (construction-inventory cip))
         (cxn-supplier-mode (get-configuration cxn-inventory :cxn-supplier-mode)))

    ;; Set cxn-supplier-mode
    (set-configuration cxn-inventory :cxn-supplier-mode :holophrase-cxns-only)

    ;; Enqueue again all nodes that were not duplicates, and reset their cxn-supplier slot
    (setf (cxn-supplier (top-node cip)) nil)
    (setf (fully-expanded? (top-node cip)) nil)
    (cip-enqueue (top-node cip) cip (get-configuration cxn-inventory :search-algorithm))

    ;; no solution can be found by appling additionnal holophrase-cxns, so the whole tree will be extended
    (next-cip-solution cip :notify nil)
    
    (set-configuration cxn-inventory :cxn-supplier-mode cxn-supplier-mode)))
 


;; Learning holophrastic constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-holophrastic-cxn (form-predicates meaning-predicates cxn-inventory)
  "Create a holophrastic construction based on form-predicates and meaning-predicates, returns the cxn."
  (let* ((form (second (first form-predicates)))
         (form-predicates-w-variables (list (list 'sequence form (make-var "LR") (make-var "LR"))))
         (initial-score 0.5)
         (meaning-hash-key (compute-meaning-hash-key-from-predicates meaning-predicates)))
    (make-instance 'holophrastic-cxn
                   :name (make-cxn-name form-predicates)
                   :conditional-part (list (make-instance 'conditional-unit
                                                          :name (make-var "holophrastic-unit")
                                                          :formulation-lock `((HASH meaning ,meaning-predicates))
                                                          :comprehension-lock `((HASH form ,form-predicates-w-variables))))
                   :cxn-inventory cxn-inventory
                   :feature-types (feature-types cxn-inventory)
                   :attributes `((:form . ,form-predicates-w-variables)
                                 (:meaning . ,meaning-predicates)
                                 (:entrenchment-score . ,initial-score)
                                 (:form-hash-key . ,form)
                                 (:meaning-hash-key . ,meaning-hash-key)))))


;; Learning filler constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-cxn-from-form-and-meaning-predicates (form-predicates meaning-predicates integration-cat integration-form-args integration-meaning-args cxn-inventory)
    "Returns a fix-cxn-inventory with a single holophrase or filler-cxn added based on form-predicates and meaning-predicates and optionally cat and args."
    (let* ((fix-cxn-inventory (copy-object cxn-inventory))
           (new-cxn (if integration-cat
                      (create-filler-cxn form-predicates meaning-predicates integration-form-args integration-meaning-args fix-cxn-inventory)
                      (create-holophrastic-cxn form-predicates meaning-predicates fix-cxn-inventory))))
      ;; Add cxn
      (add-cxn new-cxn fix-cxn-inventory)
      (when integration-cat
        (add-category (attr-val new-cxn :cxn-cat) fix-cxn-inventory :recompute-transitive-closure nil)
        (add-link (attr-val new-cxn :cxn-cat) integration-cat fix-cxn-inventory :recompute-transitive-closure nil))
      fix-cxn-inventory))


(defun create-filler-cxn (form-predicates meaning-predicates form-filler-args meaning-filler-args cxn-inventory)
  "Create a filler construction based on form-predicates and meaning-predicates and args, returns the cxn."
  (when (and form-predicates meaning-predicates)
    (let* ((initial-score 0.5)
           ;; filler-cxns learnt from source-delta can have indices in the form constraints
           ;; first, we make variable renamings for them
           (form-args-renamings (loop for arg in form-filler-args
                                      unless (variable-p arg)
                                        collect (cons arg (make-var "TEST"))))
           ;; then we collect the form-filler args
           (form-filler-args (if form-args-renamings
                               (mapcar #'cdr form-args-renamings)
                               form-filler-args))
           
           ;; and the sequence predicates           
           (form-sequence-predicates (if form-args-renamings ;; only if resulting from source delta (thereby with no precedes)
                                       (loop for (nil string left right) in (subst-bindings form-predicates form-args-renamings)
                                             collect (list 'sequence
                                                           string
                                                           (if (variable-p left) left (make-var "TEST"))
                                                           (if (variable-p right) right (make-var "TEST"))))
                                       form-predicates)) ;; these also include precedes if we did not learn from source delta
                                     
           (form-precedes-predicates (when form-args-renamings ;; only treats precedes separately if learning from source delta
                                       (loop for p in form-predicates
                                             for p-with-vars in form-sequence-predicates
                                             for p-right = (fourth p)
                                             for higher-indexed-predicates = (loop for p2 in (remove p form-predicates :test #'equal)
                                                                                   for p2-with-vars in (remove p-with-vars form-sequence-predicates :test #'equal)
                                                                                   for p2-left = (third p2)
                                                                                   when (< p-right p2-left)
                                                                                     collect p2-with-vars)
                                               append (loop for higher-indexed-p in higher-indexed-predicates
                                                            collect (list 'precedes
                                                                          (fourth p-with-vars)
                                                                          (third higher-indexed-p))))))
           (form-predicates (append form-sequence-predicates form-precedes-predicates)) 
           (cxn-name (make-cxn-name form-predicates))
           (unit-name (make-filler-unit-name form-predicates))
           (filler-cat (make-const (upcase (format nil "~a-filler-cat" (remove-cxn-tail (symbol-name cxn-name)))))))

      (make-instance 'filler-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name unit-name
                                                             :unit-structure `((category ,filler-cat)
                                                                               ,@(when form-filler-args
                                                                                   `((form-args ,form-filler-args)))
                                                                               ,@(when meaning-filler-args
                                                                                   `((meaning-args ,meaning-filler-args))))))
                     :conditional-part `(,(make-instance 'conditional-unit
                                                         :name unit-name
                                                         :formulation-lock `((HASH meaning ,meaning-predicates))
                                                         :comprehension-lock `((HASH form ,form-predicates))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `((:form . ,form-predicates)
                                   (:meaning . ,meaning-predicates)
                                   ,@(when form-filler-args
                                       `((:form-args . ,form-filler-args)))
                                   ,@(when meaning-filler-args
                                       `((:meaning-args . ,meaning-filler-args)))
                                   (:cxn-cat . ,filler-cat)
                                   (:entrenchment-score . ,initial-score))))))


;; Learning linking constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-linking-cxn (&key (cxn-inventory *fcg-constructions*) contributing-form-args contributing-meaning-args
                                form-args-slot-1 form-args-slot-2 meaning-args-slot-1 meaning-args-slot-2)
  "Create a linking construction based on conditional and contributing args, returns the cxn."
  (when (and form-args-slot-1 meaning-args-slot-1 form-args-slot-2 meaning-args-slot-2)
    (let* ((cxn-name (make-id 'linking-cxn))
           (cxn-cat (make-id 'cxn-cat))
           (slot-cat-1 (make-id 'slot-cat))
           (slot-cat-2 (make-id 'slot-cat))
           (parent-unit-name (make-var "linking-unit"))
           (slot-unit-1-name (make-var "slot-unit"))
           (slot-unit-2-name (make-var "slot-unit"))
           (initial-score 0.5))

      (make-instance 'linking-cxn
                     :name cxn-name
                     :contributing-part (list (make-instance 'contributing-unit
                                                             :name parent-unit-name
                                                             :unit-structure `((subunits ,(list slot-unit-1-name slot-unit-2-name))
                                                                               ,@(when (or contributing-form-args contributing-meaning-args)
                                                                                   `((category ,cxn-cat)))
                                                                               ,@(when contributing-form-args
                                                                                   `((form-args ,contributing-form-args)))
                                                                               ,@(when contributing-meaning-args
                                                                                   `((meaning-args ,contributing-meaning-args)))))
                                              (make-instance 'contributing-unit
                                                             :name slot-unit-1-name
                                                             :unit-structure `((footprints (linking-cxn))))
                                              (make-instance 'contributing-unit
                                                             :name slot-unit-2-name
                                                             :unit-structure `((footprints (linking-cxn)))))
                     :conditional-part (list (make-instance 'conditional-unit
                                                            :name slot-unit-1-name
                                                            :formulation-lock `((category ,slot-cat-1)
                                                                                ,@(when form-args-slot-1
                                                                                    `((form-args ,form-args-slot-1)))
                                                                                ,@(when meaning-args-slot-1
                                                                                    `((meaning-args ,meaning-args-slot-1))))
                                                            :comprehension-lock `((category ,slot-cat-1)
                                                                                  ,@(when form-args-slot-1
                                                                                      `((form-args ,form-args-slot-1)))
                                                                                  ,@(when meaning-args-slot-1
                                                                                      `((meaning-args ,meaning-args-slot-1)))
                                                                                  (footprints (NOT linking-cxn))))
                                             (make-instance 'conditional-unit
                                                            :name slot-unit-2-name
                                                            :formulation-lock `((category ,slot-cat-2)
                                                                                ,@(when form-args-slot-2
                                                                                    `((form-args ,form-args-slot-2)))
                                                                                ,@(when meaning-args-slot-2
                                                                                    `((meaning-args ,meaning-args-slot-2))))
                                                            :comprehension-lock `((category ,slot-cat-2)
                                                                                  ,@(when form-args-slot-2
                                                                                      `((form-args ,form-args-slot-2)))
                                                                                  ,@(when meaning-args-slot-2
                                                                                      `((meaning-args ,meaning-args-slot-2)))
                                                                                  (footprints (NOT linking-cxn)))))
                     :cxn-inventory cxn-inventory
                     :feature-types (feature-types cxn-inventory)
                     :attributes `(,@(when contributing-form-args
                                       `((:form-args . ,contributing-form-args)))
                                   ,@(when contributing-meaning-args
                                       `((:meaning-args . ,contributing-meaning-args)))
                                   ,@(when (or contributing-form-args contributing-meaning-args) `((:cxn-cat . ,cxn-cat)))
                                   ,@(when form-args-slot-1
                                       `((:form-args-slot-1 . ,form-args-slot-1)))
                                   ,@(when form-args-slot-2
                                       `((:form-args-slot-2 . ,form-args-slot-2)))
                                   ,@(when meaning-args-slot-1
                                       `((:meaning-args-slot-1 . ,meaning-args-slot-1)))
                                   ,@(when meaning-args-slot-2
                                       `((:meaning-args-slot-2 . ,meaning-args-slot-2)))
                                   (:slot-cats ,slot-cat-1 ,slot-cat-2)
                                   (:entrenchment-score . ,initial-score))))))


;; Anti-unfication ;;
  ;;;;;;;;;;;;;;;;;;;;;


(defgeneric anti-unify-form (cxn-form speech-act-form mode parameters &key &allow-other-keys)
  (:documentation "Anti-unification of form."))


(defmethod anti-unify-form ((cxn-form-predicates list)
                            (speech-act-sequence-predicates list)
                            (mode (eql :regex-subsequence))
                            (parameters list) &key &allow-other-keys)
  "Computes anti-unification results based on regex subsequence matching."
  ;; cxn-form-predicates can be combination of sequence and precedes predicates (always with variables as arguments)
  ;; speech-act-sequence-predicates can only be sequence predicates with instantiated argumants (numerical indices)$
  (let* ((cxn-sequence-predicates (find-all-if #'(lambda (p) (eql 'sequence (first p))) cxn-form-predicates))
         (cxn-precedes-predicates (find-all-if #'(lambda (p) (eql 'precedes (first p))) cxn-form-predicates))
         (possible-alignments-without-ordering-constraints (match-pattern-sequence-predicates-in-source-sequence-predicates cxn-sequence-predicates
                                                                                                                            speech-act-sequence-predicates
                                                                                                                            '(((T . T)))))
         (possible-alignments (loop for alignment-wo-precedes in possible-alignments-without-ordering-constraints
                                    for valid-alignment-w-precedes-p = (loop for (nil left right) in cxn-precedes-predicates
                                                                             for cxn-predicate-left-index = (car (rassoc left alignment-wo-precedes))
                                                                             for cxn-predicate-right-index = (car (rassoc right alignment-wo-precedes))
                                                                             when (and (numberp cxn-predicate-left-index)
                                                                                       (numberp cxn-predicate-right-index)
                                                                                       (> left right))
                                                                               do (return nil)
                                                                             finally (return t))
                                    when valid-alignment-w-precedes-p
                                      collect alignment-wo-precedes)))
    
    (loop for alignment in possible-alignments
          for source-delta = (recompute-root-sequence-features-based-on-bindings cxn-sequence-predicates speech-act-sequence-predicates alignment)
          for (generalisation pattern-bindings) = (multiple-value-list (fresh-variables cxn-form-predicates))
          for source-bindings = (loop for (pattern-var . gen-var) in pattern-bindings
                                      collect (cons (cdr (assoc pattern-var alignment)) gen-var))
          collect (make-instance 'sequences-au-result
                                 :pattern cxn-form-predicates 
                                 :source speech-act-sequence-predicates
                                 :generalisation generalisation ;; same sequence and precedes as in pattern
                                 :pattern-delta nil
                                 :source-delta source-delta  ;; instantiated sequences - variablification and precedes should happen later
                                 :pattern-bindings pattern-bindings
                                 :source-bindings source-bindings
                                 :cost (length source-delta)))))

;; (anti-unify-form *cxn-sequence-predicates* *speech-act-sequence-predicates* *mode* *parameters*)




(defmethod anti-unify-form ((cxn-sequence-predicates list)
                            (speech-act-sequence-predicates list)
                            (mode (eql :altschul-erickson))
                            (parameters list) &key &allow-other-keys)
  "Computes anti-unification results based on altschul-erickson string alignment with passed parameters."
  (loop with au-results = (anti-unify-sequences cxn-sequence-predicates speech-act-sequence-predicates
                                                :match-cost (cdr (assoc :match-cost parameters))
                                                :mismatch-cost (cdr (assoc :mismatch-cost parameters))
                                                :gap-opening-cost (cdr (assoc :gap-opening-cost parameters))
                                                :gap-cost  (cdr (assoc :gap-cost parameters))
                                                :remove-duplicate-alignments t
                                                :n-optimal-alignments (cdr (assoc :n-optimal-alignments parameters))
                                                :max-nr-of-au-gaps (cdr (assoc :max-nr-of-gaps parameters)))
        with cost = (when au-results (cost (first au-results)))
        for au-result in au-results
        if (= cost (cost au-result))
          collect au-result into au-form-results
        else
          do (return au-form-results)
        finally (return au-form-results)))


(defgeneric anti-unify-meaning (cxn-meaning speech-act-meaning mode &key cxn-inventory &allow-other-keys)
  (:documentation "Anti-unification of meaning."))

(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :k-swap)) &key &allow-other-keys)
  "Anti-unify meaning predicates using the k-swap algorithm."
  (loop with au-results = (au-lib:anti-unify-predicate-networks cxn-meaning-predicates speech-act-meaning-predicates :2-swap)
        with cost = (au-lib::cost (first au-results))
        for k-swap-au-result in au-results
        if (= cost (au-lib::cost k-swap-au-result))
          collect (make-instance 'predicate-network-au-result 
                                 :pattern (au-lib::pattern k-swap-au-result)
                                 :source (au-lib::source k-swap-au-result)
                                 :generalisation (au-lib::generalisation k-swap-au-result)
                                 :pattern-bindings (au-lib::pattern-bindings k-swap-au-result)
                                 :source-bindings (au-lib::source-bindings k-swap-au-result)
                                 :pattern-delta (au-lib::pattern-delta k-swap-au-result)
                                 :source-delta (au-lib::source-delta k-swap-au-result)
                                 :cost (au-lib::cost k-swap-au-result))
            into au-meaning-results
        else
          do (return au-meaning-results)
        finally (return au-meaning-results)))

  
  

(defmethod anti-unify-meaning ((cxn-meaning-predicates list)
                               (speech-act-meaning-predicates list)
                               (mode (eql :exhaustive)) &key &allow-other-keys)
  "Anti-unify meaning predicates using the exhaustive algorithm."
  (loop with au-results = (au-lib:anti-unify-predicate-networks cxn-meaning-predicates speech-act-meaning-predicates mode)
        with cost = (cost (first au-results))
        for au-result in au-results
        if (= cost (cost au-result))
          collect au-result into au-meaning-results
        else
          do (return au-meaning-results)
        finally (return au-meaning-results)))
  




