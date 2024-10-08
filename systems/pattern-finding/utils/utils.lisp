(in-package :pf)

;;;;;
;; Accessors and Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun get-cxn-score (cxn)
  (attr-val cxn :score))

(defun non-zero-cxn-p (cxn)
  (> (attr-val cxn :score) 0))

(defun routine-cxn-p (cxn)
  (or (eql (attr-val cxn :label) 'fcg::routine-apply-first)
      (eql (attr-val cxn :label) 'fcg::routine-apply-last)))

(defun meta-cxn-p (cxn)
  (or (eql (attr-val cxn :label) 'fcg::meta-apply-first)
      (eql (attr-val cxn :label) 'fcg::meta-apply-last)))

(defun item-based-cxn-p (cxn)
  (eql (attr-val cxn :cxn-type) 'item-based))

(defun holistic-cxn-p (cxn)
  (eql (attr-val cxn :cxn-type) 'holistic))

(defun holophrase-cxn-p (cxn)
  (eql (attr-val cxn :is-holophrase) t))

(defun routine-non-zero-cxns (agent)
  (remove-if-not #'non-zero-cxn-p
                 (remove-if-not #'routine-cxn-p
                                (constructions (grammar agent)))))

(defun get-cxns-of-type (agent type)
  (let ((found-cxns (if (eql type 'all)
                      (constructions-list (grammar agent))
                      (find-all type (constructions-list (grammar agent))
                                :key #'get-cxn-type))))
    (loop for cxn in found-cxns
          when (non-zero-cxn-p cxn)
          collect cxn)))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (if (routine-cxn-p cxn)
      (1- (length (conditional-part cxn)))
      (1- (length (contributing-part cxn))))))

(defun count-non-zero-holophrases (grammar)
  (count-if #'(lambda (cxn)
                (and (holophrase-cxn-p cxn)
                     (routine-cxn-p cxn)
                     (non-zero-cxn-p cxn)))
            (constructions grammar)))

(defmethod succeeded-cipn-p ((node cip-node))
  (find 'fcg::succeeded (statuses node)))


;;;;;
;; Search tree utils
;;;;;;;;;;;;;;;;;;;;

(defun initial-transient-structure (node)
  (if (find 'fcg::initial (statuses node))
    (car-source-cfs (cipn-car node))
    (car-source-cfs (cipn-car (last-elt (all-parents node))))))

(defun initial-node-p (node)
  "return t if node is initial node"
  (null (all-parents node)))

(defun all-leaf-nodes (cip)
  (remove nil (traverse-depth-first cip :collect-fn #'(lambda (node) (unless (children node) node)))))

(defun ignore-initial-nodes (cip-nodes)
  (loop for node in cip-nodes
        unless (find 'fcg::initial (statuses node))
        collect node))


;;;;;
;; Make cxn name
;;;;;;;;;;;;;;;;;;;;

(defgeneric make-cxn-name (thing cxn-inventory
                                 &key holistic-suffix item-based-suffix numeric-suffix))

(defmethod make-cxn-name ((string string) (cxn-inventory fcg-construction-set)
                          &key holistic-suffix item-based-suffix numeric-suffix)
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (when (and holistic-suffix item-based-suffix)
    (error "Cannot specify both holistic and item-based suffix"))
  (let ((name-string (upcase (substitute #\- #\Space string))))
    (when holistic-suffix
      (setf name-string (string-append name-string "-HOLISTIC-CXN")))
    (when item-based-suffix
      (setf name-string (string-append name-string "-ITEM-BASED-CXN")))
    (if numeric-suffix
      (make-id name-string)
      (intern name-string))))


(defmethod make-cxn-name ((form-constraints list) (cxn-inventory fcg-construction-set)
                          &key holistic-suffix item-based-suffix numeric-suffix)
  "Transform a list of form constraints into a suitable construction name"
  (make-cxn-name (format nil "~{~a~^-~}" (render form-constraints (get-configuration cxn-inventory :render-mode)))
                 cxn-inventory :holistic-suffix holistic-suffix
                 :item-based-suffix item-based-suffix
                 :numeric-suffix numeric-suffix))

;;;;;
;; Make unit name
;;;;;;;;;;;;;;;;;;;;

(defgeneric make-unit-name (thing cxn-inventory &key trim-cxn-suffix))

(defmethod make-unit-name ((symbol symbol) (cxn-inventory fcg-construction-set) &key trim-cxn-suffix)
  (make-unit-name (mkstr symbol) cxn-inventory :trim-cxn-suffix trim-cxn-suffix))

(defmethod make-unit-name ((string string) (cxn-inventory fcg-construction-set) &key trim-cxn-suffix)
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (when trim-cxn-suffix
    (setf string (string-replace string "-holistic-cxn" ""))
    (setf string (string-replace string "-item-based-cxn" "")))
  (variablify (intern (string-append (upcase string) "-UNIT"))))

(defmethod make-unit-name ((form-constraints list) (cxn-inventory fcg-construction-set) &key trim-cxn-suffix)
  "Transform a list of form constraints into a suitable construction name"
  (make-unit-name (format nil "~{~a~^-~}" (render form-constraints (get-configuration cxn-inventory :render-mode)))
                  cxn-inventory :trim-cxn-suffix trim-cxn-suffix))


;;;;;
;; Make grammatical category
;;;;;;;;;;;;;;;;;;;;

(defun replace-special-initial-chars (string)
  (if (member (char string 0) '(#\? #\! #\\))
    (string-append "-" string)
    string))
 
(defun make-grammatical-category (cat-name &key numeric-suffix trim-cxn-suffix slotp)
  (let* ((name-string
          (replace-special-initial-chars
           (string-downcase
            (if (equal (type-of cat-name) 'SYMBOL)
             (symbol-name cat-name)
             cat-name)))))
    (when trim-cxn-suffix
      (setf name-string (string-replace name-string "-holistic-cxn" ""))
      (setf name-string (string-replace name-string "-item-based-cxn" "")))
    (setf name-string
          (upcase (if slotp
                    (string-append name-string "-slot-cat")
                    (string-append name-string "-cat"))))
    (if numeric-suffix
      (setf name-string (make-const name-string))
      (setf name-string (make-symbol name-string)))
    (intern (string-downcase (symbol-name name-string)) :pattern-finding)))

;;;;
;; Extracting categories from units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extract-category-unit (unit)
  (:documentation "extract the category from a unit"))
  
(defmethod extract-category-unit ((unit contributing-unit))
  (first (fcg-unit-feature-value unit 'category)))

(defmethod extract-category-unit ((unit conditional-unit))
  (first (fcg-unit-feature-value unit 'category)))

(defmethod extract-category-unit ((unit list))
  (unit-feature-value unit 'category))

;;;;
;; Extracting form/meaning-args from units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extract-form-args-unit (unit)
  (:documentation "extract the form args from a unit"))

(defmethod extract-form-args-unit ((unit contributing-unit))
  (first (fcg-unit-feature-value unit 'form-args)))

(defmethod extract-form-args-unit ((unit conditional-unit))
  (first (fcg-unit-feature-value unit 'form-args)))

(defmethod extract-form-args-unit ((unit list))
  (unit-feature-value unit 'form-args))

(defgeneric extract-meaning-args-unit (unit)
  (:documentation "extract the meaning args from a unit"))

(defmethod extract-meaning-args-unit ((unit contributing-unit))
  (first (fcg-unit-feature-value unit 'meaning-args)))

(defmethod extract-meaning-args-unit ((unit conditional-unit))
  (first (fcg-unit-feature-value unit 'meaning-args)))

(defmethod extract-meaning-args-unit ((unit list))
  (unit-feature-value unit 'meaning-args))

;;;;;
;; Extracting units from cxns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-slot-units (cxn)
  (unless (holistic-cxn-p cxn)
    (if (routine-cxn-p cxn)
      (loop for unit in (conditional-part cxn)
            when (and (fcg-unit-feature unit 'meaning-args)
                      (fcg-unit-feature unit 'form-args))
            collect unit)
      (loop for unit in (contributing-part cxn)
            unless (fcg-unit-feature unit 'subunits)
            collect unit))))

;;;;;
;; Extracting categories from cxns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-top-category-holistic-cxn (cxn)
  "Extract the lex class from a holistic cxn.
   Works for both routine and meta cxns."
  (let ((unit-to-search
         (if (routine-cxn-p cxn)
           (first (contributing-part cxn))
           (first (conditional-part cxn)))))
    (extract-category-unit unit-to-search)))

(defun extract-slot-categories-item-based-cxn (cxn)
  "Extracts the lex classes from the slots of an item-based cxn.
   Works for both routine and meta cxns."
  (loop for unit in (extract-slot-units cxn)
        for category = (extract-category-unit unit)
        collect category))

(defun extract-top-category-item-based-cxn (cxn)
  (let ((unit-to-search
         (loop for unit in (contributing-part cxn)
               when (find 'subunits (fcg::unit-structure unit) :key #'first)
                 return unit)))
    (extract-category-unit unit-to-search)))

(defun extract-top-category-cxn (cxn)
  (if (holistic-cxn-p cxn)
    (extract-top-category-holistic-cxn cxn)
    (extract-top-category-item-based-cxn cxn)))

(defun extract-contributing-category (cxn)
  "return the category on the contributing part of a cxn
   works for both holistic and item-based cxns"
  (if (holistic-cxn-p cxn)
    (extract-top-category-holistic-cxn cxn)
    (extract-top-category-item-based-cxn cxn)))

(defun extract-conditional-categories (cxn)
  "return the categories on the conditional part of a cxn
   works only for item-based cxns"
  (unless (holistic-cxn-p cxn)
    (extract-slot-categories-item-based-cxn cxn)))

;;;;;
;; Extracting args from cxns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cxn-form-top-args (cxn &key by-category-p)
  (let ((units-to-search (if (or (routine-cxn-p cxn)
                                 (item-based-cxn-p cxn))
                           (contributing-part cxn)
                           (conditional-part cxn))))
    (loop for unit in units-to-search
          for form-args = (extract-form-args-unit unit)
          for category = (extract-category-unit unit)
          when (and form-args category)
          if by-category-p
            collect (cons category form-args)
          else append form-args)))
    
(defun cxn-meaning-top-args (cxn &key by-category-p)
  (let ((units-to-search (if (or (routine-cxn-p cxn)
                                 (item-based-cxn-p cxn))
                           (contributing-part cxn)
                           (conditional-part cxn))))
    (loop for unit in units-to-search
          for form-args = (extract-meaning-args-unit unit)
          for category = (extract-category-unit unit)
          when (and form-args category)
          if by-category-p
            collect (cons category form-args)
          else append form-args)))

(defun cxn-form-slot-args (cxn &key by-category-p)
  (unless (holistic-cxn-p cxn)
    (let ((units-to-search (if (routine-cxn-p cxn)
                             (conditional-part cxn)
                             (contributing-part cxn))))
      (loop for unit in units-to-search
            for form-args = (extract-form-args-unit unit)
            for category = (extract-category-unit unit)
            when (and form-args category)
            if by-category-p
            collect (cons category form-args)
            else append form-args))))

(defun cxn-meaning-slot-args (cxn &key by-category-p)
  (unless (holistic-cxn-p cxn)
    (let ((units-to-search (if (routine-cxn-p cxn)
                             (conditional-part cxn)
                             (contributing-part cxn))))
      (loop for unit in units-to-search
            for form-args = (extract-meaning-args-unit unit)
            for category = (extract-category-unit unit)
            when (and form-args category)
            if by-category-p
            collect (cons category form-args)
            else append form-args))))


;;;;;
;; Extracting args from cipns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-top-lvl-units (cipn)
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units)))
    (remove-child-units (remove root-unit ts-units))))

(defun get-open-slot-units (cipn)
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units))))
    (loop for unit in all-slot-units
          unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
          collect unit)))

(defun cipn-form-slot-args (cipn &key by-category-p)
  (loop for unit in (get-top-lvl-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
          collect (cons category (unit-feature-value unit 'form-args))
        else
          append (unit-feature-value unit 'form-args)))

(defun cipn-meaning-slot-args (cipn &key by-category-p)
  (loop for unit in (get-top-lvl-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'meaning-args))
        else
          append (unit-feature-value unit 'meaning-args)))

(defun cipn-form-top-args (cipn &key by-category-p)
  (loop for unit in (get-open-slot-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'form-args))
        else
          append (unit-feature-value unit 'form-args)))

(defun cipn-meaning-top-args (cipn &key by-category-p)
  (loop for unit in (get-open-slot-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'meaning-args))
        else
          append (unit-feature-value unit 'meaning-args)))

(defun group-cipn-args-by-unit (cipn lists-of-args)
  (loop for args in lists-of-args
        collect (loop for unit in (fcg-get-transient-unit-structure cipn)
                      when (or (equal (first (unit-feature-value unit 'form-args)) args)
                               (equal (first (unit-feature-value unit 'meaning-args)) args))
                      return (cons (extract-category-unit unit) args))))


;;;;;
;; Find identical holistic cxn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to do
;; cxns with negative scores _should_ be considered when looking for identical cxns
;; but they are not, given that they wont be applied by the cxn supplier anyway...

(defun equivalent-networks-and-args? (network cxn-network args cxn-args)
  (equivalent-irl-programs?
   (append network `((args ,@args)))
   (append cxn-network `((args ,@cxn-args)))))

(defun identical-holistic-cxn-p (form meaning form-args meaning-args cxn)
  (let ((form-top-args (cxn-form-top-args cxn))
        (meaning-top-args (cxn-meaning-top-args cxn)))
    (and (equivalent-irl-programs? form (extract-form-predicates cxn))
         (equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
         (length= form-args form-top-args)
         (length= meaning-args meaning-top-args)
         (equivalent-networks-and-args? form (extract-form-predicates cxn) form-args form-top-args)
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) meaning-args meaning-top-args))))

(defun find-identical-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  "Find a routine holistic cxn that is identical to the given form, meaning, and args"
  (let ((candidate-cxns
         (remove-if-not #'non-zero-cxn-p
                        (remove-if-not #'routine-cxn-p
                                       (remove-if-not #'holistic-cxn-p
                                                      (constructions cxn-inventory))))))
    (loop for cxn in candidate-cxns
          when (identical-holistic-cxn-p form meaning form-args meaning-args cxn)
          return cxn)))

;;;;;
;; Find identical item-based cxn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to do
;; cxns with negative scores _should_ be considered when looking for identical cxns
;; but they are not, given that they wont be applied by the cxn supplier anyway...

(defun identical-item-based-cxn-p (form meaning top-lvl-form-args top-lvl-meaning-args
                                   slot-form-args slot-meaning-args cxn)
  (let ((form-top-args (cxn-form-top-args cxn))
        (meaning-top-args (cxn-meaning-top-args cxn))
        (form-slot-args (cxn-form-slot-args cxn))
        (meaning-slot-args (cxn-meaning-slot-args cxn)))
    (and (equivalent-irl-programs? form (extract-form-predicates cxn))
         (equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
         (length= top-lvl-form-args form-top-args)
         (length= top-lvl-meaning-args meaning-top-args)
         (length= slot-form-args form-slot-args)
         (length= slot-meaning-args meaning-slot-args)
         (equivalent-networks-and-args? form (extract-form-predicates cxn) top-lvl-form-args form-top-args)
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) top-lvl-meaning-args meaning-top-args)
         (equivalent-networks-and-args? form (extract-form-predicates cxn) slot-form-args form-slot-args)
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) slot-meaning-args meaning-slot-args))))
                      

(defun find-identical-item-based-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  "Find a routine item-based cxn that is identical to the given form, meaning, and args"
  (let ((candidate-cxns
         (remove-if-not #'non-zero-cxn-p
                        (remove-if-not #'routine-cxn-p
                                       (remove-if #'holistic-cxn-p
                                                  (constructions cxn-inventory))))))
    (loop for cxn in candidate-cxns
          when (identical-item-based-cxn-p form meaning top-lvl-form-args top-lvl-meaning-args
                                           slot-form-args slot-meaning-args cxn)
          return cxn)))


;;;;;
;; Get the alter-ego cxn
;;;;;;;;;;;;;;;;;;;;;;;;

(defun alter-ego-cxn (original-cxn cxn-inventory)
  "Given a routine cxn, return its meta counterpart
   or vice-versa."
  (when (attr-val original-cxn :bare-cxn-name)
    (loop for cxn in (constructions cxn-inventory)
          when (and (attr-val cxn :bare-cxn-name)
                    (eq (attr-val cxn :bare-cxn-name)
                        (attr-val original-cxn :bare-cxn-name))
                    (not (eql (name cxn) (name original-cxn))))
          do (return cxn))))

;;;;;
;; Enable/Disable meta layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comprehend-all-with-disabled-meta-layer-configuration (utterance &key cxn-inventory silent n gold-standard-meaning meta-only-cxns)
  (let (;; store original configurations
        (original-category-linking-mode (get-configuration cxn-inventory :category-linking-mode))
        (original-parse-goal-tests (get-configuration cxn-inventory :parse-goal-tests))
        (original-max-nr-of-nodes (get-configuration cxn-inventory :max-nr-of-nodes))
        (original-node-tests (get-configuration cxn-inventory :node-tests)))
    ;; disable meta layer
    (set-configuration cxn-inventory :category-linking-mode :categories-exist)
    (set-configuration cxn-inventory :update-categorial-links nil)
    (set-configuration cxn-inventory :use-meta-layer nil)
    (set-configuration cxn-inventory :consolidate-repairs nil)
    (when meta-only-cxns
      ;; switch to meta-only cxns
      (set-configuration cxn-inventory :ignore-nil-hashes t)
      (set-configuration cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
      (set-configuration cxn-inventory :parse-order '(meta-apply-first meta-apply-last))
      (set-configuration cxn-inventory :max-nr-of-nodes 250)
      (set-configuration cxn-inventory :node-tests '(:restrict-nr-of-nodes
                                                     :restrict-search-depth)))
    ;; run comprehend-all with the new configurations
    (multiple-value-bind (meanings cip-nodes cip)
        (comprehend-all utterance :cxn-inventory cxn-inventory :silent silent :n n
                        :gold-standard-meaning gold-standard-meaning)
      ;; enable the meta layer
      (set-configuration cxn-inventory :update-categorial-links t)
      (set-configuration cxn-inventory :use-meta-layer t)
      (set-configuration cxn-inventory :consolidate-repairs t)
      (set-configuration cxn-inventory :category-linking-mode original-category-linking-mode)
      (when meta-only-cxns
        ;; switch back to routine cxns
        (set-configuration cxn-inventory :ignore-nil-hashes nil)
        (set-configuration cxn-inventory :parse-goal-tests original-parse-goal-tests)
        (set-configuration cxn-inventory :parse-order '(routine-apply-first routine-apply-last))
        (set-configuration cxn-inventory :max-nr-of-nodes original-max-nr-of-nodes)
        (set-configuration cxn-inventory :node-tests original-node-tests))
      (values meanings cip-nodes cip))))

                 
;;;;;
;; Unit Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-child-units (units)
  (loop for unit in units
        when (member 'used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
        collect unit))

(defun remove-child-units (units)
  (loop for unit in units
        unless (member 'pf::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
        collect unit))

(defun extract-form-from-unit-tree (top-unit transient-structure)
  (let ((top-unit (find (unit-name top-unit) (left-pole-structure transient-structure):key #'first)))
    (extract-forms 
     (cons top-unit
           (all-subunits
            top-unit
            (left-pole-structure transient-structure))))))

;;;;;
;; Variablify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inc-var-id (var)
  (let ((name (get-base-name var :remove-question-mark nil))
        (id (parse-integer (last-elt (split-string (format nil "~a" var) "-"))
                           :junk-allowed t)))
    (when id
      (if (gethash name utils::*nid-table*)
        (when (>= id (gethash name utils::*nid-table*))
          (setf (gethash name utils::*nid-table*) id))
        (setf (gethash name utils::*nid-table*) id)))))
    

(defun inc-var-ids (meaning-network)
  (loop for predicate in meaning-network
        do (loop for el in predicate
                 when (variable-p el)
                 do (inc-var-id el))
        finally (return meaning-network)))


(defun fresh-variables (set-of-predicates)
  (labels ((subst-bindings (bindings)
             (loop for predicate in set-of-predicates
                   collect (loop for elem in predicate
                                 for subst = (assoc elem bindings)
                                 if subst collect (cdr subst)
                                 else collect elem))))
    (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
           (unique-variables (remove-duplicates all-variables))
           (renamings (loop for var in unique-variables
                            for base-name = (get-base-name var)
                            collect (cons var (internal-symb (make-var base-name))))))
      (values (subst-bindings renamings) renamings))))


(defun variablify-form-constraints-with-constants (form-constraints-with-constants)
  "Variablify the constants in the form constraints"
  (loop for form-constraint in form-constraints-with-constants
        for constraint = (first form-constraint)
        collect (cons constraint
                      (case constraint
                        (string (list (variablify (second form-constraint))
                                      (third form-constraint)))
                        (meets (mapcar #'variablify (rest form-constraint)))
                        (top-arg (list (variablify (second form-constraint))
                                       (third form-constraint)))
                        (slot-arg (list (variablify (second form-constraint))
                                        (third form-constraint)))
                        (sequence (cons (second form-constraint)
                                        (mapcar #'variablify (cddr form-constraint))))))))


(defun fresh-variablify-form-constraints-with-constants (form-constraints-with-constants)
  "Variablify the constants in the form constraints with fresh variables.
   Uses a locally defined version of substitute-bindings that can handle
   (const . var) bindings lists."
  (labels ((subst-constants (bindings)
             (loop for form-constraint in form-constraints-with-constants
                   collect (loop for elem in form-constraint
                                 for subst = (assoc elem bindings)
                                 if subst collect (cdr subst)
                                 else collect elem))))
    (let* ((all-constants (loop for form-constraint in form-constraints-with-constants
                                for constraint = (first form-constraint)
                                append (case constraint
                                         (string (list (second form-constraint)))
                                         (meets (rest form-constraint))
                                         (sequence (cddr form-constraint)))))
           (unique-constants (remove-duplicates all-constants))
           (renamings (loop for const in unique-constants
                            for base-name = (get-base-name const)
                            unless (variable-p const)
                            collect (cons const (internal-symb (if (numberp const) (make-var) (make-var base-name)))))))
      (values (subst-constants renamings) renamings))))


;;;;;
;; Hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric form-predicates->hash-string (form-predicates form-representation)
  (:documentation "Generate a hash key for the form predicates according to the form-representation"))

(defmethod form-predicates->hash-string (form-predicates (form-representation (eql :string+meets)))
  ;; a list of all string predicates
  (mapcar #'third (find-all 'string form-predicates :key #'first)))

(defmethod form-predicates->hash-string (form-predicates (form-representation (eql :sequences)))
  nil)

(defgeneric meaning-predicates->hash-meaning (meaning-predicates meaning-representation)
  (:documentation "Generate a hash key for the meaning predicates according to the meaning-representation"))

(defmethod meaning-predicates->hash-meaning (meaning-predicates (meaning-representation (eql :irl)))
  (let* ((all-primitives (mapcar #'first meaning-predicates))
         (all-primitives-but-bind (remove 'bind all-primitives))
         (target-variable (get-target-var meaning-predicates)))
    (cond (; if there are only bind statements
           (null all-primitives-but-bind)
           ; take the last element of the first binding
           (last-elt (first (find-all 'bind meaning-predicates :key #'first))))
          (; if there is a target variable
           target-variable
           ; take the primitive that has it
           (first (find target-variable meaning-predicates :key #'second)))
          (t ;otherwise
           ; take a random primitive that has an open variable on the second position
           (let* ((open-vars (get-unconnected-vars meaning-predicates))
                  (target-vars (intersection open-vars (mapcar #'second meaning-predicates)))
                  (a-target-var (random-elt target-vars)))
             (first (find a-target-var meaning-predicates :key #'second)))))))

(defun hash-observation (form-constraints meaning-predicates)
  ;; extract string predicates + predicate names
  (let ((meaning-predicates
         (loop for meaning in meaning-predicates
               collect (if (and (= 4 (length meaning))
                                (eql 'bind (first meaning)))
                         (fourth meaning)
                         (first meaning))))
        (form-predicates
         (mapcar #'third (find-all 'string form-constraints :key #'first))))
    (append form-predicates meaning-predicates)))
                            
(defun constructions-for-anti-unification-hashed (form-constraints meaning-predicates cxn-inventory)
  (case (get-configuration cxn-inventory :form-representation-formalism)
    (:string+meets
     (remove-duplicates
      (append
       (loop for hash in (hash-observation form-constraints meaning-predicates)
             append (gethash hash (constructions-hash-table cxn-inventory)))
       (gethash nil (constructions-hash-table cxn-inventory)))))
    ;; when using sequences, there is no hashing
    ;; so just return all cxns!
    (:sequences (constructions cxn-inventory))))

;;;;;
;; Partial Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-predicates-in-root (cipn)
  (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))

(defun all-cip-nodes (cip)
  (remove nil
          (traverse-depth-first
           (top-node cip)
           :collect-fn #'(lambda (node)
                           (unless (or (find 'fcg::duplicate (fcg::statuses node))
                                       (find 'fcg::second-merge-failed (fcg::statuses node))
                                       (find 'fcg::initial (fcg::statuses node)))
                             node)))))

(defun sort-by-depth-created-at-and-avg-score (list-of-cip-nodes)
  (sort list-of-cip-nodes
        #'(lambda (cipn-1 cipn-2)
            (cond ((length= (all-parents cipn-1) (all-parents cipn-2))
                   (cond ((= (created-at cipn-1) (created-at cipn-2))
                          (> (average (mapcar #'get-cxn-score (original-applied-constructions cipn-1)))
                             (average (mapcar #'get-cxn-score (original-applied-constructions cipn-2)))))
                         ((< (created-at cipn-1) (created-at cipn-2)) t)
                         (t nil)))
                  ((length> (all-parents cipn-1) (all-parents cipn-2)) t)
                  (t nil)))))

(defun compatible-cipns-with-routine-cxns (form-constraints gold-standard-meaning cxn-inventory)
  (when (constructions cxn-inventory)
    (destructuring-bind (meanings cipns cip)
        ;; re-use the comprehension results with disabled meta layer (if available)
        ;; created by the add-categorial-links repair
        ;; otherwise, run comprehension
        (or (find-data (blackboard cxn-inventory) :comprehension-results-with-disabled-meta-layer)
            (multiple-value-list
             (comprehend-all-with-disabled-meta-layer-configuration
              form-constraints :cxn-inventory cxn-inventory :silent t)))
      (declare (ignore meanings cipns))
      (loop for cip-node in (all-cip-nodes cip)
            for meaning = (extract-meanings
                           (left-pole-structure
                            (car-resulting-cfs
                             (cipn-car cip-node))))
            when (and meaning ; meaning should be non-nil
                      (form-predicates-in-root cip-node) ; some form left in root
                      (irl::embedding meaning gold-standard-meaning)) ; partial meaning should be compatible with gold standard
            collect cip-node))))

(defun compatible-cipns-with-meta-cxns (form-constraints gold-standard-meaning cxn-inventory)
  (when (constructions cxn-inventory)
    (multiple-value-bind (meanings cipns cip)
        (comprehend-all-with-disabled-meta-layer-configuration
         form-constraints :cxn-inventory cxn-inventory :silent t
         :meta-only-cxns t)
      (declare (ignore meanings cipns))
      (loop for cip-node in (all-cip-nodes cip)
            for meaning = (extract-meanings
                           (left-pole-structure
                            (car-resulting-cfs
                             (cipn-car cip-node))))
            when (and meaning ; meaning should be non-nil
                      (form-predicates-in-root cip-node) ; some form left in root
                      (irl::embedding meaning gold-standard-meaning) ; partial meaning should be compatible with gold standard
                      (fcg::connected-syntactic-structure (fcg-get-transient-unit-structure cip-node)) ; connected structure in TS
                      (get-open-slot-units cip-node)) ; some unit(s) that represents open slot(s) in the TS
            collect cip-node))))


;;;;;
;; Sort meets constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boundaries (form-constraints)
  "returns all boundaries of the given form-constraints
   (in terms of string + meets predicates)"
  (let* ((left-units
          (loop for fc in form-constraints
                if (eql 'meets (first fc))
                collect (second fc)
                else if (eql 'sequence (first fc))
                collect (third fc)))
         (right-units
          (loop for fc in form-constraints
                if (eql 'meets (first fc))
                collect (third fc)
                else if (eql 'sequence (first fc))
                collect (fourth fc)))
         (left-boundaries (set-difference left-units right-units))
         (right-boundaries (set-difference right-units left-units)))
    (if (and left-boundaries right-boundaries)
      (flatten (pairlis left-boundaries right-boundaries))
      (when (every #'(lambda (p) (eql p 'string))
                   (mapcar #'first form-constraints))
        (mapcar #'second form-constraints)))))


#|
(defun sort-meets-constraints (meets-constraints)
  "return the sorted list of meets constraints"
  (let* ((begin-var (first (get-boundary-units meets-constraints)))
         (first-predicate (find begin-var meets-constraints :key #'second)))
    (loop with next-predicate = first-predicate
          with resulting-list = nil
          for next-var = (third next-predicate)
          while next-predicate
          do (push next-predicate resulting-list)
          (setf next-predicate (find next-var meets-constraints :key #'second))
          finally (return (reverse resulting-list)))))


(defun continuous-meets-p (form-constraints)
  "check if within a holistic chunk, all form strings are connected"
  (let* ((left-units
          (loop for fc in form-constraints
                when (equal 'meets (first fc))
                  collect (second fc)))
         (right-units
          (loop for fc in form-constraints
                when (equal 'meets (first fc))
                  collect (third fc)))
         (string-units
          (loop for fc in form-constraints
                when (equal 'string (first fc))
                  collect (second fc)))
         (left-most-diff (set-difference left-units right-units))
         (right-most-diff (set-difference right-units left-units))
         (all-units (remove-duplicates (append left-units right-units)))
         (string-meets-diff (set-difference string-units all-units))
         (meets-string-diff (set-difference all-units string-units)))
    
    (if (and left-most-diff right-most-diff)
      (and (= 1 (length left-most-diff))
           (= 1 (length right-most-diff))
           (not string-meets-diff)
           (not meets-string-diff)
           (get-boundary-units form-constraints))
      (get-boundary-units form-constraints))))
|#

;;;;;
;; Input Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric form-constraints-with-variables (utterance de-render-mode mode)
  (:documentation "Extract form constraints from utterance in the format they would appear in a construction."))

(defmethod form-constraints-with-variables (utterance de-render-mode (mode (eql :string+meets)))
  (let ((form-constraints-with-constants
         (remove 'sequence
                 (extract-forms
                  (left-pole-structure
                   (de-render utterance de-render-mode)))
                 :key #'first)))
    (fresh-variablify-form-constraints-with-constants form-constraints-with-constants)))

(defmethod form-constraints-with-variables (utterance de-render-mode (mode (eql :sequences)))
  (let ((form-constraints-with-constants
         (extract-forms
          (left-pole-structure
           (de-render utterance de-render-mode)))))
    (fresh-variablify-form-constraints-with-constants form-constraints-with-constants)))

(defgeneric meaning-predicates-with-variables (meaning mode))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :irl)))
  "Transform meaning network with constants to meaning network with variables."
  (loop for predicate in meaning
        collect (if (equal (first predicate) 'bind)
                  (list (first predicate)
                        (second predicate)
                        (variablify (third predicate))
                        (fourth predicate))
                  (cons (first predicate)
                        (mapcar #'variablify (rest predicate))))))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :geo)))
  "Transform meaning network with constants to meaning network with variables."
  (loop for predicate in meaning
        collect (cons (first predicate)
                      (mapcar #'variablify (rest predicate)))))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :amr)))
  "Transform meaning network with constants to meaning network with variables."
  (amr:variablify-amr-network meaning))

;;;;;
;; Equivalent Meaning Networks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equivalent-meaning-networks (m1 m2 mode))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :irl)))
  (equivalent-irl-programs? m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :amr)))
  (amr::equivalent-amr-predicate-networks m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :geo)))
  (amr::equivalent-amr-predicate-networks m1 m2))


;;;;;
;; Anti Unification Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-var-from-pattern-to-source (var anti-unification-result)
  "Map a variable from the pattern delta to the same variable in the source delta,
   using the bindings lists."
  (let* ((var-in-generalisation (rest (assoc var (pattern-bindings anti-unification-result)))))
    (first (rassoc var-in-generalisation (source-bindings anti-unification-result)))))

#|
(defmethod select-holistic-cxns-for-anti-unification (observation-form observation-meaning (cxn-inventory fcg-construction-set))
  "Select holistic cxns from the routine set with a score greater than 0."
  (declare (ignore observation-form observation-meaning))
  (let* ((hash-compatible-cxns
          (constructions-for-anti-unification-hashed observation-form observation-meaning cxn-inventory))
         (holistic-routine-non-zero-cxns
          (remove-if-not #'non-zero-cxn-p
                         (remove-if-not #'holistic-cxn-p
                                        (remove-if-not #'routine-cxn-p hash-compatible-cxns)))))
    (sort holistic-routine-non-zero-cxns #'> :key #'get-cxn-score)))

(defmethod anti-unify-constructions-with-observation (observation-form observation-meaning constructions (cxn-inventory fcg-construction-set))
  "Anti-unify the observation with the constructions.
   For each cxn, keep the best au result on the form side and the meaning side.
   Sum the cost and keep the cxn score."
  (let ((au-results
         (loop with max-au-cost = (get-configuration cxn-inventory :max-au-cost)
               for cxn in constructions
               for meaning-au-results
                 = (fcg::anti-unify-predicate-network (fcg::extract-meaning-predicates cxn) observation-meaning)
               for best-meaning-au-result = (first meaning-au-results)
               for form-au-results
                 = (fcg::anti-unify-predicate-network (fcg::extract-form-predicates cxn) observation-form)
               for best-form-au-result = (first form-au-results)
               when (and best-meaning-au-result best-form-au-result
                         (<= (au-cost best-meaning-au-result) max-au-cost)
                         (<= (au-cost best-form-au-result) max-au-cost))
               collect (list best-form-au-result best-meaning-au-result 
                             (+ (au-cost best-form-au-result)
                                (au-cost best-meaning-au-result))
                             (attr-val cxn :score)
                             cxn))))
    ;; take the anti-unification with the lowest summed cost (form + meaning)
    ;; if multiple, take the one that anti-unified with the highest scoring cxn
    ;; if multiple, take a random one    
    (first (all-biggest #'fourth (all-smallest #'third au-results)))))

(defun anti-unify-partial-analysis-with-observation (observation-form observation-meaning partial-analysis-cipn)
  ;; the generalisation is identical to the meaning/form of the cipn,
  ;; the pattern delta is empty
  ;; and the source delta contains the material for the new cxn to be learned
  (let* ((cipn-meaning (fcg-extract-meanings partial-analysis-cipn))
         ;; cipn-form has the same variables as the observation-form
         ;; --> these variables do not occur in the bindings of the anti-unification...
         ;; --> make fresh variables and store the mappings from the original
         ;;     constants to the fresh variables
         (cipn-form-and-variable-renamings
          (multiple-value-list
           (fresh-variables
            (variablify-form-constraints-with-constants
             (loop for unit in (fcg-get-transient-unit-structure partial-analysis-cipn)
                   unless (eql (unit-name unit) 'fcg::root)
                     append (unit-feature-value unit 'form))))))
         (cipn-form (first cipn-form-and-variable-renamings))
         (form-const-renamings
          (loop for (var . fresh-var) in (second cipn-form-and-variable-renamings)
                collect (cons (devariablify var) fresh-var)))
         (meaning-a-u (first (anti-unify-predicate-network cipn-meaning observation-meaning)))
         (form-a-u (first (anti-unify-predicate-network cipn-form observation-form))))
    (unless (or (null (source-delta meaning-a-u))
                (null (source-delta form-a-u)))
      ;; store the renamings in the cipn
      (set-data partial-analysis-cipn :form-const-renamings form-const-renamings)
      ;; nazi checks
      (assert (and (null (pattern-delta meaning-a-u))
                   (null (pattern-delta form-a-u))
                   (equivalent-irl-programs? (generalisation meaning-a-u) cipn-meaning)
                   (equivalent-irl-programs? (generalisation form-a-u) cipn-form)))
      ;; return AU results
      (values form-a-u meaning-a-u))))
|#