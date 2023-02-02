(in-package :pattern-finding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add Categorial links     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-categorial-links (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-categorial-links (create-categorial-links problem node)))
    (if cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links)
      nil)))


(defun create-categorial-links (problem node)
  (do-create-categorial-links
   (get-data problem :utterance)
   (get-data problem :meaning)
   nil
   nil
   (construction-inventory node)
   node))


(defun do-create-categorial-links (form-constraints meaning form-args meaning-args cxn-inventory node)
  "Return the categorial links and applied cxns from a comprehend with :category-linking-mode :path-exists instead of :neighbours"
  (declare (ignore form-args meaning-args))
  (disable-meta-layer-configuration cxn-inventory) 
  (with-disabled-monitor-notifications
    (multiple-value-bind (parsed-meanings solutions)
        (comprehend-all form-constraints
                        :cxn-inventory (original-cxn-set cxn-inventory)
                        :gold-standard-meaning meaning)
      (declare (ignore parsed-meanings))
      (enable-meta-layer-configuration cxn-inventory)
      (let* ((required-top-lvl-args
              (get-unconnected-vars meaning))
             (succeeded-nodes
              (loop for cipn in solutions
                    when (find 'fcg::succeeded (statuses cipn))
                      collect cipn))
             (node-with-compatible-args
              (first
               (reject-solutions-with-incompatible-args
                succeeded-nodes meaning required-top-lvl-args))))
        (when node-with-compatible-args
          (let* ((cxns-to-apply (reverse (original-applied-constructions node-with-compatible-args)))
                 (top-lvl-category (extract-lex-class-item-based-cxn (last-elt cxns-to-apply))))
            (when (> (length cxns-to-apply) 1)
              (apply-fix 
               ;; form constraints
               form-constraints
               ;; cxns to appply
               cxns-to-apply
               ;; categorial links
               (extract-used-categorial-links node-with-compatible-args)
               ;; original cxns to consolidate
               nil
               ;; categories to add
               nil
               ;; top level category
               top-lvl-category
               ;; gold standard consulted p
               (gold-standard-consulted-p node-with-compatible-args)
               ;; node
               node
               ;; repair name
               'add-categorial-links))))))))


(defun substitute-predicate-bindings (predicate bindings)
  (loop with frame-bindings = (irl::map-frame-bindings bindings)
        for elt in predicate
        for assoc-res = (assoc elt frame-bindings)
        collect (if assoc-res
                  (cdr assoc-res)
                  elt)))

(defun get-top-level-ts-args (cip-node)
  (let* ((all-units (left-pole-structure (car-resulting-cfs (cipn-car cip-node))))
         (top-unit
          (loop for unit in all-units
                for footprints = (unit-feature-value unit 'fcg:footprints)
                unless (member 'pf::used-as-slot-filler footprints)
                  return unit)))
    (second (find 'meaning-args (rest top-unit) :key #'first))))
  
;(second (find 'ARGS (rest (first (remove-child-units (left-pole-structure (car-resulting-cfs (cipn-car cip-node)))))) :key #'first)))

(defun reject-solutions-with-incompatible-args (cip-nodes gold-standard-meaning required-args)
  "Check if the open variables of the gold standard meaning
   correspond with the args of the top unit of the ts of the cipn."
  (loop for cip-node in cip-nodes
        for parsed-meaning = (extract-meanings
                              (left-pole-structure
                               (car-resulting-cfs (cipn-car cip-node))))
        for ts-top-level-args = (get-top-level-ts-args cip-node)
        for embedding = (irl::embedding parsed-meaning gold-standard-meaning)
        for renamed-ts-args = (when embedding (substitute-predicate-bindings ts-top-level-args (first embedding)))
        when (or (not required-args) ;; if there aren't any required args but you still have some, succeed
                 (equal renamed-ts-args required-args))
          collect cip-node))


(defun extract-used-categorial-links (solution-cipn)
  "For a given solution-cipn, extracts categorial links that were used (based on lex-class)."
  (loop for cipn in (ignore-initial-nodes (reverse (cons solution-cipn (all-parents solution-cipn))))
        append (let* ((processing-cxn (car-applied-cxn (cipn-car cipn)))
                      (processing-cxn (if (equal (attr-val processing-cxn :label) 'fcg::routine)
                                        processing-cxn
                                        (first (remove (name processing-cxn)
                                                       (find-all (attr-val processing-cxn :bare-cxn-name)
                                                                 (constructions-list (construction-inventory cipn))
                                                                 :key #'(lambda (cxn) (attr-val cxn :bare-cxn-name)))
                                                       :key #'name))))                               
                      (units-matching-lex-class (loop for unit in (right-pole-structure processing-cxn)
                                                      for syn-cat = (rest (unit-feature-value unit 'syn-cat))
                                                      for lex-class = (second (find 'lex-class syn-cat :key #'first))
                                                      when lex-class
                                                        collect (cons (first unit) lex-class))))
                 (loop for (cxn-unit-name . cxn-lex-class) in units-matching-lex-class
                       for ts-unit-name = (cdr (find cxn-unit-name (car-second-merge-bindings (cipn-car cipn)) :key #'first))
                       for ts-unit = (find ts-unit-name (left-pole-structure (car-source-cfs (cipn-car cipn))):key #'first)
                       for ts-lex-class = (second (find 'lex-class (second (find 'syn-cat (rest ts-unit) :key #'first)) :key #'first))
                       when (and cxn-lex-class ts-lex-class)
                         collect (cons ts-lex-class cxn-lex-class)))))
            
