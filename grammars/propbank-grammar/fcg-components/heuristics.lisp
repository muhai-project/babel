(in-package :propbank-grammar)


(defmethod apply-heuristic ((node cip-node) (mode (eql :frequency)))
  "Returns the frequency of the construction that was applied in the
node, divided by 100 to account for large numbers."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (/ (attr-val applied-cxn :score) 100)))


(defmethod apply-heuristic ((node cip-node) (mode (eql :edge-weight)))
  "Returns the weight of the categorial network edge that was used in
matching."
  (if (field? (blackboard (construction-inventory node)) :matched-categorial-links)
    ;;When the categorial network was used in matching:
    (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
      
      (cond ((attr-val applied-cxn :gram-category) ;;Lex->gram categorial link was used
             (let* ((matched-neighbours
                     (loop for links-and-score in (get-data  (blackboard (construction-inventory node)) :matched-categorial-links)
                           when (eq (caar links-and-score) (attr-val applied-cxn :gram-category))
                             collect (cdar links-and-score)))
                    (lex-cat-used 
                     (loop for matched-neighbour-cat in matched-neighbours
                           when (neighbouring-categories-p (attr-val applied-cxn :gram-category)
                                                           matched-neighbour-cat
                                                           (categorial-network (construction-inventory node))
                                                           :link-type 'lex-gram)
                             do (return matched-neighbour-cat))))
               ;;Retrieve the link weight as stored in the matched-categorial-link blackboard by FCG's unify-atom
               (cdr (find (cons (attr-val applied-cxn :gram-category) lex-cat-used)
                          (get-data (blackboard (construction-inventory node)) :matched-categorial-links)
                          :key #'car :test #'equalp))))
            ((attr-val applied-cxn :sense-category) ;;Gram->sense categorial link was used
             (let* ((matched-neighbours
                     (loop for links-and-score in (get-data  (blackboard (construction-inventory node)) :matched-categorial-links)
                           when (eq (caar links-and-score) (attr-val applied-cxn :sense-category))
                             collect (cdar links-and-score)))
                    (gram-cat-used 
                     (loop for matched-neighbour-cat in matched-neighbours
                           when (neighbouring-categories-p (attr-val applied-cxn :sense-category)
                                                           matched-neighbour-cat
                                                           (categorial-network (construction-inventory node))
                                                           :link-type 'gram-sense)
                             do (return matched-neighbour-cat))))
                ;;Retrieve the link weight as stored in the matched-categorial-link blackboard by FCG's unify-atom
               (cdr (find (cons (attr-val applied-cxn :sense-category) gram-cat-used)
                          (get-data (blackboard (construction-inventory node)) :matched-categorial-links)
                          :key #'car :test #'equalp))))
            (t
             0)))
    0))


(defmethod apply-heuristic ((node cip-node) (mode (eql :nr-of-units-matched)))
  "Returns the number of units matched by the cxn."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (length (conditional-part applied-cxn))))


(defmethod apply-heuristic ((node cip-node) (mode (eql :nr-of-units-matched-x2)))
  "Returns the number of units matched by the cxn."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (* (length (conditional-part applied-cxn)) 2)))


(defmethod apply-heuristic ((node cip-node) (mode (eql :nr-of-roles-integrated)))
  "Returns the number of roles integrated by the cxn."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (if (eql (attr-val applied-cxn :label) 'propbank-grammar::argument-structure-cxn)
      (let ((cxn-meaning (first (fcg-unit-feature-value (first (contributing-part applied-cxn)) 'meaning))))
        (- (length cxn-meaning) 1))
      0)))


(defmethod apply-heuristic ((node cip-node) (mode (eql :minimize-path-length)))
  ""
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (if (eql (attr-val applied-cxn :label) 'propbank-grammar::argument-structure-cxn)
      (let* ((transient-structure (car-resulting-cfs (cipn-car node)))
             (bindings (car-second-merge-bindings (cipn-car node)))
             (cxn-renamings (renamings (processing-cxn (get-original-cxn (car-applied-cxn (cipn-car node))))))
             (cxn-meaning (first (fcg-unit-feature-value (first (contributing-part applied-cxn)) 'meaning)))
             (v-unit-var-processing-cxn (cdr (assoc (last-elt (find 'frame cxn-meaning :key #'first :test #'eql)) cxn-renamings)))
             (cxn-renamings-2 (renamings (car-applied-cxn (cipn-car node))))
             (v-unit-name (lookup (lookup v-unit-var-processing-cxn cxn-renamings-2) bindings)))
           (if v-unit-name
             (let* ((roles-w-unit-names (loop for predicate in cxn-meaning
                                              when (eql 'frame-element (first predicate))
                                                collect (cons (second predicate) ;;role name
                                                              (lookup (lookup (cdr (assoc (fourth predicate) cxn-renamings)) cxn-renamings-2)
                                                                      (car-second-merge-bindings (cipn-car node))))))
                    (path-per-frame-element (loop with v-unit = (assoc v-unit-name (left-pole-structure transient-structure))
                                                  for (role . role-unit-name) in roles-w-unit-names
                                                  for  role-unit = (assoc role-unit-name (left-pole-structure transient-structure))
                                                  collect (cons role (find-path-in-syntactic-tree role-unit v-unit (left-pole-structure transient-structure))))))
               (if path-per-frame-element
                 (loop for (nil . path) in path-per-frame-element
                       sum (length path) into total-path-length
                       finally (return (/ total-path-length
                                          (length roles-w-unit-names))))
                 1))
             0))
      0)))


(defparameter *argm-predictor* "http://localhost:3600/predict")

(defun send-request (json &key (host *argm-predictor*) (connection-timeout 3600))
  "Send curl request and returns the answer. Makes use of text-to-role
SVM classifier accessible at https://gitlab.ai.vub.ac.be/ehai/text-to-role-classification."
  (declare (ignore host))

  (handler-case (cl-json:camel-case-to-lisp
                 (first (cl-json::decode-json-from-string
                         (dex:post *argm-predictor*
                                   :headers '(("content-type" . "application/json"))
                                   :content json
                                   :connect-timeout connection-timeout))))
    (error (e)
      (format t "Error in sending request to text-to-role classification module. The Python server probably needs to be activated. ~S.~&" e))))

;(send-request (cl-json:encode-json-to-string (list "in the Saint Stephan's dome")))

(defun extract-argm-phrase-from-ts (node cxn-argm)
  "Extract the whole argm string from the unit that constains the V's modifier argument."
  (let* ((ts (left-pole-structure (car-source-cfs (cipn-car node)))) ;;transient structure
         (bindings (car-second-merge-bindings (cipn-car node)))
         (argm-syn-class+string (mkstr (cdr cxn-argm))))

    (if (search "\(" argm-syn-class+string)
      (let* ((argm-syn-class (upcase (subseq argm-syn-class+string 0 (search "\(" argm-syn-class+string))))
             
             (ts-argm-unit-name
              (loop for binding in bindings
                    when (search (format nil "?~a-" argm-syn-class)
                                 (mkstr (car binding)))
                    return (cdr binding))))
        
        (assert ts-argm-unit-name)
    
        (loop for unit in ts
              when (eql (unit-name unit) ts-argm-unit-name)
                return (unit-feature-value unit 'string)))
      argm-syn-class+string)))

       
(defmethod apply-heuristic ((node cip-node) (mode (eql :argm-prediction)))
  "Verify whether the construction's label and the predicted label coincide"
  (let* ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node))))
         (argm-cxn? (search "ARGM-" (mkstr (name applied-cxn)))))
    
    (if argm-cxn?
      (let* ((cxn-argm (assoc "ARGM-" (attr-val applied-cxn :schema)
                                   :key #'mkstr
                                   :test #'search))
             (cxn-argm-label (car cxn-argm))
             (argm-phrase-string (extract-argm-phrase-from-ts node cxn-argm)))
        
        (assert argm-phrase-string)
        
        (when argm-phrase-string
          (let ((predicted-argm-label (send-request (cl-json:encode-json-to-string (list argm-phrase-string)))))
            (if (string= cxn-argm-label predicted-argm-label)
                10;; prediction label is the same as the cxn label
                0))))
      0)))
