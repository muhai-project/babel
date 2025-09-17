(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Helper functions for FCG ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-root-p (node)
  "Returns t if the root unit of the node contains no form or meaning."
  (when (find 'cxn-applied (statuses node)) ;; return nil immediately if node has no result (e.g. first-merge failed)
    (let ((root-unit (get-root (left-pole-structure (car-resulting-cfs (cipn-car node))))))
      (unless (or (unit-feature-value root-unit 'form)
                  (unit-feature-value root-unit 'meaning))
        t))))


(defmethod find-cxn ((cxn holophrastic-cxn) (hashed-fcg-construction-set hashed-fcg-construction-set)
                     &key (key #'identity) (test #'eql))
  "More efficient find-cxn method for holophrastic-cxns."
  (find (funcall key cxn)
        (gethash (attr-val cxn :form-hash-key) (constructions-hash-table hashed-fcg-construction-set))
        :test test :key key))

(defun substitute-bindings-including-constants (bindings list-or-atom)
  "Substitutes all bindings in list-or-atom."
  (cond ((lookup list-or-atom bindings))
        ((atom list-or-atom)
         list-or-atom)
        (t
         (cons (substitute-bindings-including-constants bindings (first list-or-atom))
               (substitute-bindings-including-constants bindings (rest list-or-atom))))))        

(defun subst-bindings (set-of-predicates bindings)
  (loop for predicate in set-of-predicates
        collect (loop for elem in predicate
                      for subst = (assoc elem bindings)
                      if subst collect (cdr subst)
                      else collect elem)))

(defun fresh-variables (set-of-predicates)
  "Renames all variables in a set-of-predicates."
  (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
         (unique-variables (remove-duplicates all-variables))
         (renamings (loop for var in unique-variables
                          for base-name = (get-base-name var)
                          collect (cons var (internal-symb (make-var base-name))))))
    (values (subst-bindings set-of-predicates renamings) renamings)))

(defun make-filler-unit-name (form-predicates)
  "Create a unique construction name based on the strings present in form-sequence-predicates."
  (make-var (upcase (substitute #\- #\Space (format nil "~{~a~^_~}-unit" (render form-predicates :render-sequences))))))

(defun make-cxn-name (form-predicates)
  "Create a unique construction name based on the strings present in form-sequence-predicates."
  (make-id (upcase (substitute #\- #\Space (format nil "~{~a~^_~}-cxn" (render form-predicates :render-sequences))))))

(defun remove-cxn-tail (string)
  "Return part of string before -cxn."
  (let ((start-tail (search "-CXN" string)))
    (subseq string 0 start-tail)))


(defun other-cxn-w-same-form-and-meaning-p (cxn-1 cxn-2)
  "Returns true if two different constructions have the same form and meaning."
  (unless (eql (name cxn-1) (name cxn-2))
    (equivalent-form-meaning-mapping cxn-1 cxn-2)))

(defun equivalent-form-meaning-mapping (cxn-1 cxn-2)
  "Returns true if two different constructions have the same form and meaning."
    (let ((meaning-cxn-1 (attr-val cxn-1 :meaning))
          (meaning-cxn-2 (attr-val cxn-2 :meaning))
          (form-cxn-1 (attr-val cxn-1 :form))
          (form-cxn-2 (attr-val cxn-2 :form)))

      (and (= (length meaning-cxn-1) (length meaning-cxn-2))
           (= (length form-cxn-1) (length form-cxn-2))
           (pn::equivalent-predicate-networks-p form-cxn-1 form-cxn-2)
           (pn::equivalent-predicate-networks-p meaning-cxn-1 meaning-cxn-2))))


(defmethod upward-branch ((state au-repair-state) &key (include-initial t))
  "Returns the given cipn and all its parents"
  (cons state (if include-initial
                 (all-parents state)
                 (butlast (all-parents state)))))


;; Annotate node with used categorial links ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun annotate-cip-with-used-categorial-links (cip)
  "Sets the used-categorial-links field in every node of cip."
  (traverse-depth-first (top-node cip) :do-fn #'annotate-node-with-used-categorial-links))

(defun annotate-node-with-used-categorial-links (node)
  "Sets the used-categorial-links field in node."
  (unless (or (field? node :used-categorial-links) (not (applied-constructions node)))
    (let* ((conditional-units-with-cat (loop for unit in (left-pole-structure (first (applied-constructions node)))
                                             for category = (unit-feature-value unit 'category)
                                             when (and category (not (j-unit-p unit)))
                                               collect (cons (unit-name unit) category)))
           (used-categorial-links (loop for (cxn-unit-name . cxn-cat) in conditional-units-with-cat
                                        for corresponding-ts-unit-name = (cdr (assoc cxn-unit-name (car-match-bindings (cipn-car node))))
                                        collect (list cxn-cat (unit-feature-value (find corresponding-ts-unit-name
                                                                                        (left-pole-structure (car-resulting-cfs (cipn-car node)))
                                                                                        :key #'first)
                                                                                  'category)
                                                      nil ;; adapt if there is an efficient way
                                                      ))))
      (set-data node :used-categorial-links used-categorial-links))))

(defun used-categorial-links (node)
  "Returns the categorial links used in the path to this node (after annotation by annotate-cip-with-used-categorial-links)."
  (mappend #'(lambda (n)
               (get-data n :used-categorial-links))
           (upward-branch node :include-initial nil)))
