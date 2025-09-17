(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;; Construction suppliers ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Construction-suppliers for routine processing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-hashed-with-regex-check ()
  ()
  (:documentation "Construction supplier that returns constructions with a compatible hash and
constructions with a compatible regex sequence form."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-with-regex-check)))
  "Method that creates the hashed-with-regex-check cxn-supplier."
  (make-instance 'cxn-supplier-hashed-with-regex-check))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-with-regex-check) (node cip-node))
  "Returns all constructions to match to the transient structure, i.e. all  constructions with a
compatible hash and constructions with a compatible regex sequence form."
  (let* ((ignore-nil-hashes (get-configuration (construction-inventory node) :ignore-nil-hashes))
         (hashed-constructions (loop for hash in (hash node (get-configuration node :hash-mode))
                                     append (gethash hash (constructions-hash-table (construction-inventory node)))))
         (non-hashed-cxns (unless ignore-nil-hashes
                            (if (eq '-> (direction (cip node)))
                              ;; In production, for now, consider all cxns hashed in nil, to be optimised later
                              (gethash nil (constructions-hash-table (construction-inventory node)))
                              ;; For comprehension, check regex-match
                              (loop with root = (get-root (left-pole-structure (car-resulting-cfs (cipn-car node))))
                                    with root-string = (format nil "~{~a~}" (mapcar #'second (feature-value (unit-feature root 'form))))
                                    for cxn in (gethash nil (constructions-hash-table (construction-inventory node)))
                                    for sequences-in-cxn = (mapcar #'second
                                                                   (find-all-if #'(lambda (p) (eq 'sequence (first p))) (attr-val cxn :form)))
                                    unless sequences-in-cxn
                                      collect cxn ;; collect cxn to try if it contains no sequences
                                    else 
                                      when (loop for sequence in sequences-in-cxn
                                                 for re-scanner = (retrieve-or-create-re-scanner sequence (construction-inventory node))
                                                 always (cl-ppcre:scan re-scanner root-string))
                                        collect cxn))))
         (all-cxns-to-supply (remove-duplicates (append hashed-constructions non-hashed-cxns))))
           
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setf all-cxns-to-supply (shuffle all-cxns-to-supply)))  
    ;; return constructions
    all-cxns-to-supply))


;; Construction-suppliers used in meta-layer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-holophrase-cxns-only ()
  ()
  (:documentation "Construction supplier that only returns holophrase-cxns."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :holophrase-cxns-only)))
  "Creates an instance of the cxn-supplier."
  (make-instance 'cxn-supplier-holophrase-cxns-only))

(defmethod next-cxn ((cxn-supplier cxn-supplier-holophrase-cxns-only) (node cip-node))
  "Returns all constructions that are found under key 'holophrase-cxns."
  (gethash 'holophrastic-cxns (constructions-hash-table (construction-inventory node))))


(defclass cxn-supplier-linking-cxns-only ()
  ()
  (:documentation "Construction supplier that only returns linking-cxns."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :linking-cxns-only)))
  "Creates an instance of the cxn-supplier."
  (make-instance 'cxn-supplier-linking-cxns-only))

(defmethod next-cxn ((cxn-supplier cxn-supplier-linking-cxns-only) (node cip-node))
  "Returns all constructions that are found under key 'holophrase-cxns."
  (gethash 'linking-cxns (constructions-hash-table (construction-inventory node))))


;; Hash methods ;;
;;;;;;;;;;;;;;;;;;

(defmethod hash ((cxn construction)
                 (mode (eql :filler-and-linking))
                 &key &allow-other-keys)
  "Hash method for constructions."
  (cond ((or (eql (type-of cxn) 'holophrastic-cxn)
             (and (eql (type-of cxn) 'processing-construction)
                  (eql (type-of (original-cxn cxn)) 'holophrastic-cxn)))
         (list (attr-val cxn :form-hash-key) (attr-val cxn :meaning-hash-key) 'holophrastic-cxns))
        ((or (eql (type-of cxn) 'linking-cxn)
             (and (eql (type-of cxn) 'processing-construction)
                  (eql (type-of (original-cxn cxn)) 'linking-cxn)))
         (append (attr-val cxn :slot-cats) (list 'linking-cxns)))))

(defmethod hash ((node cip-node)
                 (mode (eql :filler-and-linking)) 
                 &key &allow-other-keys)
  "Hash method for nodes."
  (cond ((and (find 'initial (statuses node))
              (eql '<- (direction (cip node))))
         (list (second (first (unit-feature-value (get-root (fcg-get-transient-unit-structure node)) 'form)))))
        ((and (find 'initial (statuses node))
              (eql '-> (direction (cip node))))
         (list (compute-meaning-hash-key-from-predicates (unit-feature-value (get-root (fcg-get-transient-unit-structure node)) 'meaning))))
        (t
         (loop for unit in (fcg-get-transient-unit-structure node)
               when (unit-feature unit 'category)
                 collect (unit-feature-value unit 'category) into ts-categories
               finally (return (mappend #'(lambda (cat) 
                                            (neighbouring-categories cat (categorial-network (construction-inventory node))))
                                        ts-categories))))))

(defun compute-meaning-hash-key-from-predicates (meaning-predicates)
  "Computes meaning-hash-key based on set of predicates."
  (loop for predicate in meaning-predicates
        if (and (eql (first predicate) 'bind) ; for IRL
                (= 4 (length predicate)))
          collect (symbol-name (last-elt predicate)) into keys
        else
          collect (symbol-name (first predicate)) into keys
        finally (return (intern (upcase (format nil "~{~a~^-~}" (sort keys #'string<)))))))