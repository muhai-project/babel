(in-package :cgl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add holophrase construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nothing->holophrase (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair nothing->holophrase)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (initial-node-p node)
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holophrase-cxn problem node))))
  
(defmethod repair ((repair nothing->holophrase)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (initial-node-p node)
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holophrase-cxn problem node))))

(defun create-holophrase-cxn (problem node)
  "Creates a holophrase-cxn."
  (let* ((processing-cxn-inventory (construction-inventory node))
         (cxn-inventory (original-cxn-set processing-cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (cxn-name (make-cxn-name utterance cxn-inventory))
         (form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         ;; take the last element of the form constraints (the last word) and use it for hashing
         (hash-string (loop for fc in form-constraints
                        when (equalp (first fc) 'string)
                        collect (third fc) into hash-strings
                        finally (return (last-elt hash-strings))))
         (holophrase-cxn (second (multiple-value-list  (eval
                                                        `(def-fcg-cxn ,cxn-name
                                                                      ((?holophrase-unit
                                                                        (syn-cat (phrase-type holophrase)))
                                                                       <-
                                                                       (?holophrase-unit
                                                                        (HASH meaning ,meaning)
                                                                        --
                                                                        (HASH form ,form-constraints)))
                                                                      :attributes (:cxn-type holophrase
                                                                                   :repair nothing->holophrase
                                                                                   :string ,hash-string)
                                                                      :cxn-inventory ,(copy-object cxn-inventory)))))))
    holophrase-cxn))


;; uses standard handle-fix
;; see meta dot handle-fix
