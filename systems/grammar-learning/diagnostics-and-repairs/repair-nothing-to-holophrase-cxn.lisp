(in-package :grammar-learning)

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
  (when (and (initial-node-p node)
             (form-constraints-with-variables (random-elt (get-data problem :utterances))
                                              (get-configuration (construction-inventory node) :de-render-mode)))
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holophrase-cxn problem node))))
  
(defmethod repair ((repair nothing->holophrase)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (and (initial-node-p node)
             (form-constraints-with-variables (random-elt (get-data problem :utterances))
                                              (get-configuration (construction-inventory node) :de-render-mode)))
    (make-instance 'fcg::cxn-fix
                   :repair repair
                   :problem problem
                   :restart-data (create-holophrase-cxn problem node))))

(defun create-holophrase-cxn (problem node)
  "Creates a holophrase-cxn."
  (let* ((processing-cxn-inventory (construction-inventory node))
         (cxn-inventory (original-cxn-set processing-cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (cxn-name (make-cxn-name utterance cxn-inventory :add-numeric-tail t))
         (form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (boundaries-holophrase-cxn (get-boundary-units form-constraints))
         (leftmost-unit-holophrase-cxn (first boundaries-holophrase-cxn))
         (rightmost-unit-holophrase-cxn (second boundaries-holophrase-cxn))
         (args-holophrase-cxn (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
         ;; take the last element of the form constraints (the last word) and use it for hashing
         (hash-string (loop for fc in form-constraints
                            when (equalp (first fc) 'string)
                              collect (third fc) into hash-strings
                            finally (return (last-elt hash-strings)))))
    (assert hash-string)
    (second (multiple-value-list  (eval
                                   `(def-fcg-cxn ,cxn-name
                                                 ((?holophrase-unit
                                                   (syn-cat (phrase-type holophrase))
                                                   (args ,args-holophrase-cxn)
                                                   (boundaries
                                                    (left ,leftmost-unit-holophrase-cxn)
                                                    (right ,rightmost-unit-holophrase-cxn)))
                                                  <-
                                                  (?holophrase-unit
                                                   (HASH meaning ,meaning)
                                                   --
                                                   (HASH form ,form-constraints)))
                                                 :attributes (:label fcg::routine
                                                              :cxn-type holophrase
                                                              :repair nothing->holophrase
                                                              :string ,hash-string)
                                                 :cxn-inventory ,(copy-object cxn-inventory)))))))


;; uses standard handle-fix
;; see meta dot handle-fix
