(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add holophrase construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nothing->holistic (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair nothing->holistic)
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
                   :restart-data (create-holistic-cxn problem node))))
  
(defmethod repair ((repair nothing->holistic)
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
                   :restart-data (create-holistic-cxn problem node))))

(defun create-holistic-cxn (problem node)
  (do-create-holistic-cxn
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   nil
   (construction-inventory node)
   node))


(defun do-create-holistic-cxn (form-constraints meaning parent-meaning cxn-inventory node)
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (cxn-name (make-cxn-name form-constraints cxn-inventory :add-numeric-tail t))
         (cxn-name-holistic-cxn-apply-last (intern (concatenate 'string (symbol-name cxn-name) "-APPLY-LAST")))
         (cxn-name-holistic-cxn-apply-first (intern (concatenate 'string (symbol-name cxn-name) "-APPLY-FIRST")))
         (boundaries-holistic-cxn (get-boundary-units form-constraints))
         (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
         (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
         (args-holistic-cxn (extract-args-from-meaning-networks meaning parent-meaning meaning-representation-formalism))
         (existing-holistic-cxn-apply-first
          (find-cxn-by-form-and-meaning form-constraints meaning args-holistic-cxn nil cxn-inventory
                                        :cxn-type 'holistic :cxn-set 'routine))
         (existing-holistic-cxn-apply-last
          (when existing-holistic-cxn-apply-first
            (alter-ego-cxn existing-holistic-cxn-apply-first cxn-inventory)))
         (lex-class-holistic-cxn
          (if existing-holistic-cxn-apply-first
            (extract-contributing-lex-class existing-holistic-cxn-apply-first)
            (make-lex-class cxn-name :trim-cxn-suffix t)))
         ;; take the last element of the form constraints (the last word) and use it for hashing
         (hash-string (loop for fc in form-constraints
                            when (equalp (first fc) 'string)
                            collect (third fc) into hash-strings
                            finally (return (last-elt hash-strings))))
         (cxn-inventory-copy (copy-object cxn-inventory))
         (holistic-cxn-apply-first
          (or existing-holistic-cxn-apply-first
              (second
               (multiple-value-list
                (eval
                 `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-first
                               ((?holistic-unit
                                 (args ,args-holistic-cxn)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn))
                                 (boundaries
                                  (left ,leftmost-unit-holistic-cxn)
                                  (right ,rightmost-unit-holistic-cxn)))
                                <-
                                (?holistic-unit
                                 (HASH meaning ,meaning)
                                 --
                                 (HASH form ,form-constraints)))
                               :attributes (:label fcg::routine
                                            :cxn-type holistic
                                            :is-holophrase ,(when (and node
                                                                       (get-configuration cxn-inventory :mark-holophrases))
                                                              t)
                                            :bare-cxn-name ,cxn-name
                                            :repair nothing->holistic
                                            :meaning ,(fourth (find 'bind meaning :key #'first))
                                            :string ,hash-string)
                               :score ,(get-configuration cxn-inventory :initial-cxn-score)
                               :cxn-inventory ,cxn-inventory-copy))))))
         (holistic-cxn-apply-last
          (or existing-holistic-cxn-apply-last
              (second
               (multiple-value-list
                (eval
                 `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-last
                               (
                                <-
                                (?holistic-unit
                                 (HASH meaning ,meaning)
                                 (args ,args-holistic-cxn)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn))
                                 (boundaries
                                  (left ,leftmost-unit-holistic-cxn)
                                  (right ,rightmost-unit-holistic-cxn))
                                 --
                                 (HASH form ,form-constraints)
                                 (args ,args-holistic-cxn)
                                 (syn-cat (phrase-type holistic)
                                          (lex-class ,lex-class-holistic-cxn))
                                 (boundaries
                                  (left ,leftmost-unit-holistic-cxn)
                                  (right ,rightmost-unit-holistic-cxn))))
                               :attributes (:label fcg::meta-only
                                            :cxn-type holistic
                                            :is-holophrase ,(when (and node
                                                                       (get-configuration cxn-inventory :mark-holophrases))
                                                              t)
                                            :bare-cxn-name ,cxn-name
                                            :repair nothing->holistic
                                            :meaning ,(fourth (find 'bind meaning :key #'first))
                                            :string ,hash-string)
                               :score ,(get-configuration cxn-inventory :initial-cxn-score)
                               :cxn-inventory ,cxn-inventory-copy))))))
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn)))

    (apply-fix
     form-constraints
     cxns-to-apply
     nil
     cxns-to-consolidate
     cats-to-add
     lex-class-holistic-cxn
     t
     node
     'nothing->holistic)
    ))