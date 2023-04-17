(in-package :duckie-language-learning)

;; -------------------------------------------------
;; + Repair:  HOLOPRHASE -> ITEM-BASED W/ DELETION +
;; -------------------------------------------------

(defclass holophrase->item-based--deletion (duckie-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based--deletion)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn"
  (when (gl::initial-node-p node)
    (let* ((reconstructed-intention
            (find-data problem :intention))
           (constructions-and-th-links
            (create-item-based-cxn-deletion
             problem node reconstructed-intention)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun create-item-based-cxn-deletion (problem node intention)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (initial-transient-structure (gl::initial-transient-structure node))
         (utterance (cipn-utterance node))
         (meaning intention))
    (multiple-value-bind (superset-holophrase-cxn
                          non-overlapping-form
                          non-overlapping-meaning)
        (gl::find-superset-holophrase-cxn initial-transient-structure
                                          cxn-inventory meaning
                                          utterance)
      (when superset-holophrase-cxn
        (let* ((overlapping-form
                (set-difference (extract-form-predicates superset-holophrase-cxn)
                                non-overlapping-form :test #'equal))
               (overlapping-meaning
                (set-difference (extract-meaning-predicates superset-holophrase-cxn)
                                non-overlapping-meaning :test #'equal))
               (existing-lex-cxn
                (find-cxn-by-type-form-and-meaning 'lexical non-overlapping-form
                                                   non-overlapping-meaning cxn-inventory))
               (existing-item-based-cxn
                (find-cxn-by-type-form-and-meaning 'item-based overlapping-form
                                                   overlapping-meaning cxn-inventory)))
          (unless (and existing-lex-cxn existing-item-based-cxn)
            (let* ((lex-cxn-name
                    (make-const (gl::make-cxn-name non-overlapping-form cxn-inventory)))
                   (cxn-name-item-based-cxn
                    (gl::make-cxn-name overlapping-form cxn-inventory :add-cxn-suffix nil))
                   (unit-name-lex-cxn
                    (second (find 'string non-overlapping-form :key #'first)))
                   ;; lex-class
                   (lex-class-lex-cxn
                    (if existing-lex-cxn
                      (gl::lex-class-cxn existing-lex-cxn)
                      (intern (symbol-name (make-const unit-name-lex-cxn)) :fcg)))
                   (lex-class-item-based-cxn
                    (if existing-item-based-cxn
                      (loop for unit in (contributing-part existing-item-based-cxn)
                            for lex-class = (gl::lex-class-item-based unit)
                            when lex-class return lex-class)
                      (intern (symbol-name (make-const cxn-name-item-based-cxn)) :fcg)))
                   ;; type hierachy links
                   (th-link-1
                    (cons lex-class-lex-cxn lex-class-item-based-cxn))
                   ;; args: 
                   (args-lex-cxn
                    (third (first non-overlapping-meaning))) ;; third if bind
                   (holophrase-cxn-name
                    (make-const
                     (gl::make-cxn-name
                      (remove-spurious-spaces
                       (remove-punctuation utterance))
                      cxn-inventory)))
                   (form-constraints
                    (gl::form-constraints-with-variables
                     utterance (get-configuration cxn-inventory :de-render-mode)))
                   ;; cxns
                   (initial-cxn-score
                    0.5)
                
                   (holophrase-cxn
                    (second
                     (multiple-value-list
                      (eval
                       `(def-fcg-cxn ,holophrase-cxn-name
                                     ((?holophrase-unit
                                       (syn-cat (gl::phrase-type holophrase)))
                                      <-
                                      (?holophrase-unit
                                       (HASH meaning ,meaning)
                                       --
                                       (HASH form ,form-constraints)))
                                     :attributes (:cxn-type gl::holophrase
                                                  :repair holo->item
                                                  :score ,initial-cxn-score
                                                  :string ,(form-predicates->hash-string form-constraints)
                                                  :meaning ,(meaning-predicates->hash-meaning meaning))
                                     :cxn-inventory ,(copy-object cxn-inventory)
                                     :cxn-set holoprhase)))))
                   (lex-cxn
                    (if existing-lex-cxn
                      existing-lex-cxn
                      (second
                       (multiple-value-list
                        (eval
                         `(def-fcg-cxn
                           ,lex-cxn-name
                           ((,unit-name-lex-cxn
                             (args (,args-lex-cxn))
                             (syn-cat (gl::phrase-type lexical)
                                      (gl::lex-class ,lex-class-lex-cxn)))
                            <-
                            (,unit-name-lex-cxn
                             (HASH meaning ,non-overlapping-meaning)
                             --
                             (HASH form ,non-overlapping-form)))
                           :attributes (:cxn-type gl::lexical
                                        :repair holo->item
                                        :score ,initial-cxn-score
                                        :string ,(form-predicates->hash-string non-overlapping-form)
                                        :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning))
                           :cxn-inventory ,(copy-object cxn-inventory)
                           :cxn-set non-holophrase))))))
                       (item-based-cxn
                        (or existing-item-based-cxn
                            (second
                             (multiple-value-list
                              (eval
                               `(def-fcg-cxn
                                 ,(make-const (gl::add-cxn-suffix cxn-name-item-based-cxn))
                                 ((?item-based-unit
                                   (syn-cat (gl::phrase-type item-based))
                                   (subunits (,unit-name-lex-cxn)))
                                  (,unit-name-lex-cxn 
                                   (syn-cat (gl::lex-class ,lex-class-item-based-cxn)))
                                  <-
                                  (?item-based-unit
                                   (HASH meaning ,overlapping-meaning)
                                   --
                                   (HASH form ,overlapping-form))
                                  (,unit-name-lex-cxn
                                   (args (,args-lex-cxn))
                                   --))
                                 :attributes (:cxn-type gl::item-based
                                              :repair holo->item
                                              :score ,initial-cxn-score
                                              :string ,(form-predicates->hash-string overlapping-form)
                                              :meaning ,(meaning-predicates->hash-meaning overlapping-meaning))
                                 :cxn-inventory ,(copy-object cxn-inventory)
                                 :cxn-set non-holophrase)))))))
              ;(add-composer-chunk agent overlapping-meaning)
              ;(set-data interaction :applied-repair 'holophrase->item-based)
              ;; returns 1. existing cxns to apply
              ;; 2. new cxns to apply
              ;; 3. other new cxns
              ;; 4. th links
              (list nil (list holophrase-cxn)
                    (list lex-cxn item-based-cxn)
                    (list th-link-1)))))))))
