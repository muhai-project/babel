(in-package :duckie-language-learning)

;; ---------------------------------
;; + Repair: ITEM-BASED -> LEXICAL +
;; ---------------------------------

(define-event item-based->lexical-repair-started)
(define-event item-based->lexical-new-cxn-and-categorial-links
  (cxn construction) (categorial-network categorial-network) (new-links list))

(defclass item-based->lexical (duckie-learning-repair)
  ((trigger :initform 'fcg::new-node)))

;; This repair is applied when a partial utterance was diagnosed.
(defmethod repair ((repair item-based->lexical)
                   (problem partial-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  (let ((lex-cxn-and-categorial-link (create-lexical-cxn problem node)))
    (when lex-cxn-and-categorial-link
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-categorial-link))))

(defun create-lexical-cxn (problem node)
  ;(notify item-based->lexical-repair-started)
  (let* (;; intention reading
         (agent (find-data problem :owner))
         (answer (find-data problem :answer))
         ;; pattern finding
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         ;; what was able to apply?
         (applied-cxns (original-applied-constructions node))
         (applied-lex-cxns (find-all 'lexical applied-cxns :key #'get-cxn-type))
         (applied-item-based-cxn (find 'item-based applied-cxns :key #'get-cxn-type))
         (remaining-strings-in-root (get-strings-from-root node)))
    (when (and (not (null applied-item-based-cxn))
               (= (length remaining-strings-in-root) 1)
               (= (item-based-number-of-slots applied-item-based-cxn)
                  (1+ (length applied-lex-cxns))))
      (let* ((meaning-predicates-observed (mapcan #'extract-meaning-predicates applied-cxns))
             (composer-solution (compose-program agent answer utterance :partial-program meaning-predicates-observed)))
        (if composer-solution
          (let* ((meaning-predicates-lex-cxn (set-difference composer-solution
                                                             meaning-predicates-observed
                                                             :test #'unify-irl-programs)))
            ;; we don't know what the composer will return
            ;; so we make sure that the meaning for the new
            ;; lex cxn only contains a single element
            (if (length= meaning-predicates-lex-cxn 1)
              (let* ((form-predicates-lex-cxn
                      remaining-strings-in-root)
                     (existing-lex-cxn (find-cxn-by-type-form-and-meaning 'lexical form-predicates-lex-cxn
                                                                          meaning-predicates-lex-cxn cxn-inventory))
                     (cxn-name (make-const
                                (make-cxn-name
                                 (third (first form-predicates-lex-cxn)) cxn-inventory)))
                     (unit-name (second (first form-predicates-lex-cxn)))
                     (lex-class (if existing-lex-cxn
                                  (lex-class-cxn existing-lex-cxn)
                                  (intern (symbol-name (make-const unit-name)) :fcg)))
                     (args (mapcar #'third meaning-predicates-lex-cxn))
                     (initial-cxn-score (get-configuration agent :initial-cxn-score))
                     (new-lex-cxn (or existing-lex-cxn
                                      (second
                                       (multiple-value-list
                                        (eval
                                         `(def-fcg-cxn
                                           ,cxn-name
                                           ((,unit-name
                                             (syn-cat (phrase-type lexical)
                                                      (lex-class ,lex-class))
                                             (args ,args))
                                            <-
                                            (,unit-name
                                             (HASH meaning ,meaning-predicates-lex-cxn)
                                             --
                                             (HASH form ,form-predicates-lex-cxn)))
                                           :attributes (:score ,initial-cxn-score
                                                        :cxn-type lexical
                                                        :repair item->lex
                                                        :string ,(form-predicates->hash-string form-predicates-lex-cxn)
                                                        :meaning ,(meaning-predicates->hash-meaning meaning-predicates-lex-cxn))
                                           :cxn-inventory ,(copy-object cxn-inventory)
                                           :cxn-set non-holophrase))))))
                     ;; make a list of all cxns, sort them
                     (lex-cxns (sort-cxns-by-form-string
                                (cons new-lex-cxn applied-lex-cxns)
                                (remove-punctuation utterance)))
                     (lex-classes-lex-cxns (mapcar #'lex-class-cxn lex-cxns))
                     (lex-classes-item-based-units (get-all-unit-lex-classes applied-item-based-cxn))
                     ;; assign all categorial links
                     (categorial-network (categorial-network cxn-inventory))
                     (categorial-links (when (and lex-classes-lex-cxns
                                                  lex-classes-item-based-units
                                                  (length= lex-classes-lex-cxns lex-classes-item-based-units))
                                         (create-new-categorial-links lex-classes-lex-cxns
                                                                      lex-classes-item-based-units
                                                                      categorial-network))))
                ;; returns
                ;; 1. existing cxns to apply
                ;; 2. new cxns to apply
                ;; 3. other new cxns
                ;; 4. categorial links
                (if categorial-links
                  (progn
                    (if existing-lex-cxn
                      (list (cons new-lex-cxn applied-cxns) nil nil categorial-links)
                      (list applied-cxns (list new-lex-cxn) nil categorial-links)))
                  (progn (push 'fcg::repair-failed (fcg::statuses node)) nil)))
              (progn (push 'fcg::repair-failed (fcg::statuses node)) nil)))
          (progn (push 'fcg::repair-failed (fcg::statuses node)) nil))))))
