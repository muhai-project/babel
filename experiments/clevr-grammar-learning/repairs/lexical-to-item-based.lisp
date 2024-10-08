;;;; lexical->item-based.lisp

(in-package :cgl)

;;  LEXICAL -> ITEM-BASED
;; -----------------------

(define-event lexical->item-based-repair-started)
(define-event lexical->item-based-new-cxn-and-links
  (cxn construction) (th categorial-network) (new-links list))

(defclass lexical->item-based (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))

;; This repair is applied when a partial utterance was diagnosed.

(defmethod repair ((repair lexical->item-based)
                   (problem partial-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  (let ((constructions-and-th-links
         (create-item-based-cxns-from-lex problem node)))
    (when constructions-and-th-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data constructions-and-th-links))))

(defun create-item-based-cxns-from-lex (problem node)
  ;(notify lexical->item-based-repair-started)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         (applied-cxns (original-applied-constructions node))
         (applied-lex-cxns
          (find-all 'lexical applied-cxns :key #'get-cxn-type))
         (applied-item-based-cxn
          (find 'item-based applied-cxns :key #'get-cxn-type)))
    (when (and applied-lex-cxns (null applied-item-based-cxn))
      (let* ((partial-program
              (deduplicate-variables
               (mapcan #'extract-meaning-predicates
                       applied-lex-cxns)))
             (composer-strategy
              (get-configuration agent :composer-strategy))
             (composer-solution
              (compose-program agent (topic agent) utterance composer-strategy
                               :partial-program partial-program)))
        (if composer-solution
          (let* ((new-irl-program
                  (append (bind-statements composer-solution)
                          (irl-program (chunk composer-solution))))
                 (sorted-lex-cxns
                  (sort-cxns-by-form-string
                   applied-lex-cxns
                   (remove-spurious-spaces
                    (remove-punctuation utterance))))
                 (var-form
                  (form-constraints-with-variables
                   utterance (get-configuration cxn-inventory :de-render-mode)))
                 (subunit-names-and-non-overlapping-form
                  (multiple-value-list
                   (diff-non-overlapping-form var-form sorted-lex-cxns)))
                 (subunit-names
                  (first subunit-names-and-non-overlapping-form))
                 (non-overlapping-form
                  (second subunit-names-and-non-overlapping-form))
                 (args-and-non-overlapping-meaning
                  (multiple-value-list
                   (diff-non-overlapping-meaning new-irl-program sorted-lex-cxns)))
                 (args
                  (first args-and-non-overlapping-meaning))
                 (non-overlapping-meaning
                  (second args-and-non-overlapping-meaning)))
            (if (length= subunit-names args) ;; !!!
              (let* ((cxn-name-item-based-cxn
                      (make-const (make-cxn-name non-overlapping-form cxn-inventory)))
                     (rendered-cxn-name-list
                      (make-cxn-placeholder-name non-overlapping-form cxn-inventory))
                     (placeholder-list
                      (extract-placeholder-var-list rendered-cxn-name-list))
                     (existing-item-based-cxn
                      (find-cxn-by-type-form-and-meaning 'item-based  non-overlapping-form
                                                         non-overlapping-meaning cxn-inventory))
                     (th-links
                      (if existing-item-based-cxn
                        (mapcar #'cons (mapcar #'lex-class-cxn sorted-lex-cxns)
                                (get-all-unit-lex-classes existing-item-based-cxn))
                        (create-type-hierarchy-links sorted-lex-cxns
                                                         (format nil "~{~a~^-~}"
                                                                 rendered-cxn-name-list)
                                                         placeholder-list
                                                         :item-based-numeric-tail t)))
                     (lex-cxn-subunit-blocks
                      (multiple-value-list
                       (subunit-blocks-for-lex-cxns sorted-lex-cxns subunit-names args th-links)))
                     (lex-cxn-conditional-units
                      (first lex-cxn-subunit-blocks))
                     (lex-cxn-contributing-units
                      (second lex-cxn-subunit-blocks))
                     (initial-cxn-score
                      (get-configuration agent :initial-cxn-score))
                     (interaction
                      (current-interaction (experiment agent)))
                     (interaction-nr
                      (interaction-number interaction))
                     (item-based-cxn
                      (or existing-item-based-cxn
                          (second
                           (multiple-value-list
                            (eval
                             `(def-fcg-cxn
                               ,cxn-name-item-based-cxn
                               ((?item-based-unit
                                 (syn-cat (phrase-type item-based))
                                 (subunits ,subunit-names))
                                ,@lex-cxn-contributing-units
                                <-
                                (?item-based-unit
                                 (HASH meaning ,non-overlapping-meaning)
                                 --
                                 (HASH form ,non-overlapping-form))
                                ,@lex-cxn-conditional-units)
                               :attributes (:cxn-type item-based
                                            :repair lex->item
                                            :score ,initial-cxn-score
                                            :string ,(form-predicates->hash-string non-overlapping-form)
                                            :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning)
                                            :added-at ,interaction-nr)
                               :cxn-inventory ,(copy-object cxn-inventory)
                               :cxn-set non-holophrase)))))))
                ;(add-composer-chunk agent non-overlapping-meaning)
                (set-data interaction :applied-repair 'lexical->item-based)
                ;; returns 1. existing cxns to apply
                ;; 2. new cxns to apply
                ;; 3. other new cxns
                ;; 4. th links
                (if existing-item-based-cxn
                  (list (cons item-based-cxn applied-lex-cxns) nil nil th-links)
                  (list applied-lex-cxns (list item-based-cxn) nil th-links)))
              (progn (push 'fcg::repair-failed (statuses node)) nil)))
          (progn (push 'fcg::repair-failed (statuses node)) nil))))))