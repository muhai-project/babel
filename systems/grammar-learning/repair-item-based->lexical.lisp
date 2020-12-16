(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add lexical construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-lexical-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair add-lexical-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction."
  (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
    (when lex-cxn-and-th-link
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-th-link))))
  
(defmethod repair ((repair add-lexical-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction."
  (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
    (when lex-cxn-and-th-link 
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-th-link))))

(defun find-matching-lex-cxns-in-root (cxn-inventory root-strings)
  (remove nil (loop for remaining-form in root-strings
        for root-string = (third remaining-form)
        collect (loop for cxn in (constructions cxn-inventory)
                      when (and (eql (phrase-type cxn) 'lexical)
                                (string= (third (first (extract-form-predicates cxn))) root-string))
                      return cxn))))

(defun subtract-lex-cxn-meanings (lex-cxns gold-standard-meaning)
  (let ((lex-cxn-meanings (map 'list #'extract-meaning-predicates lex-cxns)))
    (loop for lex-cxn-meaning in lex-cxn-meanings
          do (setf gold-standard-meaning (set-difference gold-standard-meaning lex-cxn-meaning :test #'irl:unify-irl-programs)))
    gold-standard-meaning
  ))

(defun subtract-lex-cxn-forms (lex-cxns string-predicates-in-root)
    (loop for lex-cxn in lex-cxns
          for lex-form = (extract-form-predicates lex-cxn)
          do (setf string-predicates-in-root (set-difference string-predicates-in-root lex-form :test #'irl:unify-irl-programs)))
    string-predicates-in-root
  )

(defun create-lexical-cxn (problem node)
  "Creates a lexical cxn."
  (let* ((observation (left-pole-structure (car-resulting-cfs (cipn-car node))))
         (item-based-cxn (first (filter-by-phrase-type 'item-based (applied-constructions node))))
         (string-predicates-in-root (form-predicates-with-variables (extract-string (get-root observation)))))

    ;; there is more than one string in root, but there can be a matching lex cxn with missing th links that can be subtracted
    (when (and (> (length string-predicates-in-root) 0)
               item-based-cxn)
      (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
             (matching-lex-cxns (find-matching-lex-cxns-in-root cxn-inventory string-predicates-in-root)))
        ;; there are one or more lex cxns, and one remaining string in root
        (when (or (and matching-lex-cxns
                       (= 1 (- (length string-predicates-in-root) (length matching-lex-cxns))))
                  (= (length string-predicates-in-root) 1))
          ;; construct the remaining cxn first
          (let* ((utterance (random-elt (get-data problem :utterances)))
                 (type-hierarchy (get-type-hierarchy cxn-inventory))
                 (meaning-predicates-gold (meaning-predicates-with-variables (first (get-data problem :meanings))))
                 (meaning-predicates-gold-minus-lex (subtract-lex-cxn-meanings matching-lex-cxns meaning-predicates-gold))
                 (meaning-predicates-observed (extract-meanings observation))
                 (meaning-predicates-lex-cxn (if (= 1 (length string-predicates-in-root))
                                               (set-difference meaning-predicates-gold meaning-predicates-observed :test #'unify)
                                               (set-difference meaning-predicates-gold-minus-lex meaning-predicates-observed :test #'unify)))
                 (form-predicates-lex-cxn (if (= 1 (length string-predicates-in-root))
                                            string-predicates-in-root
                                            (subtract-lex-cxn-forms matching-lex-cxns string-predicates-in-root)))
                 (existing-lex-cxn (find-cxn-by-form-and-meaning form-predicates-lex-cxn meaning-predicates-lex-cxn cxn-inventory))
                 (cxn-name (make-cxn-name (third (first form-predicates-lex-cxn)) cxn-inventory))
                 (unit-name (second (first form-predicates-lex-cxn)))
                 (lex-class (if existing-lex-cxn
                              (lex-class-cxn existing-lex-cxn)
                              (intern (symbol-name (make-const unit-name)) :type-hierarchies)))
                 (args (mapcar #'third meaning-predicates-lex-cxn))
                 (new-lex-cxn (or existing-lex-cxn (second (multiple-value-list (eval
                                                                                 `(def-fcg-cxn ,cxn-name
                                                                                               ((,unit-name
                                                                                                 (syn-cat (phrase-type lexical)
                                                                                                          (lex-class ,lex-class))
                                                                                                 (args ,args))
                                                                                                <-
                                                                                                (,unit-name
                                                                                                 (HASH meaning ,meaning-predicates-lex-cxn)
                                                                                                 --
                                                                                                 (HASH form ,form-predicates-lex-cxn)))
                                                                                               :cxn-inventory ,(copy-object cxn-inventory)))))))
                 ;; make a list of all cxns, sort them
                 (applied-lex-cxns (filter-by-phrase-type 'lexical (applied-constructions node)))
                 (lex-cxns (sort-cxns-by-form-string (append
                                                      (list new-lex-cxn)
                                                      matching-lex-cxns
                                                      applied-lex-cxns) utterance))
                 (lex-classes-lex-cxns (when lex-cxns (map 'list #'lex-class-cxn lex-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
                 ;; assign all th links
                 (th-links (when (and lex-classes-lex-cxns
                                      lex-classes-item-based-units
                                      (= (length lex-classes-lex-cxns) (length lex-classes-item-based-units)))
                             (create-new-th-links lex-classes-lex-cxns lex-classes-item-based-units type-hierarchy))))
            ;; return
            (list new-lex-cxn (append (list (get-processing-cxn item-based-cxn))
                                      (list (get-processing-cxn new-lex-cxn))
                                      (unless (= 1 (length string-predicates-in-root))
                                        (map 'list #'get-processing-cxn matching-lex-cxns))
                                      (map 'list #'get-processing-cxn applied-lex-cxns)) th-links)))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-lexical-cxn) (problem problem) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((new-lex-cxn (first (restart-data fix)))
           (cxns (second (restart-data fix)))
           (th-links (third (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th-flat-list nil)
           (th (loop for th-list in th-links
                     do (loop for th-link in th-list
                              do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                              (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                              (setf th-flat-list (append th-flat-list (list th-link))))
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy)))
           (last-node  (initial-node node))
           (applied-nodes (loop for cxn in cxns
                                do (wi:add-element (make-html cxn))
                                (setf last-node (fcg::cip-add-child last-node (first (fcg-apply cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car last-node))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                                   (direction (cip node))
                                                                                                   :configuration (configuration (construction-inventory node))
                                                                                                   :cxn-inventory (construction-inventory node)))))
                                collect last-node)))
      ;; ignore
      ;; Reset type hierarchy
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of last new node
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-cxns (list new-lex-cxn))
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to last new node
      (setf (cxn-supplier last-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses last-node))
      (push 'added-by-repair (statuses last-node))
      ;; enqueue only last new node; never backtrack over the first applied construction, we applied them as a block
      (cip-enqueue last-node (cip node) (get-configuration node :queue-mode)))))
