(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;
        
(defun make-holistic-cxn (form meaning form-args meaning-args holophrasep cxn-inventory)
  (let* (;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :holistic-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn form meaning form-args meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; grammatical category
         (category-holistic-cxn
          (if existing-routine-holistic-cxn
            (extract-top-category-holistic-cxn existing-routine-holistic-cxn)
            (make-grammatical-category cxn-name :trim-cxn-suffix t :numeric-suffix t)))
         ;; temp cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first category-holistic-cxn
                                                 form meaning form-args meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 holophrasep cxn-inventory-copy)))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last category-holistic-cxn
                                                form meaning form-args meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                holophrasep cxn-inventory-copy))))
    ;; done!
    (apply-fix :form-constraints form
               :cxns-to-apply (list holistic-cxn-apply-first)
               :cxns-to-consolidate (list holistic-cxn-apply-last)
               :categories-to-add (list category-holistic-cxn)
               :top-level-category category-holistic-cxn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make n holistic cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gather-predicates-from-initial-variables (initial-variables pool-of-predicates)  
  (let ((set-of-predicates
         (loop for predicate in pool-of-predicates
               when (find-if #'(lambda (elem) (member elem initial-variables)) predicate)
               collect predicate)))
    (loop for variables = (find-all-anywhere-if #'variable-p set-of-predicates)
          for next-set-of-predicates = (loop for predicate in pool-of-predicates
                                             when (find-if #'(lambda (elem) (member elem variables)) predicate)
                                             collect predicate)
          while (> (length next-set-of-predicates) (length set-of-predicates))
          do (setf set-of-predicates next-set-of-predicates))
    set-of-predicates))

(defun make-n-holistic-cxns (form meaning form-arg-groups meaning-arg-groups cxn-inventory)  
  (let* ((holistic-cxns-forms-and-meanings
          (loop for form-arg-group in form-arg-groups
                for category = (first form-arg-group)
                for meaning-arg-group = (find category meaning-arg-groups :key #'first)
                for holistic-cxn-form = (gather-predicates-from-initial-variables (rest form-arg-group) form)
                for holistic-cxn-meaning = (gather-predicates-from-initial-variables (rest meaning-arg-group) meaning)
                collect (list holistic-cxn-form holistic-cxn-meaning category)))
         (leftover-form
          (set-difference form (mappend #'first holistic-cxns-forms-and-meanings) :test #'equal))
         (leftover-meaning
          (set-difference meaning (mappend #'second holistic-cxns-forms-and-meanings) :test #'equal)))
    ;; when there is leftover form or meaning
    ;; simply add it to the first holistic cxn
    (when (or leftover-form leftover-meaning)
      (setf (first (first holistic-cxns-forms-and-meanings))
            (append (first (first holistic-cxns-forms-and-meanings)) leftover-form))
      (setf (second (first holistic-cxns-forms-and-meanings))
            (append (second (first holistic-cxns-forms-and-meanings)) leftover-meaning)))
    ;; actually make the holistic cxns
    (loop for (holistic-cxn-form holistic-cxn-meaning category) in holistic-cxns-forms-and-meanings
          for holistic-cxn-form-args = (rest (find category form-arg-groups :key #'first))
          for holistic-cxn-meaning-args = (rest (find category meaning-arg-groups :key #'first))
          for recursion-args
            = (make-blackboard :data-fields (list (cons :top-lvl-form-args holistic-cxn-form-args)
                                                  (cons :top-lvl-meaning-args holistic-cxn-meaning-args)))
          for holistic-apply-fix-result
            ;= (make-holistic-cxn holistic-cxn-form holistic-cxn-meaning holistic-cxn-form-args holistic-cxn-meaning-args cxn-inventory)
            = (handle-potential-holistic-cxn holistic-cxn-form holistic-cxn-meaning recursion-args cxn-inventory)
          collect holistic-apply-fix-result)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; make item-based cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-item-based-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  ;; slot-args are lists of lists
  ;; the length of these lists is the number of units
  ;; should be equal on form side and meaning side!
  (assert (length= slot-form-args slot-meaning-args))
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; find an identical existing item-based cxn
         (existing-routine-item-based-cxn
          (find-identical-item-based-cxn form meaning top-lvl-form-args top-lvl-meaning-args
                                         slot-form-args slot-meaning-args cxn-inventory))
         (existing-meta-item-based-cxn
          (when existing-routine-item-based-cxn
            (alter-ego-cxn existing-routine-item-based-cxn cxn-inventory)))
         ;; lex classes
         (top-cat-item-based
          (if existing-routine-item-based-cxn
            (extract-top-category-item-based-cxn existing-routine-item-based-cxn)
            (make-grammatical-category (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t)))
         (slot-cats-item-based
          (if existing-routine-item-based-cxn
            (extract-slot-categories-item-based-cxn existing-routine-item-based-cxn)
            (loop repeat (length slot-form-args)
                  collect (make-grammatical-category (symbol-name bare-cxn-name)
                                                     :trim-cxn-suffix t :numeric-suffix t :slotp t))))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; build cxns!
         (item-based-cxn-apply-last
          (or existing-routine-item-based-cxn
              (item-based-cxn-apply-last-skeleton bare-cxn-name cxn-name-apply-last
                                                  top-cat-item-based slot-cats-item-based
                                                  form meaning
                                                  top-lvl-form-args top-lvl-meaning-args
                                                  slot-form-args slot-meaning-args
                                                  (get-configuration cxn-inventory :initial-cxn-score)
                                                  cxn-inventory-copy)))
         (item-based-cxn-apply-first
          (or existing-meta-item-based-cxn
              (item-based-cxn-apply-first-skeleton bare-cxn-name cxn-name-apply-first
                                                   top-cat-item-based slot-cats-item-based
                                                   form meaning
                                                   top-lvl-form-args top-lvl-meaning-args
                                                   slot-form-args slot-meaning-args
                                                   (get-configuration cxn-inventory :initial-cxn-score)
                                                   cxn-inventory-copy))))
    ;; done!
    (apply-fix :form-constraints form
               :cxns-to-apply (list item-based-cxn-apply-last)
               :cxns-to-consolidate (list item-based-cxn-apply-first)
               :categories-to-add (cons top-cat-item-based slot-cats-item-based)
               :top-level-category top-cat-item-based)))