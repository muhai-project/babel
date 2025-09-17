(in-package :fcg)

;;;;;;;;;;;;;
;;         ;;
;; Repairs ;;
;;         ;;
;;;;;;;;;;;;;



(defmethod repair ((repair repair-through-anti-unification)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  "Learn constructions through anti-unification."
  (loop with speech-act = (get-data (blackboard (construction-inventory cip)) :speech-act)
        with solution-states = (learn-through-anti-unification speech-act cip)
        with fixes = nil
        for solution-state in solution-states
        for fix-cxn-inventory = (fix-cxn-inventory solution-state)
        do (set-configuration fix-cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) (list :gold-standard))
           (let* ((repair-solution-node (fcg-apply (processing-cxn-inventory fix-cxn-inventory) (initial-cfs cip) (direction cip)))
                  (fixed-cars (mapcar #'cipn-car (reverse (upward-branch repair-solution-node :include-initial nil)))))
             (when (find 'succeeded (statuses repair-solution-node))
               (push (make-instance 'anti-unification-fix
                                    :anti-unification-state solution-state
                                    :repair repair
                                    :problem problem
                                    :base-cxns (remove nil (mapcar #'base-cxn (upward-branch solution-state)))
                                    :fix-constructions (constructions-list fix-cxn-inventory)
                                    :fix-categories (categories fix-cxn-inventory)
                                    :fix-categorial-links (links fix-cxn-inventory)
                                    :fixed-cars fixed-cars
                                    :speech-act speech-act
                                    :cip cip)
                     fixes)))
        finally
          ;; Set fix slot of the fix-constructions before returning the fixes
          (mapcar #'(lambda (fix)
                      (loop for fix-cxn in (fix-constructions fix)
                              do (setf (attr-val fix-cxn :fix) fix))) fixes)
          ;; Set the description slot to the speech act
          (mapcar #'(lambda (fix)
                      (loop for fix-cxn in (fix-constructions fix)
                            do (setf (description fix-cxn) (form speech-act)))) fixes)
          (return (select-fixes fixes (get-configuration (construction-inventory cip) :fix-selection-mode)))))


(defmethod repair ((repair repair-add-categorial-link)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  "Learn by adding a new categorial link."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (category-linking-mode (get-configuration cxn-inventory :category-linking-mode))
         (cxn-supplier-mode (get-configuration cxn-inventory :cxn-supplier-mode))
         (goal-tests (get-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests)))
         (nodes-w-empty-root (remove nil (traverse-depth-first cip :collect-fn #'(lambda (node)
                                                                                    (unless (or (find 'duplicate (statuses node))
                                                                                                (not (empty-root-p node)))
                                                                                      node))))))
    
    ;; Change configurations of cip in order to explore continuations where categories are not yet linked
    (set-configuration cxn-inventory :cxn-supplier-mode :linking-cxns-only)
    (set-configuration cxn-inventory :category-linking-mode :categories-exist)
    (set-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) (list :gold-standard))

    ;; Created fixes
    (loop for node-w-empty-root in nodes-w-empty-root
          for (repair-node new-cip) = (multiple-value-list (fcg-apply (processing-cxn-inventory cxn-inventory)
                                                                      (car-resulting-cfs (cipn-car node-w-empty-root))
                                                                      (direction cip)))
          when (find 'succeeded (statuses repair-node))
            do (annotate-cip-with-used-categorial-links new-cip)
            and
            collect (make-instance 'categorial-link-fix
                                   :repair repair
                                   :problem problem
                                   :fix-categorial-links (remove-duplicates (loop for (cat-1 cat-2 link-type) in (mappend #'(lambda (node)
                                                                                                                      (get-data node :used-categorial-links))
                                                                                                                  (upward-branch repair-node :include-initial nil))
                                                                                  unless (link-exists-p cat-1 cat-2 cxn-inventory)
                                                                                    collect (list cat-1 cat-2 link-type))
                                                                            :test #'equal)
                                   :fixed-cars (append (mapcar #'cipn-car (reverse (upward-branch node-w-empty-root :include-initial nil)))
                                                       (mapcar #'cipn-car (reverse (upward-branch repair-node :include-initial nil))))
                                   :speech-act (get-data (blackboard (construction-inventory cip)) :speech-act)
                                   :cip cip)
            into fixes-with-duplicates
          finally (set-configuration cxn-inventory :category-linking-mode category-linking-mode)
                  (set-configuration cxn-inventory :cxn-supplier-mode cxn-supplier-mode)
                  (set-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) goal-tests)
                  (return (remove-duplicates fixes-with-duplicates :test #'(lambda (fix-1 fix-2)
                                                                             (permutation-of? (fix-categorial-links fix-1)
                                                                                              (fix-categorial-links fix-2)
                                                                                              :test #'equal)))))))
