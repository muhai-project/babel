(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; Learning and consolidation ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric learn (solution-node cip consolidate mode &key silent)
  (:documentation "Diagnose, repair, consolidate."))

(defmethod learn ((solution-node t) (cip construction-inventory-processor)
                  (consolidate symbol) (mode (eql :no-learning)) &key silent)
  "No learning."
  (declare (ignore silent)))

(defmethod learn ((solution-node t)
                  (cip construction-inventory-processor)
                  (consolidate symbol)
                  (mode (eql :pattern-finding)) &key silent)
  "Learn based on pattern finding only."
  (let ((cxn-inventory (original-cxn-set (construction-inventory cip))))
    ;; Add diagnostics, repairs and best-solution to cip
    (loop for diagnostic in (reverse (get-configuration cxn-inventory :diagnostics))
          do (add-diagnostic cip diagnostic))
    (loop for repair in (reverse (get-configuration cxn-inventory :repairs))
          do (add-repair cip repair))
    (set-data cip :best-solution-node solution-node)

    ;; Notify learning
    (let ((fixes (second (multiple-value-list (notify-learning cip :trigger 'routine-processing-finished))))
          (best-solution nil)
          (consolidated-cxns nil)
          (consolidated-categories nil)
          (consolidated-links nil)
          (category-mapping nil)) ; mappings from categories in learnt cxns to existing equivalent-cxns
  
      (loop with top-node = (top-node cip)
            for fix in fixes
            for current-node = top-node
            do ;; Add fixes to cip
              (loop for fixed-car in (fixed-cars fix)
                    for child = (cip-add-child current-node fixed-car)
                    do (setf current-node child)
                       (push (type-of (issued-by fix)) (statuses child))
                       (setf (fully-expanded? child) t)
                       (cip-run-goal-tests child cip) ;;to include succeeded status in node statuses
                       (push 'added-by-repair (statuses child))) 
                                                                   
              (when consolidate
                ;; consolidation of cxns
                (let ((equivalent-cxns (loop for cxn in (when (slot-exists-p fix 'fix-constructions)
                                                          (fix-constructions fix))
                                             for equivalent-cxn = (find-cxn cxn cxn-inventory :test #'equivalent-cxn)
                                             if equivalent-cxn
                                               do (setf (attr-val cxn :equivalent-cxn) equivalent-cxn)
                                               and
                                               collect (cons cxn equivalent-cxn) into eq-cxns
                                             else
                                               do (add-cxn cxn cxn-inventory)
                                                  (push cxn consolidated-cxns)
                                                  (when (attr-val cxn :cxn-cat)
                                                    (add-category (attr-val cxn :cxn-cat) cxn-inventory :recompute-transitive-closure nil)
                                                    (push (attr-val cxn :cxn-cat) consolidated-categories))
                                                  (when (attr-val cxn :slot-cats)
                                                    (add-categories (attr-val cxn :slot-cats) cxn-inventory :recompute-transitive-closure nil)
                                                    (setf  consolidated-categories (append (attr-val cxn :slot-cats) consolidated-categories)))
                                             finally (return eq-cxns))))
                
                  ;; consolidation of categorial links
                  (setf category-mapping (append (loop for eq-cxn in equivalent-cxns
                                                       for new-cxn = (car eq-cxn)
                                                       for old-cxn = (cdr eq-cxn)
                                                       for cxn-cat-new-cxn = (attr-val new-cxn :cxn-cat)
                                                       for cxn-cat-old-cxn = (attr-val old-cxn :cxn-cat)
                                                       for slot-cats-new-cxn = (attr-val new-cxn :slot-cats)
                                                       for slot-cats-old-cxn = (attr-val old-cxn :slot-cats)
                                                       when cxn-cat-new-cxn
                                                         collect (cons cxn-cat-new-cxn cxn-cat-old-cxn) into eq-cat-mappings-cxn-cats
                                                       when slot-cats-new-cxn
                                                         append (loop for new-cat in slot-cats-new-cxn
                                                                      for old-cat in slot-cats-old-cxn
                                                                      collect (cons new-cat old-cat)) into eq-cat-mappings-slot-cats
                                                       finally (return (append eq-cat-mappings-cxn-cats eq-cat-mappings-slot-cats)))
                                                 category-mapping))
                  
                  (loop for (cat-1 cat-2 link-type) in (when (slot-exists-p fix 'fix-categorial-links)
                                                         (fix-categorial-links fix))
                        for cat-1-to-add = (or (cdr (assoc cat-1 category-mapping))
                                               cat-1)
                        for cat-2-to-add = (or (cdr (assoc cat-2 category-mapping))
                                               cat-2)
                        unless (link-exists-p cat-1-to-add cat-2-to-add cxn-inventory :link-type link-type)
                          do ;; add links to network
                            (add-link cat-1-to-add cat-2-to-add cxn-inventory :weight 0.5 :link-type link-type :recompute-transitive-closure nil)
                            (push (cons cat-1-to-add cat-2-to-add) consolidated-links)))))
                      
      (when fixes
        (annotate-cip-with-used-categorial-links cip)
        (set-data cip :category-mapping category-mapping)
        (setf best-solution (best-solution cip (get-configuration cxn-inventory :best-solution-mode))))
      
      (unless silent
        (notify meta-level-learning-finished cip best-solution consolidated-cxns consolidated-categories consolidated-links))

      (values best-solution
              fixes))))

