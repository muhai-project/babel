(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ;;
;; Selecting au-fixes to consolidate ;;
;;                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric select-fixes (list-of-fixes mode)
  (:documentation "Implements a strategy to select fixes to consolidate from a list of (working) fixes."))

(defmethod select-fixes ((list-of-fixes list) (mode (eql :all)))
  "Returns all fixes."
  list-of-fixes)

(defmethod select-fixes ((list-of-fixes list) (mode (eql :deepest-branch)))
  "Returns all fixes with the highest depth."
  (select-deepest-fixes list-of-fixes))


(defmethod select-fixes ((list-of-fixes list) (mode (eql :deepest-branch-with-highest-cxn-entrenchment)))
  "From all fixes with the highest depth, select those with the highest average entrenchment score."
  (select-fixes-with-highest-cxn-entrenchment (select-deepest-fixes list-of-fixes)))

(defmethod select-fixes ((list-of-fixes list) (mode (eql :single-fix-max-reuse)))
  "From all fixes with the highest depth, select those with most reused cxns, and from those one with the highest average entrenchment score."
  (list (random-elt (select-fixes-with-highest-cxn-entrenchment (select-deepest-fixes (select-fixes-with-most-reused-cxns list-of-fixes))))))

(defmethod select-fixes ((list-of-fixes list) (mode (eql :max-reuse)))
  "From all fixes with the highest depth, select those with most reused cxns, and from those one with the highest average entrenchment score."
  (select-deepest-fixes (select-fixes-with-most-reused-cxns list-of-fixes)))

(defun select-deepest-fixes (list-of-fixes)
  "Returns all fixes with the max depth."
  (loop with sorted-fixes = (sort list-of-fixes #'>= :key #'(lambda (fix) (length (fixed-cars fix))))
        with length-deepest-branch = (length (fixed-cars (first sorted-fixes)))
        for fix in sorted-fixes
        if (= length-deepest-branch (length (fixed-cars fix)))
          collect fix into fixes
        else
          do (return fixes)
        finally (return fixes)))

(defun select-fixes-with-highest-cxn-entrenchment (list-of-fixes)
  "Returns all fixes with the max entrenchment score."
  (loop with fixes-with-average-score = (loop for fix in list-of-fixes
                                              for average-score = (loop for car in (fixed-cars fix)
                                                                        for eq-cxn = (find-cxn (original-cxn (car-applied-cxn car))
                                                                                               (original-cxn-set (construction-inventory (cip fix)))
                                                                                               :test #'equivalent-cxn)
                                                                        for score = (attr-val (if eq-cxn
                                                                                                eq-cxn
                                                                                                (car-applied-cxn car))
                                                                                              :entrenchment-score)
                                                                          sum score into total-score
                                                                        finally (return (/ total-score (length (fixed-cars fix)))))
                                              collect (cons fix average-score))
        with sorted-fixes = (sort fixes-with-average-score #'>= :key #'cdr)
        with highest-average = (cdr (first sorted-fixes))
        for (fix . score) in sorted-fixes
        if (= highest-average score)
          collect fix into fixes
        else
          do (return fixes)
        finally (return fixes)))

(defun select-fixes-with-most-reused-cxns (list-of-fixes)
  "Returns all fixes with most reused cxns."
  (let ((fixes-with-nr-of-reused-cxns-sorted (loop for fix in list-of-fixes
                                                   for nr-of-reused-cxns = (loop for car in (fixed-cars fix)
                                                                                 unless (eql 'linking-cxn (type-of (original-cxn (car-applied-cxn car))))
                                                                                 count (find-cxn (original-cxn (car-applied-cxn car))
                                                                                                 (original-cxn-set (construction-inventory (cip fix)))
                                                                                                 :test #'equivalent-cxn))
                                                   collect (cons fix nr-of-reused-cxns) into fixes-with-nr-of-reused-cxns
                                                   finally (return (sort fixes-with-nr-of-reused-cxns #'>= :key #'cdr)))))
    (loop with highest = (cdr (first fixes-with-nr-of-reused-cxns-sorted))
          for (fix . score) in fixes-with-nr-of-reused-cxns-sorted
          if (= highest score)
             collect fix into fixes
          else
            do (return fixes)
          finally (return fixes))))
          
          