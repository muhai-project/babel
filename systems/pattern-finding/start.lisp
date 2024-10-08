(ql:quickload :pattern-finding)
(in-package :pf)

(progn
  (monitors::deactivate-all-monitors)
  (monitors::activate-monitor trace-fcg)
  (monitors::activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-interactions-in-wi-verbose))

(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  )

;; TO DO
;; Reduce complexity (turn off partial analysis, only apply
;; repairs recursively for holistic parts, ...) and see what
;; types of cxns are being learned. Are the cxns learned
;; through anti-unification compatible with those learned
;; through partial analysis? 



;; TO DO
;; Merge the repair-anti-unify-cipn and repair-anti-unify-cxn?
;; Take ALL anti-unification results (combining anti-unifying the observation
;; with cxns and anti-unifying the observation with partial analyses)
;; and sort the results by cost and try to learn from the cheapest one first?


;; default: use string and meets as form-representation
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:number-of-epochs . 1)
                              (:anti-unification-mode . :heuristic)
                              (:partial-analysis-mode . :heuristic)
                              (:allow-cxns-with-no-strings . nil)
                              (:repair-recursively . t)
                              (:max-nr-of-nodes . 2000)
                              (:corpus-file . ,(make-pathname :directory '(:relative "val")
                                                              :name "stage-1-clean" :type "jsonl"))))))

;; use sequences as form-representation
;; also requires different cxn supplier!
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:form-representation . :sequences)
                              (:learner-cxn-supplier . :cxn-sets-positive-scores)
                              (:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:number-of-epochs . 1)
                              (:repair-recursively . nil)
                              (:corpus-file . ,(make-pathname :directory '(:relative "val")
                                                              :name "stage-1" :type "jsonl"))))))

(length (corpus *experiment*))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 20)

;;;; Showing the cxn inventory and categorial network

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(defparameter *categorial-network* (categorial-network *cxn-inventory*))
(add-element (make-html *cxn-inventory* :sort-by-type-and-score t))
(add-element (make-html (categorial-network *cxn-inventory*)))

(setf *cxn* (find-cxn 'rubber-the-of-cube-item-based-cxn-1-apply-last *cxn-inventory*))

;;;; Manually trying out sentences

(comprehend-all "What number of tiny objects are there?"
                :cxn-inventory *cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?shape-1)
                                         (bind shape-category ?shape-1 thing)
                                         (filter ?set-2 ?set-1 ?size-1)
                                         (bind size-category ?size-1 small)
                                         (count! ?target ?set-2)))

;;;; Time travel

(go-back-n-interactions *experiment* 1)
(remove-cxns-learned-at *experiment* 9)

(defun go-back-n-interactions (experiment n)
  (setf (interactions experiment)
        (subseq (interactions experiment) n)))

(defun remove-cxns-learned-at (experiment at)
  (let ((learned-at-cxns
         (find-all-if #'(lambda (cxn)
                          (string= (format nil "@~a" at)
                                   (attr-val cxn :learned-at)))
                      (constructions (grammar (learner experiment))))))
    (loop with grammar = (grammar (learner experiment))
          for cxn in learned-at-cxns
          do (delete-cxn-and-grammatical-categories cxn grammar))))


;;;; Changing the order of repairs on the fly

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

(loop for repair in (get-repairs *cxn-inventory*)
      do (delete-repair *cxn-inventory* repair))

(loop for repair-name in '(nothing->holistic
                           anti-unify-partial-analysis 
                           anti-unify-cxn-inventory
                           add-categorial-links)
      do (add-repair *cxn-inventory* repair-name))


