(ql:quickload :pattern-finding)
(in-package :pf)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))


(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions))


;; default: use string and meets as form-representation
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:corpus-file . ,(make-pathname :directory '(:relative "val")
                                                              :name "stage-1" :type "jsonl"))))))

;; use sequences as form-representation
;; also requires different cxn supplier!
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:form-representation . :sequences)
                              (:learner-cxn-supplier . :ordered-by-label-and-positive-score)
                              (:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:corpus-file . ,(make-pathname :directory '(:relative "val")
                                                              :name "stage-1" :type "jsonl"))))))

(length (corpus *experiment*))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 10)

;;;; Showing the cxn inventory and categorial network

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory* :sort-by-type-and-score t))
(add-element (make-html (categorial-network *cxn-inventory*)))

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
(remove-cxns-learned-at *experiment* 144)

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
          for alter-ego-cxn = (alter-ego-cxn cxn grammar)
          do (delete-cxn (name cxn) grammar :key #'name)
             (delete-cxn (name alter-ego-cxn) grammar :key #'name))))


;;;; Changing the order of repairs on the fly

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

(loop for repair in (get-repairs *cxn-inventory*)
      do (delete-repair *cxn-inventory* repair))

(loop for repair-name in '(nothing->holistic
                           anti-unify-partial-analysis 
                           anti-unify-cxn-inventory
                           add-categorial-links)
      do (add-repair *cxn-inventory* repair-name))


