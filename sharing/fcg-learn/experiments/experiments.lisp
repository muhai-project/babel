(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; FCG Learn experiment configurations ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :fcg-learn)
;; (activate-monitor trace-fcg-learning)
;; (deactivate-monitor trace-fcg-learning)
;; (activate-monitor trace-fcg-search-process)
;; (activate-monitor trace-fcg-debugging)
(deactivate-all-monitors)
(activate-monitor trace-fcg-learning-in-output-browser)
(activate-monitor trace-fcg-learning-in-output-browser-verbose)

(def-fcg-constructions clevr-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :hashed-with-regex-check)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:hash-mode . :filler-and-linking)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs  repair-add-categorial-link repair-through-anti-unification)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :punish-other-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.5)
                       (:best-solution-mode . :highest-average-link-weight)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:fix-selection-mode . :max-reuse)
                       (:form-generalisation-mode :altschul-erickson
                        ((:match-cost . 0)
                         (:mismatch-cost . 1)
                         (:gap-cost . 1)
                         (:gap-opening-cost . 5)
                         (:n-optimal-alignments . nil)
                         (:max-nr-of-gaps . 1)))
                       (:meaning-generalisation-mode . :exhaustive)
                       (:max-nr-of-nodes . 5000)
                       (:k-swap-k . 1)
                       (:k-swap-w . 1)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence-predicates)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:expand-nodes-in-search-tree . t)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))


(defparameter *clevr-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning-ipa" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;;Takes 10-20 seconds to load corpus
(defparameter *clevr-stage-1-train-processor* (load-corpus *clevr-stage-1-train* :sort-p t :remove-duplicates t :ipa nil))
(defparameter *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))


(setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))


(setf (counter *clevr-stage-1-train-processor*) 0)
(comprehend *clevr-stage-1-train-processor* :cxn-inventory *clevr-stage-1-grammar*  :nr-of-speech-acts 1)


(loop for cxn in (constructions-list *clevr-stage-1-grammar*)
      do (setf (attr-val cxn :fix) nil))

(defun run-speech-acts (from to series cxn-inventory corpus-processor)
  (loop for i from 1 to series
        do (setf (counter corpus-processor) from)
           (comprehend corpus-processor :cxn-inventory cxn-inventory  :nr-of-speech-acts (- to from))))

(run-speech-acts 0 47134 1 *clevr-stage-1-grammar* *clevr-stage-1-train-processor*)

(comprehend (nth-speech-act *clevr-stage-1-train-processor* 11991)  :cxn-inventory *clevr-stage-1-grammar*)

(comprehend (next-speech-act *clevr-stage-1-train-processor*)  :cxn-inventory *clevr-stage-1-grammar*)

(set-data (blackboard *clevr-stage-1-grammar*) :matched-categorial-links nil)

(store-grammar *clevr-stage-1-grammar*)

(progn
  (reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
  (comprehend *clevr-stage-1-train-processor*
              :cxn-inventory *clevr-stage-1-grammar*
              :nr-of-speech-acts 100) ;;(array-dimension (corpus *clevr-stage-1-train-processor*) 0)
  )

unify-atom find-duplicate


(deactivate-monitor trace-fcg-learning)
(activate-monitor trace-fcg-learning)

(reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
(comprehend *clevr-stage-1-train-processor* :cxn-inventory *clevr-stage-1-grammar* :nr-of-speech-acts 1)

(add-element (make-html (meaning (nth-speech-act *clevr-stage-1-train-processor* 20))))
(inspect (cip  *saved-cipn*))

(gold-standard-solution-p (fcg-get-transient-structure (get-data (cip *saved-cipn*) :best-solution-node))
                          (nth-speech-act *clevr-stage-1-train-processor* 469)
                          (direction (cip *saved-cipn*))
                          (configuration (construction-inventory (cip *saved-cipn*))))

(form (speech-act (attr-val *saved-cxn* :fix)))

(inspect *saved-cxn*)

(inspect *saved-cipn*)

(add-element (make-html (categorial-network *clevr-stage-1-grammar*)))


unify-atom rename-variables




(defun equivalent-nodes (node-1 node-2))


;; RE checken in cxn supplier!!! (geen safe-cxn nodig)


find-duplicate








(defun duplicate-nodes? (node other-node)
  (and (not (eq node other-node))
       (not (duplicate other-node))
       (permutation-of? (applied-constructions node)
                        (applied-constructions other-node)
                        :key #'name)
       ;; (equivalent-node? node other-node)
       (equivalent-coupled-feature-structures 
        (car-resulting-cfs (cipn-car node))
        (car-resulting-cfs (cipn-car other-node)))))

(defun find-duplicate (node other-node)
  (let ((parent-nodes (all-parents node )))
  
  (or (when (duplicate-nodes? node other-node) other-node)
      (loop for child in (children other-node)
            for duplicate = (find-duplicate node child)
            when duplicate do (return duplicate))))

(defmethod cip-node-test ((node cip-node) (mode (eql :check-duplicate)))
  "Checks whether the node is a duplicate of another one in the tree"
  (let ((duplicate (find-duplicate node (top-node (cip node)))))
    (if duplicate
      (progn
        (setf (duplicate node) duplicate)
        (push 'duplicate (statuses node))
        nil)
      t)))