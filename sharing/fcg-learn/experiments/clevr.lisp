(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; CLEVR grammar learning              ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :fcg-learn)

;; (deactivate-all-monitors)
;; (activate-monitor trace-fcg-learning) ;; uses FCG web interface
;; (activate-monitor trace-fcg-learning-in-output-browser)
;; (activate-monitor trace-fcg-learning-in-output-browser-verbose)


;;#######################################
;; Experiment configurations
;;#######################################

(def-fcg-constructions clevr-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; Search settings:
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:expand-nodes-in-search-tree . t)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth) ;;FCG default
                       (:max-nr-of-nodes . 5000) ;;FCG default
                       (:max-search-depth . 25) ;;FCG default
                       
                       ;; Construction supplier:
                       (:cxn-supplier-mode . :hashed-with-regex-check)
                       
                       ;; Hash mode:
                       (:hash-mode . :filler-and-linking)
                       
                       ;; Meaning representation format:
                       (:meaning-representation-format . :irl)
                       
                       ;; Construction learning settings:
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs  repair-add-categorial-link repair-through-anti-unification)
                       (:consolidate-repairs . t)
                       (:learning-mode . :pattern-finding)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:best-solution-mode . :highest-average-link-weight)
                       (:fix-selection-mode . :max-reuse)
                       (:category-linking-mode . :neighbours)
                       
                       ;; Generalisation modes
                       (:form-generalisation-mode :altschul-erickson
                        ((:match-cost . 0)
                         (:mismatch-cost . 1)
                         (:gap-cost . 1)
                         (:gap-opening-cost . 5)
                         (:n-optimal-alignments . nil)
                         (:max-nr-of-gaps . 1)))
                       (:meaning-generalisation-mode . :exhaustive)
                       (:k-swap-k . 1)
                       (:k-swap-w . 1)

                       ;; Alignment mode + reward and punishment settings
                       (:alignment-mode . :punish-other-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.5)

                       ;; Rendering and de-rendering
                       (:de-render-mode . :de-render-sequence-predicates)
                       (:render-mode . :render-sequences))
  
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))


;;#######################################
;; Learning a grammar for CLEVR stage 1
;;#######################################

(defparameter *clevr-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-like\ datasets" "clevr-grammar-learning-ipa" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;; Takes 10-20 seconds to load the corpus
(defparameter *clevr-stage-1-train-processor* (load-corpus *clevr-stage-1-train* :sort-p t :remove-duplicates t :ipa t))

;; Initialise an empty grammar
(defparameter *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))

;; Run a number of speech acts (stage 1 = 11991 speech acts)
(comprehend *clevr-stage-1-train-processor* :cxn-inventory *clevr-stage-1-grammar*  :nr-of-speech-acts 100)

;; Optionally reset grammar and/or train processor
;;(setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
;;(reset-cp *clevr-stage-1-train-processor*)



;;#############################################
;; Testing learnt grammar on new utterances
;;#############################################

(defparameter *clevr-stage-1-test*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-like\ datasets" "clevr-grammar-learning-ipa" "val")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defparameter *clevr-stage-1-test-processor* (load-corpus *clevr-stage-1-test* :sort-p nil :remove-duplicates t :ipa t))


;; Run a number of speech acts (set learn to NIL to disable meta-layer)
(comprehend *clevr-stage-1-test-processor* :cxn-inventory *clevr-stage-1-grammar* :nr-of-speech-acts 2
            :learn nil :consolidate nil :align nil)

;; TO DO: function for evaluation

;;################################################
;; Continue learning CLEVR grammar for stage  2 
;;################################################

;; Take a copy of the grammar learnt in stage 1 to start from
(defparameter *clevr-stage-2-grammar* (copy-object *clevr-stage-1-grammar*))

;; Store the stage 1 grammar, but first erase fixes and matched categorial links to reduce size
(progn
  (set-data (blackboard *clevr-stage-1-grammar*) :matched-categorial-links nil)
  (loop for cxn in (constructions-list *clevr-stage-1-grammar*)
        do (setf (attr-val cxn :fix) nil))
  (store-grammar *clevr-stage-1-grammar*))


(defparameter *clevr-stage-2-train*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-like\ datasets" "clevr-grammar-learning-ipa" "train")
                                  :name "stage-2" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;; Takes > 10 MINUTES to load the corpus
(defparameter *clevr-stage-2-train-processor* (load-corpus *clevr-stage-2-train* :sort-p nil :remove-duplicates t :ipa t))

;; Run a number of speech acts (stage 1 = 11991 speech acts)
(comprehend *clevr-stage-2-train-processor* :cxn-inventory *clevr-stage-2-grammar*  :nr-of-speech-acts 100)

;; Optionally reset grammar and/or train processor
;;(setf *clevr-stage-2-grammar* (copy-object *clevr-stage-1-grammar*))
;;(reset-cp *clevr-stage-2-train-processor*)



;; Helper functions
;;;;;;;;;;;;;;;;;;;;;


(defun run-speech-acts (from to series cxn-inventory corpus-processor)
  (loop for i from 1 to series
        do (setf (counter corpus-processor) from)
           (comprehend corpus-processor :cxn-inventory cxn-inventory  :nr-of-speech-acts (- to from))))

;;(run-speech-acts 0 47134 1 *clevr-stage-1-grammar* *clevr-stage-1-train-processor*)






#|

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
      t))) |#