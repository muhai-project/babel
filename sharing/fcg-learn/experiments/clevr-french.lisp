(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; CLEVRançais grammar learning        ;;
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

(def-fcg-constructions clevr-french-cxn-inventory
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
                       (:max-nr-of-nodes . 10000) ;;FCG default
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
;; Loading training data for CLEVR French
;;#######################################

(defparameter *clevr-french-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-like\ datasets" "clevr-french-learning-ipa" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;; Takes 10-20 seconds to load the corpus
(defparameter *clevr-french-stage-1-train-processor* (load-corpus *clevr-french-stage-1-train* :sort-p t :remove-duplicates t :ipa t))

;; Initialise an empty grammar
(defparameter *clevr-french-stage-1-grammar* (make-clevr-french-cxn-inventory-cxns))


;;(defparameter *first-500-shuffled* (shuffle-first-nth-speech-acts *clevr-french-stage-1-train-processor* 500))

;; Run a number of speech acts (stage 1 = 11991 speech acts)
(comprehend *clevr-french-stage-1-train-processor* :cxn-inventory *clevr-french-stage-1-grammar* :nr-of-speech-acts 100)

;; Optionally reset grammar and/or train processor
;;(setf *clevr-french-stage-1-grammar* (make-clevr-french-cxn-inventory-cxns))
;;(reset-cp *clevr-french-stage-1-train-processor*)



;;#############################################
;; Testing learnt grammar on new utterances
;;#############################################

(defparameter *clevr-french-stage-1-test*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-like\ datasets" "clevr-french-learning-ipa" "val")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defparameter *clevr-french-stage-1-test-processor* (load-corpus *clevr-french-stage-1-test* :sort-p nil :remove-duplicates t :ipa t))


;; Run a number of speech acts (set learn to NIL to disable meta-layer)
(comprehend *clevr-french-stage-1-test-processor* :cxn-inventory *clevr-french-stage-1-grammar* :nr-of-speech-acts 2
            :learn nil :consolidate nil :align nil)