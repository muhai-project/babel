(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; AMR  grammar learning               ;;
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

(def-fcg-constructions amr-cxn-inventory
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
                       (:meaning-representation-format . :amr)
                       
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


;;#############################################
;; Learning a grammar for AMR 3.0
;;#############################################

(defparameter *amr-3.0*
  (merge-pathnames (make-pathname :directory '(:relative "AMR-corpora" "amr_annotation_3.0" "pre-processed")
                                  :name "amr3" 
                                  :type "json")
                   cl-user:*babel-corpora*))

;; Takes 10-20 seconds to load the corpus (sort based on size of meaning network)
(defparameter *amr-speech-act-processor* (load-corpus *amr-3.0* :sort-p t :remove-duplicates t :amr t :remove-punctuation t))

;; (length (corpus *amr-speech-act-processor*))
;; 57147

(defun filter-speech-acts (original-corpus-processor &key (predicates-to-filter '(url-entity amr-empty)))
  (loop with new-processor = (make-instance 'corpus-processor
                                       :corpus nil 
                                       :source-file (source-file original-corpus-processor))
        with filtered-speech-acts = nil
        for speech-act across (corpus original-corpus-processor)
        for meaning = (meaning speech-act)
        unless (and (= (length meaning) 1)
                    (find (caar meaning) predicates-to-filter :test #'equalp))
          do (setf filtered-speech-acts (append filtered-speech-acts (list speech-act)))
        finally (setf (corpus new-processor) (make-array (length filtered-speech-acts)
                                                            :initial-contents filtered-speech-acts))
                (return new-processor)))
                

(setf *amr-speech-act-processor* (filter-speech-acts *amr-speech-act-processor*))
;; Initialise an empty grammar
(defparameter *amr-corpus-grammar* (make-amr-cxn-inventory-cxns))

;; Run a number of speech acts (stage 1 = 11991 speech acts)
(comprehend *amr-speech-act-processor* :cxn-inventory *amr-corpus-grammar*  :nr-of-speech-acts 50)

;; Optionally reset grammar and/or train processor
;;(setf *amr-grammar* (make-amr-cxn-inventory-cxns))
;;(reset-cp *amr-speech-act-processor*)

;; Rewind speech act processor by 1 speech act:
(setf (counter *amr-speech-act-processor*) (- (counter *amr-speech-act-processor*) 1))


;;#############################################
;; Learning a grammar for AMR Little Prince
;;#############################################

(defparameter *amr-little-prince*
  (merge-pathnames (make-pathname :directory '(:relative "AMR-corpora" "little-prince-amr" "pre-processed")
                                  :name "little-prince-amr" 
                                  :type "json")
                   cl-user:*babel-corpora*))

;; Takes 10-20 seconds to load the corpus
(defparameter *amr-little-prince-speech-act-processor* (load-corpus *amr-little-prince* :sort-p t :remove-duplicates nil :amr t))

;; Initialise an empty grammar
(defparameter *amr-little-prince-grammar* (make-amr-cxn-inventory-cxns))

;; Run a number of speech acts (stage 1 = 11991 speech acts)
(comprehend *amr-little-prince-speech-act-processor* :cxn-inventory *amr-little-prince-grammar*  :nr-of-speech-acts 2)

;; Optionally reset grammar and/or train processor
;;(setf *amr-little-prince-grammar* (make-amr-cxn-inventory-cxns))
;;(reset-cp *amr-little-prince-speech-act-processor*)