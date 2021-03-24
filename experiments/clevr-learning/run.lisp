
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; run an experiment and immediately create the missing plots
(progn
  
  (run-experiments '(
                     (cxn-decf-02-15k-games
                      ((:question-sample-mode . :first)
                       (:questions-per-challenge . 5000)
                       (:scenes-per-question . 20)
                       (:confidence-threshold . 1.1) ;; force to stay in level 1
                       (:cxn-decf-score . 0.2)
                       (:tutor-mode . :smart)
                       (:tutor-counts-failure-as . 1)))
                     )
                   :number-of-interactions 15000
                   :number-of-series 2
                   :monitors (append '("print-a-dot-for-each-interaction")
                                     (get-all-lisp-monitors)
                                     (get-all-csv-monitors)
                                     (get-all-export-monitors)))
  
  (create-graph-for-single-strategy
   :experiment-name "cxn-decf-02-15k-games"
   :measure-names '("communicative-success" "lexicon-size")
   :y-axis '(1 2) :y1-max 1 :open nil)
  
  (create-graph-for-single-strategy
   :experiment-name "cxn-decf-02-15k-games"
   :measure-names '("lexical-meanings-per-form" "lexical-forms-per-meaning")
   :y-axis '(1) :y1-max nil :open nil)

)
                 

