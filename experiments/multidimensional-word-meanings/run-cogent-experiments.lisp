(ql:quickload :mwm)
(in-package :mwm)

(progn
(run-parallel-batch-for-different-configurations 
 :asdf-system "mwm"
 :package "mwm"
 :experiment-class "mwm-experiment"
 :number-of-interactions 5000
 :number-of-series 10
 :monitors (list "export-communicative-success"
                 "export-lexicon-size"
                 "export-communicative-success-given-conceptualisation"
                 "export-learner-concepts-to-pdf"
                 "export-learner-concepts-to-store"
                 ;"export-experiment-configurations"
                 )
 ;; default configuration settings
 :shared-configuration '((:initial-certainty . 0.5)
                         (:certainty-incf . 0.1)
                         (:certainty-decf . -0.1)
                         (:remove-on-lower-bound . nil)
                         (:lexical-variation . nil))
 ;; configurations
 :configurations '(
                   (cogent-simulated-switch-1000
                    ((:experiment-type . :cogent)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:alignment-filter . :all)
                     (:switch-conditions-after-n-interactions . 1000)))
                   (cogent-simulated-bidirectional-switch-1000
                    ((:experiment-type . :cogent)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default)
                     (:alignment-filter . :all)
                     (:switch-conditions-after-n-interactions . 1000)))
                   (cogent-extracted-switch-1000
                    ((:experiment-type . :cogent)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:alignment-filter . :all)
                     (:switch-conditions-after-n-interactions . 1000)))
                   (cogent-extracted-bidirectional-switch-1000
                    ((:experiment-type . :cogent)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :default)
                     (:alignment-filter . :all)
                     (:switch-conditions-after-n-interactions . 1000)))
                   )
 ;; output directory
 :output-dir (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "raw-data")))

(create-graph-for-single-strategy
 "cogent-simulated-switch-1000"
 '("communicative-success" "lexicon-size")
 :plot-file-name "cogent-simulated-switch-1000"
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-for-single-strategy
 "cogent-simulated-bidirectional-switch-1000"
 '("communicative-success"
   "communicative-success-given-conceptualisation"
   "lexicon-size")
 :plot-file-name "cogent-simulated-bidirectional-switch-1000"
 :average-windows '(100 100 1)
 :use-y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-mixing-strategies
 '(("cogent-simulated-switch-1000" . "communicative-success")
   ("cogent-simulated-bidirectional-switch-1000" . "communicative-success")
   ("cogent-simulated-bidirectional-switch-1000" . "communicative-success-given-conceptualisation")
   ("cogent-simulated-bidirectional-switch-1000" . "lexicon-size"))
 :plot-file-name "cogent-simulated-comparison-1000"
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :average-windows '(100 100 100 1)
 :use-y-axis '(1 1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-for-single-strategy
 "cogent-extracted-switch-1000"
 '("communicative-success"
   "lexicon-size")
 :plot-file-name "cogent-extracted-switch-1000"
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-for-single-strategy
 "cogent-extracted-bidirectional-switch-1000"
 '("communicative-success"
   "communicative-success-given-conceptualisation"
   "lexicon-size")
 :plot-file-name "cogent-extracted-bidirectional-switch-1000"
 :average-windows '(100 100 1)
 :use-y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-mixing-strategies
 '(("cogent-extracted-switch-1000" . "communicative-success")
   ("cogent-extracted-bidirectional-switch-1000" . "communicative-success")
   ("cogent-extracted-bidirectional-switch-1000" . "communicative-success-given-conceptualisation")
   ("cogent-extracted-bidirectional-switch-1000" . "lexicon-size"))
 :plot-file-name "cogent-extracted-comparison-1000"
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :average-windows '(100 100 100 1)
 :use-y-axis '(1 1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

)
