(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(deactivate-all-monitors)

;; --------------------------------
;; + Batch experiments on CLUSTER +
;; --------------------------------


(time (run-parallel-batch-for-grid-search
       :asdf-system "cle"
       :package "cle"
       :experiment-class "cle-experiment"
       :number-of-interactions 10000
       :number-of-series 1
       :monitors (list "export-communicative-success"
                       "export-lexicon-coherence"
                       "export-experiment-configurations"
                       "export-experiment-store"
                       "export-record-time"
                       "export-unique-form-usage"
                       "print-a-dot-for-each-interaction"
                       )
       ;; default configuration settings
       :shared-configuration `(
                ;; monitoring
                               (:dot-interval . 100)
                ;(:record-every-x-interactions . 100) ;; important for fast logging
                               (:usage-table-window . 1000)
                               (:save-distribution-history . nil)
                               ;; setup interacting agents
                               (:interacting-agents-strategy . :standard)
                               (:population-size . 10)
                               ;; setup data scene
                               (:dataset . "gqaglove50")
                               (:dataset-split . "train")
                ;(:data-fname . "all.lisp")
                               (:available-channels ,@(get-all-channels :gqaglove50))
                               ;; disable channels
                               (:disable-channels . :none)
                               (:amount-disabled-channels . 0)
                               ;; noised channels
                               (:sensor-noise . :none)
                               (:sensor-std . 0.0)
                               (:observation-noise . :none)
                               (:observation-std . 0.0)
                               ;; scene sampling
                               (:scene-sampling . :random)
                               (:topic-sampling . :random)
                               ;; general strategy
                               (:align . t)
                               (:similarity-threshold . 0.0)
                               ;; entrenchment of constructions
                               (:initial-cxn-entrenchement . 0.5)
                               (:entrenchment-incf . 0.1)
                               (:entrenchment-decf . -0.1)
                               (:entrenchment-li . -0.02) ;; lateral inhibition
                               (:trash-threshold . 0.0)
                               (:slow-threshold . -0.1)
                               (:conceptualisation-heuristics . :heuristic-1)
                               (:speaker-competitors . nil)
                               (:hearer-competitors . nil)
                               ;; concept representations
                               (:concept-representation . :distribution)
                               (:distribution . :gaussian-welford)
                               (:M2 . 0.0001) ;; only for gaussian-welford
                               ;; prototype weight inits
                               (:weight-update-strategy . :j-interpolation)
                               (:initial-weight . 0)
                               (:weight-incf . 1)
                               (:weight-decf . -5)
                               ;; staging
                               (:switch-condition . :none) ; :after-n-interactions)
                               (:switch-conditions-after-n-interactions . 2500)
                               (:stage-parameters nil)
                               )
       ;; configurations
       :configurations `(;(:similarity-threshold 0.0 0.01 0.05 0.1 0.2)
                         ;(:initial-weight 0 35)
                         )
       ;; output directory
       :output-dir (babel-pathname :directory '("experiments" "concept-emergence2" "logging" "all"))
       :heap-size 12248))

#|
(calculate-amount-of-variations `(;(:similarity-threshold 0.0 0.01 0.05 0.1 0.2)
                                  (:initial-weight 0 35)
                                  ))
|#
