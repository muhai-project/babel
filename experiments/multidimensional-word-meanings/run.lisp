
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)

;; Creating the experiment might take a few seconds
;; since large amounts of data need to be loaded from file
(defparameter *configuration*
  (make-configuration
   :entries '((:game-mode . :tutor-tutor)
              (:determine-interacting-agents-mode . :default)
              (:tutor-lexicon . :continuous)
              (:category-representation . :exponential)
              (:alignment-strategy . :discrimination-based)
              (:lexical-variation . nil)
              (:feature-selection . :all)
              (:noise-amount . nil)
              (:noise-prob . nil)
              (:scale-world . t))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 200)

(run-series *experiment* 2000)

(show-learner-lexicon (find 'learner (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (tttest-min-max
                    ((:game-mode . :tutor-tutor)
                     (:determine-interacting-agents-mode . :default)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :min-max)
                     (:alignment-strategy . :similarity-based)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t)))
                   (tttest-prototype
                    ((:game-mode . :tutor-tutor)
                     (:determine-interacting-agents-mode . :default)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :prototype)
                     (:alignment-strategy . :discrimination-based)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t)))
                   (tttest-pmm
                    ((:game-mode . :tutor-tutor)
                     (:determine-interacting-agents-mode . :default)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :prototype-min-max)
                     (:alignment-strategy . :discrimination-based)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t)))
                   (tttest-exponential
                    ((:game-mode . :tutor-tutor)
                     (:determine-interacting-agents-mode . :default)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :exponential)
                     (:alignment-strategy . :discrimination-based)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t)))
                   )
                 :number-of-interactions 1000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-x-pos-convergence-graph :nr-of-interactions 100)
(create-tutor-attribute-use-graph :nr-of-interactions 500)

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-for-single-strategy
 :experiment-name "baseline"
 :measure-names '("features-per-form"
                  "utterance-length")
 :y-axis '(1 1 1)
 :y1-max nil
 :xlabel "Number of games")

(create-graph-comparing-strategies
 :experiment-names '("exponential-discrimination-no-variation"
                     "exponential-discrimination-with-variation")
 :measure-name "communicative-success"
 :y-max 1 :xlabel "Number of games" :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("exponential-discrimination-no-variation"
                     "exponential-discrimination-with-variation")
 :measure-name "lexicon-size"
 :y-max nil :xlabel "Number of games" :y1-label "Success")
  
