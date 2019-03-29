
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
   :entries '((:shift-prototype . :always)
              (:update-certainty . t)
              (:category-representation . :prototype-min-max)
              (:feature-selection . :all)
              (:noise . nil))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 10) 

(run-series *experiment* 2000)

(show-learner-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (feature-sampling
                    ((:shift-prototype . :always)
                     (:category-representation . :prototype-min-max)
                     (:update-certainty . t)
                     (:feature-selection . :all)
                     (:noise . nil)))
                   )
                 :number-of-interactions 10000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-features-per-form"
                                 "export-utterance-length"))

(create-x-pos-convergence-graph :nr-of-interactions 100)
(create-tutor-attribute-use-graph :nr-of-interactions 500)

(create-graph-for-single-strategy
 :experiment-name "feature-sampling"
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
 :experiment-names '("min-max" "min-max-noise-0.1" "min-max-noise-0.2" "min-max-noise-0.3")
 :measure-name "communicative-success"
 :y-max 1 :xlabel "Number of games" :y1-label "Success")
  
