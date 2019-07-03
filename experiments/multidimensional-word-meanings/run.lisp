
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)


;; ----------
;; + CoGenT +
;; ----------

(setf *clevr-data-path*
      (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT"))
                       cl-user:*babel-corpora*))

(reset-clevr-data-path)

;; --------------------
;; + Run interactions +
;; --------------------

(defparameter *configuration*
  (make-configuration
   :entries '((:data-source . :clevr)
              (:scale-world . nil)
              (:category-representation . :exponential)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:test-after-n-interactions . nil)
              (:data-sets . ("val")))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 200)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (test
                    ((:data-source . :clevr)
                     (:scale-world . nil)
                     (:category-representation . :exponential)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:test-after-n-interactions . nil)
                     (:data-sets . ("val"))))
                   )
                 :number-of-interactions 5000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("baseline-min-max-not-scaled"
                     "baseline-prototype-not-scaled"
                     "baseline-pmm-not-scaled"
                     "baseline-exponential-not-scaled")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("min-max" "prototype" "prototype-min-max" "exponential")
 :title nil :end 10000)
