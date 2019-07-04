
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
   :entries `((:data-source . :extracted)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:test-after-n-interactions . ,3)
              (:data-sets . ("valA"))
              (:data-path . ,(merge-pathnames
                              (make-pathname :directory '(:relative "CLEVR-CoGenT" "scenes" "valA-extracted"))
                              cl-user:*babel-corpora*)))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 6)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (exponential-no-scaling
                    ((:data-source . :clevr)
                     (:scale-world . nil)
                     (:category-representation . :exponential)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:test-after-n-interactions . nil)
                     (:data-sets . ("val"))))
                   )
                 :number-of-interactions 50000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "exponential-no-scaling"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("cogent-extracted-prototype-train-100"
                     "cogent-extracted-prototype-train-500"
                     "cogent-extracted-prototype-train-1000"
                     "cogent-extracted-prototype-train-2500"
                     "cogent-extracted-prototype-train-5000")
 :measure-name "lexicon-size"
 :y-min 0 :y-max nil :xlabel "Number of games" :y1-label "Lexicon Size"
 :captions '("train=100" "500" "1000" "2500" "5000")
 :title "CLEVR CoGenT (prototype)" :end 10000)
