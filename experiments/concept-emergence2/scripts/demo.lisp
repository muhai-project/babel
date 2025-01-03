(ql:quickload :cle)

(in-package :cle)

;; experiments with entrenchment values - keep the same
(progn
  (defparameter *baseline-simulated*
    (make-configuration
     :entries `(
                ;; monitoring
                (:dot-interval . 10)
                (:usage-table-window . 100)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 2)
                ;; setup data scene
                (:dataset-loader . :runtime)
                (:dataset-view . :exclusive-views) ;; vs. :shared
                (:dataset "mscoco-facebook@dinov2-small" "mscoco-google@vit-base-patch16-224-in21k")
                (:min-context-size . 10)
                (:max-context-size . 10)
                (:dataset-split . "test")
                ;(:data-fname . "all.lisp")
                (:feature-set "mscoco-facebook@dinov2-small" "mscoco-google@vit-base-patch16-224-in21k")
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
                (:trash-concepts . t)
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001) ;; only for gaussian-welford
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -5) ;; TODO TO ADAPT?
                ;; staging
                (:switch-condition . :none) ; :after-n-interactions)
                (:switch-conditions-after-n-interactions . 2500) 
                (:stage-parameters nil)
                ;; saving
                (:exp-top-dir . "a")
                (:exp-name . "b")
                (:log-dir-name . "c")
                (:prototype-distance . :paper)
                )))
  (setf *experiment* (make-instance 'cle-experiment :configuration *baseline-simulated*))
  (notify reset-monitors)
  (wi::reset))

(progn
  (wi::reset)
  (notify reset-monitors)
  (deactivate-all-monitors)
  (activate-monitor export-communicative-success)
  (activate-monitor export-lexicon-coherence)
  ;; (activate-monitor export-unique-form-usage)
  (activate-monitor print-a-dot-for-each-interaction)
  (format t "~%---------- NEW GAME ----------~%")
  (time
   (loop for i from 1 to 100000
         do (run-interaction *experiment*))))

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (loop for idx from 1 to 1
        do (run-interaction *experiment*)))

;; display lexicon
(display-lexicon (first (agents *experiment*)) :sort t :weight-threshold 0.8)

(progn
  (format t "~% Lexicon overview")
  (loop for agent in (agents *experiment*)
        for fast-inventory = (fast-inventory (lexicon (first (agents *experiment*))))
        for trash-inventory = (trash-inventory (lexicon (first (agents *experiment*))))
        do (format t "~% ~a: ~a, ~a"
                   (id agent)
                   (hash-table-count fast-inventory)
                   (hash-table-count trash-inventory))))

;; restore an experiment and initialise the world
(progn
  (setf *experiment*
        (cl-store:restore (babel-pathname :directory '("experiments"
                                                        "concept-emergence2"
                                                        "logging"
                                                        "top-level-dir"
                                                        "exp-name"
                                                        "log-dir-name"
                                                        )
                                          :name "1-history"
                                          :type "store")))
  ;; (set-configuration *experiment* :dataset-split "val")
  ;; set-configuration *experiment* :align nil)
  (initialise-world *experiment*))