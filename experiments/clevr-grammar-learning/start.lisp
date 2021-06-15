(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(defun summarize-cxn-types (cxn-inventory)
  (let ((holophrase-cxns (sort (find-all 'gl::holophrase (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (lexical-cxns (sort (find-all 'gl::lexical (constructions-list cxn-inventory)
                                      :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (item-based-cxns (sort (find-all 'gl::item-based (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score)))))
        
    (add-element `((h2) ,(format nil "Holophrases: ~a" (length holophrase-cxns))))
    (loop for cxn in holophrase-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Lexical cxns: ~a" (length lexical-cxns))))
    (loop for cxn in lexical-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Item-based cxns: ~a" (length item-based-cxns))))
    (loop for cxn in item-based-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element '((hr)))))


(defun failure-analysis (error-file cxn-inventory)
  (activate-monitor trace-fcg)
  (loop for (utterance meaning) in error-file
        do (comprehend utterance :gold-standard-meaning meaning :cxn-inventory cxn-inventory))
  (deactivate-monitor trace-fcg))
        

(defun run-training ()
  (wi::reset)
  (let ((experiment-name 'training-stage-1))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :train)
                         (:learner-th-connected-mode . :neighbours)
                         (:run-mode :training)
                         (:current-challenge-level . 1)
                         ))
                       )
                     :number-of-interactions 47133
                     :number-of-series 2
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "summarize-results-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors))))
  (create-graph-for-single-strategy
   :experiment-name "training-stage-1" :measure-names '("communicative-success" "lexicon-size")
   :y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-max nil
   :xlabel "# Observations"
   :y1-label "Communicative Success"
   :y2-label "# Constructions"
   :captions '("communicative success" "grammar size")
   :open t)
#|
  (create-graph-for-single-strategy
   :experiment-name "training-stage-1" :measure-names '("communicative-success" "avg-cxn-score")
   :y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-max nil
   :xlabel "# Observations"
   :y1-label "Communicative Success"
   :y2-label "Average Construction Score"
   :captions '("communicative success" "average construction score")
   :open t)
|#
  (create-graph-for-single-strategy
   :experiment-name "training-stage-1" :measure-names '("communicative-success" "th-size")
   :y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-max nil
   :xlabel "# Observations"
   :y1-label "Communicative Success"
   :y2-label "# Categorial Links"
   :captions '("communicative success" "number of edges in categorial network")
   :open t))

(defun run-training-stage-2 ()
  (wi::reset)
  (let ((experiment-name 'training-stage-2))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :train)
                         (:learner-th-connected-mode . :neighbours)
                         (:run-mode :training)
                         (:current-challenge-level . 2)
                         ))
                       )
                     :number-of-interactions 100;408656
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "summarize-results-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors)))))

(defun run-evaluation (stored-grammar)
  (wi::reset)
  (let ((experiment-name 'evaluation))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :evaluation)
                         (:evaluation-grammar . ,stored-grammar)
                         (:learner-th-connected-mode . :neighbours)
                         (:run-mode :evaluation)
                         ))
                       )
                     :number-of-interactions 10043
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "evaluation-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors)))))

(defun run-dev-set (stored-grammar)
  (wi::reset)
  (let ((experiment-name 'development))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :development)
                         (:evaluation-grammar . ,stored-grammar)
                         (:learner-th-connected-mode . :neighbours)
                         (:run-mode :test)
                         ))
                       )
                     :number-of-interactions 10181
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "evaluation-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors)))))

#|
(progn
  (activate-monitor trace-fcg)
  (formulate '((get-context ?source-1)
               (query ?target-51 ?target-object-1 ?attribute-15)
               (bind attribute-category ?attribute-15 material)
               (filter ?target-2 ?target-1 ?size-2)
               (unique ?target-object-1 ?target-2)
               (bind shape-category ?shape-2 cube)
               (filter ?target-1 ?source-1 ?shape-2)
               (bind size-category ?size-2 small))
             :gold-standard-utterance "What is the small cube made of?"
             :cxn-inventory *saved-inventory*))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; train, load the exported grammar and evaluate it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (run-training)
;(run-training-stage-2)
; (defparameter *saved-inventory* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data" "training") :name "cxn-inventory-training-latest" :type "store")))
; (run-evaluation *saved-inventory*)
; (run-dev-set *saved-inventory*)
; (defparameter *error-file* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data" "development") :name "errors-training-latest" :type "store")))
; (failure-analysis *error-file* *saved-inventory*)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualise all constructions per type in web interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (add-element (make-html *saved-inventory*))
; (summarize-cxn-types *saved-inventory*)
; (add-element (make-html (get-type-hierarchy *saved-inventory*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; configuraties
; - bidirectional (trainen in formulation en comprehension)
; - met en zonder lateral inhibition


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; - th-connected mode op path-exists (enkel bij evaluatie) levert slechtere resultaten!
; --> 99.26 ipv 99.29% accuracy