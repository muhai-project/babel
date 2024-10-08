(in-package :grammar-learning)
;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------


;; Finding the data
(define-configuration-default-value :meaning-representation :irl)

(define-configuration-default-value :corpus-files-root
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :corpus-data-file
                                    (make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))


(define-configuration-default-value :observation-sample-mode :train) ; train, debug, development or evaluation
(define-configuration-default-value :number-of-epochs 1) ; how many times the training data is concatenated in random variations
(define-configuration-default-value :de-render-mode :de-render-string-meets)

;; Learning Operators
(define-configuration-default-value :repairs 
                                    '(add-categorial-links
                                     item-based->item-based--substitution
                                     item-based->holistic
                                     holistic->item-based--substitution
                                     holistic->item-based--addition
                                     holistic->item-based--deletion
                                     holistic->item-based
                                     nothing->holistic))


;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)

(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0)

(define-configuration-default-value :evaluation-grammar nil)
(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :remove-cxn-on-lower-bound t)
(define-configuration-default-value :mark-holophrases t)
(define-configuration-default-value :categorial-network-export-interval 1000)
(define-configuration-default-value :max-nodes 1000)
(define-configuration-default-value :initial-categorial-link-weight 0.0)
(define-configuration-default-value :comprehend-n 2)

(define-configuration-default-value :determine-interacting-agents-mode :corpus-learner)
(define-configuration-default-value :learner-cxn-supplier :hashed-and-scored-routine-cxn-set-only)
(define-configuration-default-value :category-linking-mode :neighbours)
(define-configuration-default-value :learning-strategy :optimal-form-coverage) ;or: by-score


;; Misc
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :result-display-interval 100)

;; --------------
;; + Experiment +
;; --------------

(defclass grammar-learning-experiment (experiment)
  ((question-data :initarg :question-data :initform nil 
                   :accessor question-data :type list
                   :documentation "A list of samples for the current challenge level")
   (failed-question-data :initarg :failed-question-data :initform nil 
                   :accessor failed-question-data :type list
                   :documentation "A list of unsuccessful observations")
   (confidence-buffer :initarg :confidence-buffer :initform nil
                      :accessor confidence-buffer :type list
                      :documentation "A buffer to keep track of outcomes of games")
   (success-buffer :initarg :success-buffer :initform nil
                      :accessor success-buffer :type list
                      :documentation "A buffer to keep track of communicative success")
   (repair-buffer :initarg :repair-buffer :initform nil
                      :accessor repair-buffer :type list
                      :documentation "A buffer to keep track of all used repairs")
   )
  (:documentation "A grammar learning experiment"))

(defmethod initialize-instance :after ((experiment grammar-learning-experiment) &key)
  ;; set the utterances/gold standard meanings of the experiment
  (load-utterances experiment (get-configuration experiment :observation-sample-mode))
  
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-clevr-learning-learner experiment)))
  ;; set configurations for evaluation
  (when (get-configuration experiment :evaluation-grammar)
    (setf (grammar (first (agents experiment))) (get-configuration experiment :evaluation-grammar)))
  (when (equal (get-configuration experiment :observation-sample-mode) :evaluation)
    (set-configuration (grammar (first (agents experiment))) :update-th-links nil)
    ;(set-configuration (grammar (first (agents experiment))) :use-meta-layer nil)
    (set-configuration (grammar (first (agents experiment))) :consolidate-repairs nil)))


(define-event corpus-utterances-loaded)

(defgeneric pre-process-meaning-data (meaning mode))

(defmethod pre-process-meaning-data (meaning (mode (eql :geo)))
  ;(format t "~a" meaning)
  (read-from-string meaning))

(defun fresh-variables (predicates)
  (let* ((all-variables (find-all-anywhere-if #'variable-p predicates))
         (unique-variables (remove-duplicates all-variables))
         (renamings (loop for var in unique-variables
                          for base-name = (get-base-name var)
                          collect (cons var (make-var base-name)))))
    (values (substitute-bindings renamings predicates)
            renamings))) 

(defmethod pre-process-meaning-data (meaning (mode (eql :cooking)))
  (fresh-variables (read-from-string meaning)))                               

(defmethod pre-process-meaning-data (meaning (mode (eql :irl)))
  (fresh-variables (read-from-string meaning)))

(defmethod pre-process-meaning-data (meaning (mode (eql :amr)))
  (let ((*package* (find-package "GL-DATA"))
        (parsed-meaning (read-from-string meaning)))
    
    (if (listp (first parsed-meaning))
      parsed-meaning
      (amr:penman->predicates parsed-meaning))))




(defun load-question-data (experiment challenge-file num-epochs &key sort-p shuffle-data-p)
  (unless (find-package "GL-DATA")
      (make-package "GL-DATA"))
  (with-open-file (stream challenge-file)
    (let* ((stage-data (loop for line = (read-line stream nil)
                             for data = (when line (cl-json:decode-json-from-string line))
                             while data
                             collect (cons (cdr (assoc :utterance data)) (pre-process-meaning-data (cdr (assoc :meaning data)) (get-configuration experiment :meaning-representation) ))))
           (ordered-stage-data (if shuffle-data-p
                                 (shuffle stage-data)
                                 (if sort-p
                                   (sort stage-data #'< :key #'(lambda (x) (count #\space (first x))))
                                   stage-data))))
      (setf (question-data experiment)
            (loop repeat num-epochs
                  append ordered-stage-data))))
    (notify corpus-utterances-loaded))


(defgeneric load-utterances (experiment mode)
  (:documentation "Load all data for the current challenge level"))

(defmethod load-utterances ((experiment grammar-learning-experiment)
                                                       (mode (eql :train)))
    (let* ((challenge-file (merge-pathnames
                          (get-configuration experiment :corpus-data-file)
                          (get-configuration experiment :corpus-files-root))))
    (load-question-data experiment challenge-file (get-configuration experiment :number-of-epochs) :shuffle-data-p t)))

(defmethod load-utterances ((experiment grammar-learning-experiment)
                                                       (mode (eql :sort-length-ascending)))
    (let* ((challenge-file (merge-pathnames
                          (get-configuration experiment :corpus-data-file)
                          (get-configuration experiment :corpus-files-root))))
    
    (load-question-data experiment challenge-file (get-configuration experiment :number-of-epochs) :sort-p t :shuffle-data-p nil)))

(defmethod load-utterances ((experiment grammar-learning-experiment)
                                                       (mode (eql :debug)))
  (let* ((challenge-file (merge-pathnames
                          (get-configuration experiment :corpus-data-file)
                          (get-configuration experiment :corpus-files-root))))
    
    (load-question-data experiment challenge-file (get-configuration experiment :number-of-epochs) :shuffle-data-p nil)))

(defmethod load-utterances ((experiment grammar-learning-experiment)
                                                       (mode (eql :evaluation)))
  (let* ((challenge-file (merge-pathnames
                          (get-configuration experiment :corpus-data-file)
                          (get-configuration experiment :corpus-files-root))))
    (load-question-data experiment challenge-file 1 :shuffle-data-p nil)))

(defmethod load-utterances ((experiment grammar-learning-experiment)
                                                       (mode (eql :development)))
  (let* ((challenge-file (merge-pathnames
                          (get-configuration experiment :corpus-data-file)
                          (get-configuration experiment :corpus-files-root))))
    (load-question-data experiment challenge-file (get-configuration experiment :number-of-epochs) :shuffle-data-p nil)))


(defmethod tutor ((experiment grammar-learning-experiment))
  (find 'tutor (population experiment) :key #'role))

(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'role))

(defmethod learner ((experiment grammar-learning-experiment))
  (find 'learner (population experiment) :key #'role))

(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'role))


;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------

(defmethod determine-interacting-agents ((experiment grammar-learning-experiment)
                                         interaction (mode (eql :corpus-learner)) &key)
  ;; Tutor is speaker, learner is hearer
  (setf (interacting-agents interaction) (list (learner experiment))
        (discourse-role (learner experiment)) 'hearer)
  (setf (utterance (learner experiment)) nil
        (communicated-successfully (learner experiment)) nil)
  ;(notify interacting-agents-determined experiment interaction)
  )