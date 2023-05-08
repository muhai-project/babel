(in-package :duckie-language-learning)

;; ---------------
;; + Interaction +
;; ---------------

(defun execute (irl-program agent)
  "Evaluates the IRL program and returns the answer."
  (let* ((solutions (evaluate-irl-program irl-program (ontology agent)
                                          :primitive-inventory (primitive-inventory agent)))) 
    (if (equal (length solutions) 1)
      (let* ((solution (first solutions))
             (target-var (get-target-var irl-program))
             (answer (value (find target-var solution :key #'var))))
        answer))))

(defun confirm-answer (answer)
  (capi:prompt-for-confirmation (format nil "I think the answer is: ~a" (id answer))))

(defmethod interact ((experiment duckie-language-learning-experiment)
                     interaction &key)
  "Runs an interaction in which the user is prompted to ask a question."
  (let ((answer-correct? nil)
        (correct-answer nil)
        (fcg-solution? nil)
        (cipn nil)
        (agent (hearer interaction))
        (meaning nil))
    (set-data (blackboard (grammar agent)) :ontology (ontology agent))
    (set-data (blackboard (grammar agent)) :primitive-inventory (primitive-inventory agent))
    (setf (utterance agent) (downcase (capi:prompt-for-string "Enter your question:")))
    (multiple-value-bind (resulting-meaning resulting-cipn)
        (comprehend (utterance (hearer interaction))
                    :cxn-inventory (grammar (hearer interaction)))
      (setf cipn resulting-cipn)
      (setf meaning resulting-meaning)
      ;; comprehension werkt zonder repairs
      ;; dus vragen aan de user of het juist is
      (when (and (find 'fcg::succeeded (statuses cipn))
                 (not (some-applied-repair-in-tree cipn)))
        (let* ((answer (execute meaning (hearer interaction))))
          (setf fcg-solution? t)
          (when answer
            (setf answer-correct? (confirm-answer answer)))
          (setf correct-answer (if answer-correct? (id answer)
                                 (ask-correct-answer2 agent)))
          (set-data (blackboard (grammar agent)) :ground-truth-topic correct-answer))
        (cond (answer-correct?
               ;; joepie, rewarden!
               (run-alignment agent answer-correct? cipn))
              (fcg-solution?
               ;; spijtig, punishen!
               (run-alignment agent answer-correct? cipn)
               ;; en nieuwe holophrase leren
               (learn-through-intention-reading-and-pattern-finding agent correct-answer cipn)))))))

(defun learn-through-intention-reading-and-pattern-finding (agent correct-answer cipn)
  "Learns a new holophrase from the given CIPN and the correct answer."
  (let* ((intention (compose-program agent correct-answer))
         (holophrase-cxn (create-holophrase-cxn cipn intention)))
    (add-cxn holophrase-cxn (grammar agent))))
            
(defun run-alignment (agent answer-correct? cipn)
  "Aligns the applied constructions based on the correctness of the answer."
  (let ((applied-cxns (mapcar #'get-original-cxn
                              (applied-constructions cipn))))
    (loop for cxn in applied-cxns
          if answer-correct?
            do (progn (incf (attr-val cxn :score) 0.1)
                 (when (> (attr-val cxn :score) 1.0)
                   (setf (attr-val cxn :score) 1.0)))
          else
            do (progn (decf (attr-val cxn :score) 0.1)
                 (when (< (attr-val cxn :score) 0.0)
                   (delete-cxn cxn (grammar agent)))))))

(defun process-text-from-capi (agent text)
  ;; text from capi needs to be symbol to find in irl ontology, integers also need to be symbols
  (let* ((correct-answer (intern (upcase text)))
         (answer (loop for fields in (data-fields (ontology agent))
                       if (listp (cdr fields))
                         when (find correct-answer (cdr fields)
                                    :key #'id :test #'equal)
                           return it)))
    (if (string= (first (split-sequence:split-sequence  #\Space text)) "car-in")
      (let ((new-car (copy-object (get-data *ontology* 'agent-car))))
        (setf (zone new-car) (intern (upcase (second (split-sequence:split-sequence  #\Space text)))))
        new-car)
      answer)))

(defun ask-correct-answer2 (agent)
  (let* ((answer-string (downcase (capi:prompt-for-string "Tell me the correct answer please?")))
         (answer (process-text-from-capi agent answer-string)))
    answer))

 ;; returns either the concept from the ontology or when the answer was "car-in zone-1" a car object with position zone-1.
(defun ask-correct-answer (agent)
  (let* ((answer-string (downcase (capi:prompt-for-string "I'm afraid I didn't understand your question, what would the answer be?")))
         (answer (process-text-from-capi agent answer-string)))
    answer))

(defparameter *answer-categories*
  (list 'numbers 'zones 'building-functions 'bools 'colors))

(defun possible-answers ()
  (loop for field in (data-fields *ontology*)
        if (find (first field) *answer-categories*)
          append (loop for el in (rest field)
                       collect (id el))))

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree can be added by a repair
  ;; luckily, the handle-fix methods write this
  ;; on the blackboard of the initial node, so
  ;; we don't have to traverse the entire tree
  (multiple-value-bind (some-repair-applied-p foundp)
      (find-data (initial-node node) :some-repair-applied)
    (when foundp some-repair-applied-p)))
