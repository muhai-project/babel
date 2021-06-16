;;;; agent.lisp

(in-package :clevr-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass clevr-learning-agent (agent object-w-tasks)
  ((topic :initarg :topic :initform nil
          :accessor topic :type (or null entity number)
          :documentation "The answer for the current question")
   (role :initarg :role :initform 'no-role :type symbol :accessor role
         :documentation "The role of the agent (tutor or learner)")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform nil
             :type (or null blackboard)
             :documentation "The agent's ontology")
   (available-primitives :initarg :available-primitives
                         :accessor available-primitives
                         :initform nil :type (or null primitive-inventory)
                         :documentation "The primitives available for the agent"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  ((question-success-table
    :initarg :success-table :initform nil :type list
    :accessor question-success-table
    :documentation "Agent keeps track of which questions
                               were seen, and how often it was
                               successful or not")
   (current-question-index :initform -1 :accessor current-question-index
                           :type number :documentation "Index of current question"))
   (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((task-result :initarg :task-result
                :accessor task-result
                :initform nil
                :documentation "Pointer to the result of the task in this interaction")
   (memory :initarg :memory :accessor memory :initform (make-hash-table :test #'eq)
           :documentation "The agent's memory (used by composer strategy)")
   (composer-chunks :initarg :composer-chunks :accessor composer-chunks
                    :initform nil :type list
                    :documentation "The chunks the agent can use for composing"))
  (:documentation "The learner agent"))

(defmethod tutorp ((agent clevr-learning-agent))
  (eql (role agent) 'tutor))

(defmethod learnerp ((agent clevr-learning-agent))
  (eql (role agent) 'learner))

(defun make-clevr-learning-tutor (experiment)
  (make-instance 'clevr-learning-tutor
                 :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :ontology (copy-object *clevr-ontology*)
                 :success-table (loop for i below (length (question-data experiment))
                                      collect (cons i nil))
                 :available-primitives (copy-object *clevr-primitives*)))

(defun make-clevr-learning-learner (experiment)
  (let ((learner
         (make-instance 'clevr-learning-learner
                        :role 'learner :experiment experiment
                        :grammar (empty-cxn-set (get-configuration experiment :hide-type-hierarchy)
                                                (get-configuration experiment :learner-cxn-supplier))
                        :ontology (copy-object *clevr-ontology*))))
    (set-primitives-for-current-challenge-level
     learner (get-configuration experiment :primitives))
    (update-composer-chunks-w-primitive-inventory learner)
    ;; when running the experiment in :hybrid mode
    ;; store the server address and the cookie jar in the ontology
    (when (eql (get-configuration experiment :primitives) :hybrid)
      (let ((server-address
             (format nil "~a:~a/"
                     (get-configuration experiment :hybrid-server-address)
                     (get-configuration experiment :hybrid-server-port)))
            (cookie-jar
             (make-instance 'drakma:cookie-jar)))
        (set-data (ontology learner) 'hybrid-primitives::server-address server-address)
        (set-data (ontology learner) 'hybrid-primitives::cookie-jar cookie-jar)))
    learner))

(defmethod clear-question-success-table ((agent clevr-learning-tutor))
  (setf (question-success-table agent)
        (loop for i below (length (question-data (experiment agent)))
              collect (cons i nil))))

(defmethod clear-memory ((agent clevr-learning-learner))
  (setf (memory agent)
        (make-hash-table :test #'eq)))

