(in-package :ont-alignment)

(defclass ont-alignment-agent (agent)
  ((question-answer-pair
    :initarg :question-answer-pair
    :accessor qa-pair
    :initform nil
    :documentation "The chosen question-answer pair for the interaction")
   (personal-db
    :accessor personal-db
    :initform nil
    :documentation "The db which the agent is connected to")
   (personal-query-language
    :accessor query-lang
    :initform nil
    :documentation "The query language which is assigned to the agent to query its own database at the beggining of the experiment")
   (dictionary
    :accessor dictionary
    :initform nil
    :documentation "A set of the question-answer pairs that were already given with their sql query")))

(defclass agent-dictionary ()
  ((question
    :initarg :question
    :accessor question
    :type string
    :documentation "The question in natural language")
   (answer
    :initarg :answer
    :accessor answer
    :type string
    :documentation "The answer in natural language")
   (q
    :initarg :q
    :accessor q
    :documentation "The query in query language")
   ))

(defun create-dictionary-item ()
     "function to create a dictionary that stores qa-pairs and queries for an agent"
     (make-instance 'agent-dictionary
     ))

(defun add-to-dictionary (question answer learner)
  (setf item (create-dictionary-item))
  (setf (question item) question)
  (setf (answer item) answer)
  (setf (q item) nil)
)

(defmethod make-ont-agents ((nb-of-agents integer)(experiment experiment))
  "method to create a population of agents"
  (let ((agents (loop for i from 1 to nb-of-agents
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'ont-alignment-agent
                                             :id agent-id
                                             :query-lang "sql"))))
    (setf (agents experiment) agents)))

