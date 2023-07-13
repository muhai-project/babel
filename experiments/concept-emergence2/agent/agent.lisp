(in-package :cle)

;; ---------
;; + Agent +
;; ---------

(defclass cle-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon."
    :type list :accessor lexicon :initform nil)
   (trash
    :documentation "The lexicon trash."
    :type list :accessor trash :initform nil)
   (disabled-channels
    :documentation "Disabled channels in agent."
    :type list :accessor disabled-channels :initarg :disabled-channels :initform nil)
   (invented-or-adopted
    :documentation "Whether the agent invented or adopted during the interaction."
    :type boolean :accessor invented-or-adopted :initform nil)))

(defmethod clear-agent ((agent cle-agent))
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (invented-or-adopted agent) nil
        (communicated-successfully agent) nil))

(defmethod find-in-lexicon ((agent cle-agent) (form string))
  "Finds constructions with the given form in the lexicon of the given agent."
  (let ((result (find form (lexicon agent) :key #'form :test #'string=)))
    (if result
      result
      (find form (trash agent) :key #'form :test #'string=))))
