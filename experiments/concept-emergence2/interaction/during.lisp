(in-package :cle)

;; ----------------------
;; + Interaction script +
;; ----------------------

(defgeneric do-interaction 
    (experiment)
    (:documentation "Run the interaction script"))

(defmethod do-interaction ((experiment cle-experiment))
  "The speaker conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (when (and (not (conceptualise speaker)) (align-p experiment))
      ;; when speaker could not conceptualise
      ;; invent and conceptualise again
      ;; invention always leads to conceptualisation
      (invent speaker))

    ;; failure to conceptualise can only occur if speaker cannot invent
    (when (conceptualised-p speaker)
      ;; speaker utters the form
      (production speaker)
      ;; hearer hears the utterance
      (setf (utterance hearer) (utterance speaker))
      ;; success if
      (when (and ;; 1. the hearer recognises it,
                 (parsing hearer)
                 ;; 2. can interpret it, and
                 (interpret hearer)
                 ;; 3. it matches the topic
                 (if (has-topic-id (get-data speaker 'topic))
                   (equalp (get-topic-id (get-data speaker 'topic))
                           (get-topic-id (get-data hearer 'interpreted-topic)))
                   (equalp (id (get-data speaker 'topic))
                           (id (get-data hearer 'interpreted-topic)))))
        (setf (communicated-successfully speaker) t)
        (setf (communicated-successfully hearer) t)))
      
    ;; check lexicon coherence (before alignment)
    (set-data (current-interaction experiment) 'lexicon-coherence (lexicon-coherence-p experiment speaker hearer))))
