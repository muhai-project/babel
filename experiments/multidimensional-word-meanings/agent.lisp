(in-package :mwm)

;; -------------
;; + MWM agent +
;; -------------
(defclass mwm-agent (agent)
  ((grammar
    :documentation "The agent's grammar"
    :type fcg-construction-inventory :accessor grammar
    :initform (make-agent-grammar))
   (context
    :documentation "The current context (continuous values)"
    :accessor context :initform nil)
   (symbolic-context
    :documentation "The symbolic clevr context"
    :accessor symbolic-context :initform nil)
   (topic
    :documentation "The current topic"
    :accessor topic :initform nil)
   (applied-cxns
    :documentation "The applied cxns"
    :type list :accessor applied-cxns :initform nil)
   (discriminative-set
    :documentation "The discriminative set for the topic"
    :type list :accessor discriminative-set :initform nil)
   (parsed-meaning
    :documentation "The meaning obtained after parsing"
    :type list :accessor parsed-meaning :initform nil))
  (:documentation "The agent class"))

;; ---------------------------
;; + Agent utility functions +
;; ---------------------------
(defmethod speakerp ((agent mwm-agent))
  (eql (discourse-role agent) 'speaker))

(defmethod hearerp ((agent mwm-agent))
  (eql (discourse-role agent) 'hearer))

(defmethod learner ((experiment mwm-experiment))
  (find 'learner (population experiment) :key #'id))
(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'id))

(defmethod learnerp ((agent mwm-agent))
  (eql (id agent) 'learner))

(defmethod tutor ((experiment mwm-experiment))
  (find 'tutor (population experiment) :key #'id))
(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'id))

(defmethod tutorp ((agent mwm-agent))
  (eql (id agent) 'tutor))

(defun make-tutor-agent (experiment)
  (let ((tutor (make-instance 'mwm-agent :id 'tutor
                              :experiment experiment)))
    (make-tutor-lexicon tutor)
    tutor))

(defun make-learner-agent (experiment)
  (make-instance 'mwm-agent :id 'learner
                 :experiment experiment))

(defun make-agent-grammar ()
  (let ((grammar-name (make-const "agent-grammar")))
    (eval
     `(def-fcg-constructions ,grammar-name
        :cxn-inventory ,grammar-name
        :feature-types ((args sequence)
                        (form set-of-predicates)
                        (meaning set-of-predicates)
                        (subunits set)
                        (footprints set))))))

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defun discriminate-topic (topic list-of-objects)
  "Returns the minimal amount of (attr . val) conses that
   discriminates object from the objects in list-of-objects.
   Make sure the object is not in list-of-objects, otherwise
   the functions will logically return nil."
  (loop for nr-of-attr-needed from 1 to (length topic)
        do (let ((attr-combinations (shuffle (combinations-of-length topic nr-of-attr-needed))))
            (loop for attr-combination in attr-combinations
                  when (discriminative-combination? attr-combination list-of-objects)
                  do (return-from discriminate-topic attr-combination)))))

(defun discriminative-combination? (list-of-attributes list-of-object-attributes)
  "Returns t if the attribute combination in list-of-attributes
   does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object-attributes in list-of-object-attributes
          while unique
          when (loop for attribute in list-of-attributes
                     always (eql (rest attribute)
                                 (rest (assoc (car attribute) object-attributes))))
          do (setf unique nil))
    unique))

(define-event conceptualisation-finished (agent mwm-agent))

;;; Tutor conceptualisation
(defgeneric conceptualise (agent)
  (:documentation "Conceptualise the topic"))

(defmethod conceptualise ((agent mwm-agent))
  (case (get-configuration agent :tutor-lexicon)
    (:symbolic (conceptualise-symbolic agent))
    (:continuous (conceptualise-continuous agent))))

; symbolic
(defmethod conceptualise-symbolic ((agent mwm-agent))
  "The tutor uses a symbolic representation of the context and
   computes the minimal discriminative set of attributes"
  (let* ((all-objects-as-alist
          (loop for object in (objects (context agent))
                collect (cons (id object) (object->alist object))))
         (topic-as-alist
          (cdr (find (id (topic agent)) all-objects-as-alist :key #'car)))
         (context-as-alist
          (mapcar #'cdr
                  (remove-if #'(lambda (id) (eql id (id (topic agent))))
                             all-objects-as-alist :key #'car)))
         (discriminative-set (mapcar #'cdr (discriminate-topic topic-as-alist context-as-alist))))
    (unless (and (get-configuration agent :max-tutor-utterance-length)
                 (length> discriminative-set (get-configuration agent :max-tutor-utterance-length)))
      (setf (discriminative-set agent) discriminative-set))
    (notify conceptualisation-finished agent)
    (discriminative-set agent)))

; continuous
(defun choose-best-word (agent meaning-so-far cxns-to-consider)
  "Choose the best word to add to the utterance. This function
   simply takes the word with the highest similarity."
  (extremum cxns-to-consider
            :key #'(lambda (cxn)
                     (let* ((cxn-meaning (attr-val cxn :meaning))
                            (meaning (fuzzy-union cxn-meaning meaning-so-far)))
                       (weighted-similarity (topic agent) meaning)))
            :test #'>))

(defun choose-discriminating-word (agent meaning-so-far cxns-to-consider)
  "Choose the most discriminating word to add to the utterance.
   The word has to have a similarity to the topic that is higher
   than the similarity to any other object. If this is
   the case for multiple words, the one with the biggest difference
   between best-word<->topic and best-word<->best-other-object
   is chosen."
  (loop with best-cxn = nil
        with best-similarity = 0
        with best-difference = 0
        for cxn in (shuffle cxns-to-consider)
        for cxn-meaning = (attr-val cxn :meaning)
        for meaning = (fuzzy-union cxn-meaning meaning-so-far)
        for topic-similarity = (weighted-similarity (topic agent) meaning)
        for best-other-similarity = (loop for object in (remove (topic agent) (objects (context agent)))
                                          maximizing (weighted-similarity object meaning))
        for difference = (- topic-similarity best-other-similarity)
        when (and (> topic-similarity best-other-similarity)
                  (> difference best-difference))
        do (setf best-cxn cxn
                 best-similarity topic-similarity
                 best-difference difference)
        finally
        (return best-cxn)))

(defmethod conceptualise-continuous ((agent mwm-agent))
  "Conceptualisation according to Pieter Wellens' adaptive strategy.
   Keep adding words to the utterance as long as they improve the
   similarity between the utterance and the topic. It is possible
   that the lexicon is not good enough to describe the topic and
   not a single discriminating word is found"
  (loop with utterance = nil ; list of cxns
        with utterance-meaning = nil ; combined meaning
        with best-similarity = 0
        with continue = t
        ; utterance has a max length
        while (and (length< utterance (get-configuration agent :max-tutor-utterance-length)) continue)
        for best-new-word
        = (choose-discriminating-word agent utterance-meaning
                                      (remove-if #'(lambda (cxn)
                                                     (member cxn utterance))
                                                 (constructions (grammar agent))))
        for new-similarity = (when best-new-word
                               (let* ((cxn-meaning (attr-val best-new-word :meaning))
                                      (extended-meaning (fuzzy-union cxn-meaning utterance-meaning)))
                                 (weighted-similarity (topic agent) extended-meaning)))
        if (and new-similarity (> new-similarity best-similarity))
        do (progn (push best-new-word utterance)
             (setf utterance-meaning (fuzzy-union (attr-val best-new-word :meaning) utterance-meaning))
             (setf best-similarity new-similarity))
        else
        do (setf continue nil)
        finally
        (progn (setf (applied-cxns agent) utterance)
          (notify conceptualisation-finished agent)
          (return utterance))))


;; ---------------
;; + Re-entrance +
;; ---------------

(defgeneric re-entrance (agent)
  (:documentation "Do re-entrance"))

(defmethod re-entrance ((agent mwm-agent))
  "The tutor checks whether interpreting his own utterance
   will lead him to the topic."
  (when (and (eql (get-configuration agent :tutor-lexicon) :continuous)
             (applied-cxns agent))
    ;; construct the utterance
    (let ((utterance (mapcar #'(lambda (cxn) (attr-val cxn :form))
                             (applied-cxns agent)))
          interpreted-object success)
      (setf (utterance agent) utterance)
      ;; disable monitors
      (with-disabled-monitors
        ;; parse and interpret
        (parse-word agent)
        (setf interpreted-object
              (interpret agent)))
      ;; compare equality
      (setf success (eql (topic agent) interpreted-object))
      ;; clear slots
      (setf (utterance agent) nil
            (parsed-meaning agent) nil)
      success)))
    

;; --------------
;; + Production +
;; --------------
(defgeneric produce-word (agent)
  (:documentation "Produce an utterance"))

(define-event production-finished (agent mwm-agent))

(defparameter *synonyms*
  '((cube . ("cube" "block")) (sphere . ("sphere" "ball")) (cylinder . ("cylinder"))
    (gray . ("gray")) (red . ("red")) (blue . ("blue")) (green . ("green"))
    (brown . ("brown")) (purple . ("purple")) (cyan . ("cyan")) (yellow . ("yellow"))
    (left . ("left")) (right . ("right")) (front . ("front")) (behind . ("behind"))
    (small . ("small" "tiny")) (large . ("large" "big"))
    (metal . ("metal" "metallic" "shiny")) (rubber . ("rubber" "matte"))))


;;; Tutor production
(defmethod produce-word ((agent mwm-agent))
  "Simply make strings from the symbols. When lexical variation is
   enabled, the tutor randomly chooses one of the available
   synonyms."
  (setf (utterance agent)
        (case (get-configuration agent :tutor-lexicon)
          (:symbolic
           (if (get-configuration agent :lexical-variation)
             (loop for attr in (discriminative-set agent)
                   for synonyms = (rest (assoc attr *synonyms*))
                   collect (random-elt synonyms))
             (mapcar (compose #'downcase #'mkstr)
                     (discriminative-set agent))))
          (:continuous
           (mapcar #'(lambda (cxn) (attr-val cxn :form))
                   (applied-cxns agent)))
          (otherwise nil)))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(defgeneric parse-word (agent)
  (:documentation "Parse an utterance"))

(define-event parsing-finished (agent mwm-agent))

;;; Learner parsing
(defmethod parse-word ((agent mwm-agent))
  "Parse as much words as possible and compute the combined meaning
   using the fuzzy-union operation. Set the applied-cxns and parsed-meaning."
  (multiple-value-bind (meaning cipn)
      (comprehend (utterance agent)
                  :cxn-inventory (grammar agent))
    (when meaning
      (let ((all-meanings
             (loop for cxn in (applied-constructions cipn)
                   collect (attr-val cxn :meaning))))
        (setf (applied-cxns agent) (mapcar #'get-original-cxn
                                           (applied-constructions cipn))
              (parsed-meaning agent) (reduce #'fuzzy-union all-meanings))))
    (notify parsing-finished agent)
    (parsed-meaning agent)))

;; ------------------
;; + Interpretation +
;; ------------------
(defgeneric interpret (agent)
  (:documentation "Interpret a meaning"))

(define-event interpretation-finished (agent mwm-agent))

;;; Learner interpretation
(defmethod interpret ((agent mwm-agent))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (let (interpreted-topic)
    (when (parsed-meaning agent)
      (let* ((objects-with-similarity
              (loop for object in (objects (context agent))
                    for sim = (weighted-similarity object (parsed-meaning agent))
                    collect (cons object sim)))
             ;; if two objects have exactly the same
             ;; maximum similarity, interpretation fails
             (highest-pair
              (the-biggest #'cdr objects-with-similarity))
             (maybe-topic (car highest-pair))
             (duplicatesp (> (count (cdr highest-pair)
                                    objects-with-similarity
                                    :key #'cdr :test #'=)
                             1)))
        (if (hearerp agent)
          (setf (topic agent)
                (unless duplicatesp maybe-topic))
          (setf interpreted-topic
                (unless duplicatesp maybe-topic)))))
    (notify interpretation-finished agent)
    (if (hearerp agent) (topic agent) interpreted-topic)))
              

;; ---------------------
;; + Determine success +
;; ---------------------
(defun closest-to-topic (speaker hearer-context)
  (let ((topic-x (get-attr-val (topic speaker) 'x-pos))
        (topic-y (get-attr-val (topic speaker) 'y-pos)))
    (the-smallest #'(lambda (object)
                      (euclidean (list topic-x topic-y)
                                 (list (get-attr-val object 'xpos)
                                       (get-attr-val object 'ypos))))
                  (objects hearer-context))))

(defgeneric determine-success (speaker hearer)
  (:documentation "Determine the success of the interaction"))

(defmethod determine-success ((speaker mwm-agent) (hearer mwm-agent))
  "Compare the IDs of the topics of both agents"
  (if (eql (get-configuration speaker :data-source) :clevr)
    (when (and (topic speaker) (topic hearer))
      (eql (id (topic speaker)) (id (topic hearer))))
    (when (and (topic speaker) (topic hearer))
      (eql (closest-to-topic speaker (context hearer))
           (topic hearer)))))
  

