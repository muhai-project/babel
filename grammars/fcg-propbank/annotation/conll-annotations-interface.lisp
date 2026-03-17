(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;; Parsing PropBank-annotated CoNNL files. ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From PropBank-annotated CoNNL files to conll-sentence objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Token ;;
;;;;;;;;;;;

(defclass conll-token ()
  ((source-file
    :type string
    :accessor source-file
    :initarg :source-file 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :initarg :sentence-id 
    :documentation "The id of the sentence in the file.")
   (token-id 
    :type number
    :accessor token-id
    :initarg :token-id 
    :documentation "The id of the token in the sentence.")
   (token-string 
    :type string 
    :accessor token-string
    :initarg :token-string 
    :initform nil 
    :documentation "The string of the token.")
   (propbank-frame-file 
    :type string 
    :accessor propbank-frame-file
    :initarg :propbank-frame-file 
    :documentation "The propbank frame file (.xml) of the predicate (if applicable).")
   (propbank-roleset 
    :type string 
    :accessor propbank-roleset
    :initarg :propbank-roleset 
    :documentation "The propbank frame roleset of the predicate (if applicable).")
   (propbank-roles 
    :type list 
    :accessor propbank-roles
    :initarg :propbank-roles 
    :documentation "A list of propbank roles for this token."))
  (:documentation "Representation of a CONLL token."))

(defmethod print-object ((token conll-token) (stream t))
  "Printing a conll token."
  (format stream "<Connl-token: ~s>" (token-string token)))


;; Frame ;;
;;;;;;;;;;;

(defclass propbank-frame ()
  ((frame-name
    :type string 
    :accessor frame-name
    :initarg :frame-name 
    :initform nil 
    :documentation "The name of the frame.")
   (propbank-frame-file 
    :type string 
    :accessor propbank-frame-file
    :initarg :propbank-frame-file 
    :initform nil)
   (frame-roles 
    :type list 
    :accessor frame-roles
    :initarg :frame-roles 
    :documentation "The list of frame roles."))
   (:documentation "Representation of a propbank frame."))

(defmethod print-object ((frame propbank-frame) (stream t))
  "Printing a propbank-frame."
  (format stream "<Propbank-frame: ~s>" (frame-name frame)))

(defmethod copy-object-content ((source propbank-frame) (destination propbank-frame))
  "Copying a propbank-frame."
  (setf (frame-name destination) (frame-name source))
  (setf (propbank-frame-file destination) (propbank-frame-file source))
  (setf (frame-roles destination) (frame-roles source)))


;; Frame Role ;;
;;;;;;;;;;;;;;;;

(defclass propbank-frame-role ()
  ((role-type
    :type string
    :accessor role-type
    :initarg :role-type 
    :documentation "The type of the role (e.g. 'V', 'arg0', 'arg1', etc.)")
   (indices 
    :type list 
    :accessor indices
    :initarg :indices 
    :documentation "The indices of the words belonging to the role.")
   (role-string 
    :type string 
    :accessor role-string
    :initarg :role-string 
    :documentation "The string of the words belonging to the role."))
   (:documentation "Representation of a propbank frame role."))

(defmethod print-object ((role propbank-frame-role) (stream t))
  "Printing a propbank-frame-role."
  (format stream "<propbank-frame-role: ~s>" (role-type role)))


;; Sentence ;;
;;;;;;;;;;;;;;

(defclass conll-sentence ()
  ((source-file
    :type string
    :accessor source-file
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :documentation "The id of the sentence.")
   (tokens 
    :type list 
    :accessor tokens
    :initarg :tokens 
    :documentation "The tokens belonging to the sentence.")
   (propbank-frames 
    :type list
    :accessor propbank-frames
    :documentation "The propbank frames annotated in the sentence."))
  (:documentation "Representation of a conll sentence."))

(defmethod print-object ((s conll-sentence) (stream t))
  "Printing a conll-sentence."
  (format stream "<Connl-sentence: ~s>" (format nil "~{~a~^ ~}" (mapcar #'token-string (tokens s)))))

(defmethod initialize-instance :after ((sentence conll-sentence) &key &allow-other-keys)
  "Sets all other fields in conll-sentence based on the tokens."
  ;; source file (retrieve from first token)
  (setf (source-file sentence) (source-file (first (tokens sentence))))
  ;; sentence id (retrieve from first token)
  (setf (sentence-id sentence) (sentence-id (first (tokens sentence))))                                                  
  ;; propbank frames
  (setf (propbank-frames sentence)
        (remove nil
                (loop for role-number from 0 upto (- (length (propbank-roles (first (tokens sentence)))) 1)
                      collect (loop with frame-name = nil
                                    with frame-file = nil
                                    with propbank-roles = nil
                                    with current-open-role = nil
                                    with current-open-role-indices = nil
                                    with current-open-role-strings = nil

                                    for token in  (tokens sentence)
                                    for role-field = (nth role-number (propbank-roles token))
                                    ;; Role opening and closing (single token)
                                    if
                                      (and (not current-open-role)
                                           (string= "(" (subseq role-field 0 1))
                                           (string= ")" (subseq role-field (- (length role-field) 1))))
                                      do
                                        (when (string= "V" (subseq role-field 1 (- (length role-field) 2)))
                                          (setf frame-name (propbank-roleset token))
                                          (setf frame-file (propbank-frame-file token)))
                                        (setf propbank-roles (append propbank-roles (list (make-instance 'propbank-frame-role
                                                                                                         :role-type (subseq role-field 1 (- (length role-field) 2))
                                                                                                         :indices (list (token-id token))
                                                                                                         :role-string (token-string token)))))
                                    else
                      
                                      ;; Role opening
                                      if
                                        (and (not current-open-role)
                                             (string= "(" (subseq role-field 0 1)))
                                        do
                                          (when (string= "V" (subseq role-field 1 (- (length role-field) 1)))
                                            (setf frame-name (propbank-roleset token))
                                            (setf frame-file (propbank-frame-file token)))
                                          (setf current-open-role (subseq role-field 1 (- (length role-field) 1)))
                                          (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                                          (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                                      else
                                        ;; Role continuing
                                        if
                                          (and current-open-role
                                               (string= "*" role-field))
                                          do
                                            (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                                            (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                                        else
                                          ;; Role closing
                                          if
                                            (and current-open-role
                                                 (string= ")" (subseq role-field (- (length role-field) 1))))
                                            do
                                              (setf current-open-role-indices (append current-open-role-indices (list (token-id token))))
                                              (setf current-open-role-strings (append current-open-role-strings (list (token-string token))))
                                              (setf propbank-roles (append propbank-roles (list (make-instance 'propbank-frame-role
                                                                                                               :role-type current-open-role
                                                                                                               :indices current-open-role-indices
                                                                                                               :role-string (format nil "~{~a~^ ~}" current-open-role-strings)))))
                                              (setf current-open-role nil)
                                              (setf current-open-role-indices nil)
                                              (setf current-open-role-strings nil)
                                    finally (return (when (> (length propbank-roles) 1) ;;do not create annotations for rolesets with only 'v' role
                                                      (make-instance 'propbank-frame
                                                                     :frame-name frame-name
                                                                     :propbank-frame-file frame-file
                                                                     :frame-roles propbank-roles))))))))



(defun read-propbank-conll-file (pathname)
  "Reads a propbank-conll file and returns a list of conll-sentence objects."
  (let* ((conll-lines (with-open-file (inputstream pathname :direction :input)
                        (uiop/stream:read-file-lines inputstream)))
         (conll-tokens (loop for line in conll-lines
                             for conll-fields = (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)
                             unless (equalp line "")
                               collect (make-instance 'conll-token
                                                      :source-file (first conll-fields)
                                                      :sentence-id (parse-integer (second conll-fields))
                                                      :token-id (parse-integer (third conll-fields))
                                                      :token-string
                                                      ;; to investigate:
                                                      (if (and (string= (subseq (fourth conll-fields) 0 1) "/")
                                                                             (not (string= (subseq (fourth conll-fields) 1) "")))
                                                                      (subseq (fourth conll-fields) 1)
                                                                      (fourth conll-fields))
                                                      :propbank-frame-file (seventh conll-fields)
                                                      :propbank-roleset (eighth conll-fields)
                                                      :propbank-roles (subseq conll-fields 8)))))
    ;; Collect all propbank sentences as objects.
    (loop with conll-sentences = nil
          with previous-sentence-id = nil
          with current-sentence-tokens = nil
          for conll-token in conll-tokens
          for sentence-id = (sentence-id conll-token)
          if (and previous-sentence-id
                  (not (= sentence-id previous-sentence-id)))
            do
              ;; add previous sentence to conll-sentences
              (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
              ;; start collecting tokens of new sentence
              (setf current-sentence-tokens (list conll-token))
              (setf previous-sentence-id sentence-id)
          else
              ;; add token to corrent sentence
            do
              (setf current-sentence-tokens (append current-sentence-tokens (list conll-token)))
              (setf previous-sentence-id sentence-id)
          finally
            ;; the last sentence still needs to be added.
            (progn (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
              (return conll-sentences)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From PropBank-annotated sentences to spacy-benepar-annotated sentences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spacy-benepar-sentence ()
  ((source-file
    :type string
    :accessor source-file
    :initarg :source-file
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :initarg :sentence-id
    :documentation "The id of the sentence.")
   (tokens 
    :type list 
    :accessor tokens
    :initarg :tokens 
    :documentation "The tokens as a list of strings.")
   (propbank-frames 
    :type list
    :accessor propbank-frames
    :initarg :propbank-frames
    :documentation "The propbank frames annotated in the sentence.")
   (sentence-string 
    :type string
    :accessor sentence-string
    :documentation "The string that serves as input to syntactic analysis.")
   (syntactic-analysis 
    :type list
    :accessor syntactic-analysis
    :documentation "The spacy-benepar analysis of the sentence.")
   (language 
    :type string
    :accessor language
    :initarg :language
    :documentation "Language code for the sentence."))
  (:documentation "Representation of a spacy-benepar-annotated sentence."))


(defmethod initialize-instance :after ((sentence spacy-benepar-sentence) &key &allow-other-keys)
  "Sets all other fields in conll-sentence based on the tokens."
  ;; sentence string
  (setf (sentence-string sentence) (loop with first-token = t
                                         with sentence-string-list = nil
                                         for token-string in (tokens sentence)
                                         do (cond (first-token
                                                   (setf first-token nil)
                                                   (setf sentence-string-list (append sentence-string-list (list token-string))))
                                                  ((member token-string '("'ve" "'d" "'ll" "'t" "'m" "'s" "'re") :test #'string=)
                                                   (setf sentence-string-list (append sentence-string-list (list token-string))))
                                                  (t
                                                   (setf sentence-string-list (append sentence-string-list (list " " token-string)))))
                                         finally (return (format nil "~{~a~}" sentence-string-list))))
                                                    
  ;; syntactic anlysis
  (setf (syntactic-analysis sentence) (nlp-tools:get-penelope-syntactic-analysis (sentence-string sentence)
                                                                                 :model (format nil "~a_benepar" (language sentence)))))

(defun conll-sentence-to-spacy-benepar-sentence (conll-sentence language)
  "Takes a conll-sentence and returns a spacy-benepar-sentence."
  (make-instance 'spacy-benepar-sentence
                 :source-file (source-file conll-sentence)
                 :sentence-id (sentence-id conll-sentence)
                 :tokens (mapcar #'token-string (tokens conll-sentence))
                 :propbank-frames (propbank-frames conll-sentence)
                 :language language))

(defun add-syntactic-analysis (corpus language)
  "Returns a corpus with all conll-sentences being annotated using spacy-benepar."
  (make-instance 'corpus
                 :name (name corpus)
                 :train-split (loop for sentence in (train-split corpus)
                                    collect (conll-sentence-to-spacy-benepar-sentence sentence language))
                 :dev-split (loop for sentence in (dev-split corpus)
                                    collect (conll-sentence-to-spacy-benepar-sentence sentence language))
                 :test-split (loop for sentence in (test-split corpus)
                                     collect (conll-sentence-to-spacy-benepar-sentence sentence language))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From to spacy-benepar-annotated sentences to fcg-propbank-sentences    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-initial-transient-structures (corpus)
  "Returns a corpus with all spacy benepar sentences being annotated with an initial transient structure."
  (make-instance 'corpus
                 :name (name corpus)
                 :train-split (loop for sentence in (train-split corpus)
                                    collect (spacy-benepar-sentence-to-fcg-propbank-sentence sentence))
                 :dev-split (loop for sentence in (dev-split corpus)
                                  collect (spacy-benepar-sentence-to-fcg-propbank-sentence sentence))
                 :test-split (loop for sentence in (test-split corpus)
                                   collect (spacy-benepar-sentence-to-fcg-propbank-sentence sentence))))

(defun spacy-benepar-sentence-to-fcg-propbank-sentence (spacy-benepar-sentence)
  "Takes a spacy-benepar-sentence. and returns a fcg-propbank-sentence"
  (make-instance 'fcg-propbank-sentence
                 :source-file (source-file spacy-benepar-sentence)
                 :sentence-id (sentence-id spacy-benepar-sentence)
                 :tokens (tokens spacy-benepar-sentence)
                 :sentence-string (sentence-string spacy-benepar-sentence)
                 :propbank-frames (propbank-frames spacy-benepar-sentence)
                 :language (language spacy-benepar-sentence)
                 :initial-transient-structure (create-initial-transient-structure-based-on-benepar-analysis
                                               (syntactic-analysis spacy-benepar-sentence))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert conll-sentence into fcg-propbank-sentence    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod connl->fcg-propbank ((sentence conll-sentence) &key (language "en"))
  "Turn connl-sentence into fcg-propbank sentence for learning"
  (let ((spacy-benepar-sentence (conll-sentence-to-spacy-benepar-sentence sentence language)))
    (spacy-benepar-sentence-to-fcg-propbank-sentence spacy-benepar-sentence)))

