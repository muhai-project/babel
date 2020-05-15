(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; Representing, parsing and processing Propbank annotated corpora. ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

;; Pointer to propbank annotated corpora
(defparameter *propbank-annotations-directory* (merge-pathnames "English/propbank-release/data/ontonotes/bc/cnn/00/" *babel-corpora*))

;; Global variable where propbank annotations will be loaded.
(defparameter *propbank-annotations* "Propbank annotations will be stored here.")

;; File wheer propbank annotations will be stored in binary format
(defparameter *propbank-annoations-storage-file* (babel-pathname :directory '("grammars" "propbank-english" "learning")
                                                                 :name "pb-annotations"
                                                                 :type "store"))


;;;;;;;;;;;;;
;; Classes ;;
;;;;;;;;;;;;;

;; Token ;;
;;;;;;;;;;;

(defclass conll-token ()
  ((source-file
    :type string :initarg :source-file 
    :accessor source-file
    :initform nil 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number :initarg :sentence-id 
    :accessor sentence-id
    :initform nil 
    :documentation "The id of the sentence.")
   (token-id 
    :type number :initarg :token-id 
    :accessor token-id
    :initform nil 
    :documentation "The id of the token.")
   (token-string 
    :type string :initarg :token-string 
    :accessor token-string
    :initform nil 
    :documentation "The string of the token.")
   (pos 
    :type string :initarg :pos 
    :accessor pos
    :initform nil 
    :documentation "The pos of the token.")
   (parse-bit
    :type string :initarg :parse-bit 
    :accessor parse-bit
    :initform nil 
    :documentation "The parse bit of the token.")
   (propbank-frame-file 
    :type string :initarg :propbank-frame-file 
    :accessor propbank-frame-file
    :initform nil 
    :documentation "The propbank frame file (.xml) of the predicate.")
   (propbank-roleset 
    :type string :initarg :propbank-roleset 
    :accessor propbank-roleset
    :initform nil 
    :documentation "The propbank frame roleset of the predicate.")
   (propbank-roles 
    :type list :initarg :propbank-roles 
    :accessor propbank-roles
    :initform nil 
    :documentation "A list of propbank roles for this token."))
   (:documentation "Representation of a conll token."))



;; Frame ;;
;;;;;;;;;;;

(defclass propbank-frame ()
  ((frame-name
    :type string :initarg :frame-name 
    :accessor frame-name
    :initform nil 
    :documentation "The name of the frame.")
   (propbank-frame-file 
    :type string :initarg :propbank-frame-file 
    :accessor propbank-frame-file
    :initform nil)
   (frame-roles 
    :type list :initarg :frame-roles 
    :accessor frame-roles
    :initform nil 
    :documentation "The list of frame roles."))
   (:documentation "Representation of a propbank frame."))


;; Frame Role ;;
;;;;;;;;;;;;;;;;

(defclass propbank-frame-role ()
  ((role-type
    :type string :initarg :role-type 
    :accessor role-type
    :initform nil 
    :documentation "The type of the role (e.g. V, arg0, arg1, etc.)")
   (indices 
    :type list :initarg :indices 
    :accessor indices
    :initform nil 
    :documentation "The indices of the words belonging to the role.")
   (role-string 
    :type string :initarg :role-string 
    :accessor role-string
    :initform nil 
    :documentation "The string of the words belonging to the role."))
   (:documentation "Representation of a propbank frame role."))


;; sentence ;;
;;;;;;;;;;;;;;

(defclass conll-sentence ()
  ((source-file
    :type string
    :accessor source-file
    :initform nil 
    :documentation "The path to the source file.")
   (sentence-id 
    :type number 
    :accessor sentence-id
    :initform nil 
    :documentation "The id of the sentence.")
   (tokens 
    :type list :initarg :tokens 
    :accessor tokens
    :initform nil 
    :documentation "The tokens belonging to the sentence.")
   (sentence-string 
    :type string 
    :accessor sentence-string
    :initform nil 
    :documentation "The sentence as a string.")
  (propbank-frames 
    :type string 
    :accessor propbank-frames
    :initform nil 
    :documentation "The propbank frames annotated in the sentence."))
   (:documentation "Representation of a conll sentence."))


(defmethod initialize-instance :after ((sentence conll-sentence) &key &allow-other-keys)
  "Sets all other fields in conll-sentence based on the tokens."
  ;; source file
  (setf (source-file sentence) (source-file (first (tokens sentence))))
  ;; sentence id
  (setf (sentence-id sentence) (sentence-id (first (tokens sentence))))
  ;; sentence string
  (setf (sentence-string sentence) (format nil "~{~a~^ ~}" (mapcar #'token-string (tokens sentence))))
  ;; propbank frames
  (setf (propbank-frames sentence) (loop for role-number from 0 upto (- (length (propbank-roles (first (tokens sentence)))) 1)
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
                      finally return (make-instance 'propbank-frame
                                                    :frame-name frame-name
                                                    :propbank-frame-file frame-file
                                                    :frame-roles propbank-roles)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading in propbank annotated corpora ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-propbank-annotations (&key (store-data t) ignore-stored-data)
  "Loads ProbBank annotations and stores the result. It is loaded from a pb-annotations.store file if it
   is available, from the raw pb-annoation files otherwise. :store-data nil avoids storing the data,
   ignore-stored-data t forces to load the data from the original files."
  ;; Load the data into *propbank-annotations*
  (if (and (probe-file *propbank-annoations-storage-file*)
           (not ignore-stored-data))
    ;; Option 1: Load from storage file if file exists ignore-stored-data is nil.
    (setf *propbank-annotations* (cl-store:restore *propbank-annoations-storage-file*))
    ;; Option 2: Load from original propbank files.
    (setf *propbank-annotations* (load-propbank-annotations-from-files)))
  ;; Store the data into an *fn-data-storage-file*
  (if (and store-data (or ignore-stored-data
                          (not (probe-file *propbank-annoations-storage-file*))))
    (cl-store:store *propbank-annotations* *propbank-annoations-storage-file*))
  ;; Finally return pb-data
  (format nil "Loaded ~a annotated sentences into *propbank-annotations*" (length *propbank-annotations*)))

(defun load-propbank-annotations-from-files ()
  "Loads all framesets and returns the predicate objects."
  (loop for file in (directory (merge-pathnames
                                (make-pathname :name :wild :type "gold_conll")
                                *propbank-annotations-directory*))
        do (format t "Loading ~a~%" file)
        append (read-propbank-conll-file file)))

; (load-propbank-annotations-from-files)


#|
(directory (merge-pathnames
            (make-pathname :name :wild :type "gold_conll")
            (merge-pathnames "English/propbank-release/data/ontonotes/bc/cnn/" *babel-corpora*)))

|#

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
                                                    :token-string (fourth conll-fields)
                                                    :pos (fifth conll-fields)
                                                    :parse-bit (sixth conll-fields)
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
          (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
          (setf current-sentence-tokens (list conll-token))
          (setf previous-sentence-id sentence-id)
          else
          do
          (setf current-sentence-tokens (append current-sentence-tokens (list conll-token)))
          (setf previous-sentence-id sentence-id)
          finally
          do (setf conll-sentences (append conll-sentences (list (make-instance 'conll-sentence :tokens current-sentence-tokens))))
          (return conll-sentences))))
 