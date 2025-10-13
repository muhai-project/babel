(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; Loading PropBank-annotated corpora: EWT and Ontonotes English    ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :fcg-propbank)

;; Bind *babel-corpora* if not yet bound.
(unless (boundp '*babel-corpora*)
  (warn "*babel-corpora* not bound.")
  (defparameter *babel-corpora* "no-corpus-path-provided"))

#|
;; 1. From ConLL files to corpus object (without augmentations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ;; => Set to true to rebuilt corpora from scratch
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)

;; 2. From corpus without augmentations to corpus with syntactic analyses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

(defparameter *ewt-corpus-annotated-with-spacy-benepar* (add-syntactic-analysis *ewt-propbank-annotated-corpus* "en"))
(defparameter *ontonotes-corpus-annotated-with-spacy-benepar* (add-syntactic-analysis *ontonotes-propbank-annotated-corpus* "en"))

(defparameter *ontonotes-syntactically-annotated-corpus-file*
  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                  :name "ontonotes-syntactically-annotated-corpus"
                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                   *babel-corpora*))

(defparameter *ewt-syntactically-annotated-corpus-file*
  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                  :name "ewt-syntactically-annotated-corpus"
                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                   *babel-corpora*))

(cl-store:store *ewt-corpus-annotated-with-spacy-benepar* *ewt-syntactically-annotated-corpus-file*)
(cl-store:store *ontonotes-corpus-annotated-with-spacy-benepar* *ontonotes-syntactically-annotated-corpus-file*)


;; 3. From corpus with syntactic analysis for corpus with initial transient structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (boundp '*ewt-corpus-annotated-with-spacy-benepar*)
  (defparameter *ewt-corpus-annotated-with-spacy-benepar* (cl-store:restore *ewt-syntactically-annotated-corpus-file*)))

(unless (boundp '*ontonotes-corpus-annotated-with-spacy-benepar*)
  (defparameter *ontonotes-corpus-annotated-with-spacy-benepar* (cl-store:restore *ontonotes-syntactically-annotated-corpus-file*)))

(defparameter *ewt-corpus-annotated-with-init-ts* (add-initial-transient-structures *ewt-corpus-annotated-with-spacy-benepar*))
(defparameter *ontonotes-corpus-annotated-with-init-ts* (add-initial-transient-structures *ontonotes-corpus-annotated-with-spacy-benepar*))

(defparameter *ewt-init-ts-annotated-corpus-file*
  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                  :name "ewt-init-ts-annotated-corpus"
                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                   *babel-corpora*))

(defparameter *ontonotes-init-ts-annotated-corpus-file*
  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                  :name "ontonotes-init-ts-annotated-corpus"
                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                   *babel-corpora*))

(cl-store:store *ewt-corpus-annotated-with-init-ts* *ewt-init-ts-annotated-corpus-file*)
(cl-store:store *ontonotes-corpus-annotated-with-init-ts* *ontonotes-init-ts-annotated-corpus-file*)
|#



(defparameter *ontonotes-annotations-directory* (merge-pathnames
                                                 (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-release" "data")))
                                                 *babel-corpora*))

(defparameter *ewt-annotations-directory* (merge-pathnames
                                           (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-release" "data" "google" "ewt")))
                                           *babel-corpora*))

;; Global variables where propbank annotations will be loaded.
(defparameter *ontonotes-annotations* "Ontonotes annotations will be stored here.")
(defparameter *ewt-annotations* "Ewt annotations will be stored here.")

;; File where propbank annotations will be stored in binary format
(defparameter *ontonotes-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ontonotes-annotations"
                                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                                    *babel-corpora*))

(defparameter *ewt-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ewt-annotations"
                                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                                    *babel-corpora*))

  
(defun load-propbank-annotations (corpus-name &key (store-data t) ignore-stored-data)
  "Loads ProbBank annotations and stores the result. It is loaded from a pb-annotations.store file if it
   is available, from the raw pb-annoation files otherwise. :store-data nil avoids storing the data,
   ignore-stored-data t forces to load the data from the original files."
  (let ((corpus-annotations-storage-file (get-corpus-annotations-storage-file corpus-name)))
    (if (and (probe-file corpus-annotations-storage-file)
             (not ignore-stored-data))
      ;; Option 1: Load from storage file if file exists ignore-stored-data is nil.
      (case corpus-name
        (ewt (setf *ewt-propbank-annotated-corpus* (cl-store:restore corpus-annotations-storage-file)))
        (ontonotes (setf *ontonotes-propbank-annotated-corpus* (cl-store:restore corpus-annotations-storage-file))))
      ;; Option 2: Load from original propbank files.
      (case corpus-name
        (ewt (setf *ewt-propbank-annotated-corpus* (load-propbank-annotations-from-files corpus-name)))
        (ontonotes (setf *ontonotes-propbank-annotated-corpus* (load-propbank-annotations-from-files corpus-name)))))
    ;; Store the data into an *fn-data-storage-file*
    (if (and store-data (or ignore-stored-data
                            (not (probe-file corpus-annotations-storage-file))))
      (case corpus-name
        (ewt (cl-store:store *ewt-propbank-annotated-corpus* corpus-annotations-storage-file))
        (ontonotes (cl-store:store *ontonotes-propbank-annotated-corpus* corpus-annotations-storage-file))))

    (format nil "Loaded ~a annotated sentences into ~a"
            (+ (length (train-split (case corpus-name
                                      (ewt *ewt-propbank-annotated-corpus*)
                                      (ontonotes *ontonotes-propbank-annotated-corpus*))))
               (length (dev-split (case corpus-name
                                    (ewt *ewt-propbank-annotated-corpus*)
                                    (ontonotes *ontonotes-propbank-annotated-corpus*))))
               (length (test-split (case corpus-name
                                     (ewt *ewt-propbank-annotated-corpus*)
                                     (ontonotes *ontonotes-propbank-annotated-corpus*)))))
            (case corpus-name
              (ewt '*ewt-propbank-annotated-corpus*)
              (ontonotes '*ontonotes-propbank-annotated-corpus*)))))


(defun get-corpus-annotations-storage-file (corpus-name)
  (case corpus-name
    (ewt *ewt-annotations-storage-file*)
    (ontonotes *ontonotes-annotations-storage-file*)))


(defun load-propbank-annotations-from-files (corpus-name)
  "Loads all framesets and returns the predicate objects for a given corpus."
  (let* ((file-names-for-splits (get-corpus-file-lists corpus-name))
         (file-names-train (first file-names-for-splits))
         (file-names-dev (second file-names-for-splits))
         (file-names-test (third file-names-for-splits)))
    (make-instance 'corpus
                   :name corpus-name
                   :train-split (load-split-from-files file-names-train)
                   :dev-split (load-split-from-files file-names-dev)
                   :test-split (load-split-from-files file-names-test))))

(defun load-split-from-files (file-names-for-split)
  (loop for file in file-names-for-split
        if (probe-file file)
        do (format t "Loading file: ~s.~%" file)
        and
        append (read-propbank-conll-file file)
        into sentences
        else
        do (warn (format nil "File not found: ~s." file))
        finally
        (return sentences)))


(defun get-corpus-file-lists (corpus-name)
  (case corpus-name
    (ewt (ewt-file-lists))
    (ontonotes (ontonotes-file-lists))))

(defun ontonotes-file-lists ()
  "Returns the filelists for train, dev and test splits."
  (loop for split in (list (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ontonotes-train-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*)
                           (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ontonotes-dev-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*)
                           (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ontonotes-test-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*))
        collect
        (with-open-file (inputstream split :direction :input)
          (mapcar #'(lambda (subpath)
                     (merge-pathnames (string-append subpath ".gold_conll") *ontonotes-annotations-directory*))
                  (uiop/stream:read-file-lines inputstream)))))

(defun ewt-file-lists ()
  "Returns the filelists for train, dev and test splits."
  (loop for split in (list (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ewt-train-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*)
                           (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ewt-dev-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*)
                           (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "propbank-annotations"))
                                                                                   :name "ewt-test-list"
                                                                                   :type "txt")
                                                                    *babel-corpora*))
        collect
        (with-open-file (inputstream split :direction :input)
          (mapcar #'(lambda (subpath)
                     (merge-pathnames (string-append subpath ".gold_conll") *ewt-annotations-directory*))
                  (uiop/stream:read-file-lines inputstream)))))

;; (ontonotes-file-lists)




