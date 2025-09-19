(in-package :fcg-propbank)

;; (ql:quickload :fcg-propbank)


;; Retrieving the training corpora annotated with initial transient structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defparameter *ewt-corpus-annotated-with-init-ts* (cl-store:restore *ewt-init-ts-annotated-corpus-file*))

(defparameter *ontonotes-corpus-annotated-with-init-ts* (cl-store:restore *ontonotes-init-ts-annotated-corpus-file*))




;; Learning grammars from the annotated data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *full-corpus* (append (train-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (test-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (dev-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (train-split *ewt-corpus-annotated-with-init-ts*)
                                    (test-split *ewt-corpus-annotated-with-init-ts*)
                                    (dev-split *ewt-corpus-annotated-with-init-ts*)))

(defparameter *training-set* (append ;(train-split *ontonotes-corpus-annotated-with-init-ts*)
                                     (train-split *ewt-corpus-annotated-with-init-ts*)
                                     ;(dev-split *ontonotes-corpus-annotated-with-init-ts*)
                                     (dev-split *ewt-corpus-annotated-with-init-ts*)))

(mapcar #'sentence-string *training-set*)

(learn-propbank-grammar (subseq *training-set* 225 226)
                        #|:excluded-rolesets '("be.01" "be.02" "be.03"
                                             "do.01" "do.02" "do.04" "do.11" "do.12"
                                             "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                             "get.03" "get.06" "get.24")|#
                        :cxn-inventory '*ewt-grammar*
                        :fcg-configuration '((:replace-when-equivalent . nil)
                                             (:learning-modes :core-roles)))      ;:argm-leaf :argm-pp :argm-sbar :argm-phrase-with-string


;; Using a learnt grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;

(activate-monitor trace-fcg)

(set-configuration *ewt-grammar* :heuristics '(:minimize-path-length :nr-of-roles-integrated))

(add-element (make-html *ewt-grammar*))

(loop for propbank-utterance in (subseq *training-set* 225 226)
      do (comprehend-and-extract-frames propbank-utterance :cxn-inventory *ewt-grammar*))
