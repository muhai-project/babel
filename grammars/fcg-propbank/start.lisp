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

(learn-propbank-grammar *full-corpus*
                        :excluded-rolesets '("be.01" "be.02" "be.03"
                                             "do.lv" "do.01" "do.02" "do.04" "do.11" "do.12"
                                             "have.lv" "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                             "get.lv" "get.03" "get.06" "get.24")
                        :cxn-inventory '*propbank-grammar*
                        :fcg-configuration '((:replace-when-equivalent . nil)
                                             (:learning-modes :core-roles)))      ;:argm-leaf :argm-pp :argm-sbar :argm-phrase-with-string


;; Using a learnt grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (activate-monitor trace-fcg)
;; (defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")


(set-configuration *propbank-grammar* :heuristics '(:minimize-path-length :nr-of-roles-integrated))

(add-element (make-html *propbank-grammar*))

(loop for propbank-utterance in (subseq (shuffle *full-corpus*) 0 5)
      do (comprehend-and-extract-frames propbank-utterance :cxn-inventory *propbank-grammar*))
