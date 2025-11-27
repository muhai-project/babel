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

(defparameter *training-set* (append (train-split *ontonotes-corpus-annotated-with-init-ts*)
                                     (train-split *ewt-corpus-annotated-with-init-ts*)
                                     (dev-split *ontonotes-corpus-annotated-with-init-ts*)
                                     (dev-split *ewt-corpus-annotated-with-init-ts*)))

(defparameter *test-set* (append (test-split *ontonotes-corpus-annotated-with-init-ts*)
                                 (test-split *ewt-corpus-annotated-with-init-ts*)))

;;(mapcar #'sentence-string *training-set*)

(learn-propbank-grammar *full-corpus*
                       #| :excluded-rolesets '("be.01" "be.02" "be.03"
                                             "do.lv" "do.01" "do.02" "do.04" "do.11" "do.12" "done.08"
                                             "have.lv" "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                             "get.lv" "get.03" "get.06" "get.24")|#
                        :cxn-inventory '*propbank-grammar-core-roles*
                        :fcg-configuration '((:replace-when-equivalent . nil)
                                             (:learning-modes :core-roles)))      ;:argm-leaf :argm-pp :argm-sbar :argm-phrase-with-string

;; Cleaning a grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;delete be(v) en have(v) cxns

(defun delete-have-and-be-cxns (grammar)
  "Delete all constructions under the hash keys 'be' and 'have',
together with all constructions for 'be' and 'have' that are stored
under different keys"
  (format t "Nr of cxns before cleaning (FCG-2): ~a ~%" (size grammar))
  (format t "Nr of cxns before cleaning (FCG-1): ~a ~%" (size (processing-cxn-inventory grammar)))
  
  (remhash 'be (constructions-hash-table grammar))
  (remhash 'be (constructions-hash-table (processing-cxn-inventory grammar)))
  (remhash 'have (constructions-hash-table grammar))
  (remhash 'have (constructions-hash-table (processing-cxn-inventory grammar)))
  
  (loop for v being each hash-values of (constructions-hash-table grammar) using (hash-key k)
        for remaining-cxns = (loop for cxn in v
                                   unless (or (search "BE." (subseq (mkstr (name cxn)) 0 3))
                                              (search "HAVE." (mkstr (name cxn))))
                                   collect cxn)
        do (setf (gethash k (constructions-hash-table grammar)) remaining-cxns))

  (loop for v being each hash-values of (constructions-hash-table (processing-cxn-inventory grammar)) using (hash-key k)
        for remaining-cxns = (loop for cxn in v
                                   unless (or (search "BE." (subseq (mkstr (name cxn)) 0 3))
                                              (search "HAVE." (mkstr (name cxn))))
                                   collect cxn)
        do (setf (gethash k (constructions-hash-table (processing-cxn-inventory grammar))) remaining-cxns))

  (format t "Nr of cxns after cleaning (FCG-2): ~a ~%" (size grammar))
  (format t "Nr of cxns after cleaning (FCG-1): ~a ~%" (size (processing-cxn-inventory grammar)))

  )

(delete-have-and-be-cxns *propbank-grammar-core-roles*)



;; Using a learnt grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (activate-monitor trace-fcg)
;; (defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")


(set-configuration *propbank-grammar-core-roles* :heuristics '(:minimize-path-length :nr-of-roles-integrated))

(add-element (make-html *propbank-grammar-core-roles*))

(loop for propbank-utterance in (subseq (shuffle *full-corpus*) 0 5)
      do (comprehend-and-extract-frames propbank-utterance :cxn-inventory *propbank-grammar-core-roles*))

(length *test-set*)


;; Evaluating a learnt grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comprehend-and-evaluate (subseq (shuffle *full-corpus*) 0 10)
                         *propbank-grammar-core-roles*
                         :core-roles-only t :include-word-sense nil :include-timed-out-sentences nil
                         :include-sentences-with-incomplete-role-constituent-mapping nil :silent nil
                         :timeout 120
                         :per-frame-evaluation t)