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

(defun create-train-test-splits (full-corpus &optional (size-of-test-set 1000))
  "Create train/test splits by randomly selecting size-of-test-set
sentences out of full corpus to serve as test sentences, keeping the
remaining sentences for training."
  (setf full-corpus (shuffle full-corpus))
  (values (subseq full-corpus size-of-test-set) ;;training set
          (subseq full-corpus 0 size-of-test-set)) ;;test set
  )

(multiple-value-bind (training-set test-set)
    (create-train-test-splits *full-corpus* 1000)
  (defparameter  *training-set* training-set)
  (defparameter *test-set* test-set))


(learn-propbank-grammar *training-set*
                        :cxn-inventory '*propbank-grammar-ontonotes-ewt-core-roles*
                        :fcg-configuration '((:replace-when-equivalent . nil)
                                             (:learning-modes :core-roles)))     ;:argm-leaf :argm-pp :argm-sbar :argm-phrase-with-string

*propbank-grammar-ontonotes-ewt-core-roles*
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

;;(get-configuration *propbank-grammar-ontonotes-ewt-core-roles* :heuristics)
;;(set-configuration *propbank-grammar-ontonotes-ewt-core-roles* :heuristics '(:edge-weight :nr-of-roles-integrated))


(comprehend-and-extract-frames "First, Moses told the people every command in the law."
                               :cxn-inventory *propbank-grammar-ontonotes-ewt-core-roles* :timeout 6000 )

(comprehend-and-extract-frames "The children sent him a cake."
                               :cxn-inventory *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*)

(comprehend-and-extract-frames "Mary sent the letter to her cousin."
                               :cxn-inventory *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*)

(loop for propbank-utterance in (subseq *test-set* 1 2)
       do (comprehend-and-extract-frames propbank-utterance :cxn-inventory *propbank-grammar-ontonotes-ewt-core-roles*))




 ;; Evaluating a learnt grammar
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-configuration *propbank-grammar-ontonotes-ewt-core-roles-full-corpus* :heuristics '(:edge-weight :nr-of-roles-integrated)) ;:minimize-path-length

(comprehend-and-evaluate (subseq *test-set* 0 500)
                         *propbank-grammar-ontonotes-ewt-core-roles*
                         :core-roles-only t :include-word-sense t :include-timed-out-sentences nil
                         :include-sentences-with-incomplete-role-constituent-mapping nil :silent nil
                         :timeout 60
                         :per-frame-evaluation t)



;;(cl-store::store *propbank-grammar-ontonotes-ewt-core-roles* "ontonotes-ewt-core-roles-w-hapaxes.fcg")

