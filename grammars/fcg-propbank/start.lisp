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

;; Append all splits from OntoNotes and EWT corpora
(defparameter *full-corpus* (append (train-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (test-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (dev-split *ontonotes-corpus-annotated-with-init-ts*)
                                    (train-split *ewt-corpus-annotated-with-init-ts*)
                                    (test-split *ewt-corpus-annotated-with-init-ts*)
                                    (dev-split *ewt-corpus-annotated-with-init-ts*)))


(defparameter *training-set* nil)
(defparameter *test-set* nil)

;; Randomly select 1000 sentences from the full corpus as test sentences,
;; while using the remaining sentences for learning the grammar
(multiple-value-bind (training-set test-set)
    (create-train-test-splits *full-corpus* 1000)
  (setf  *training-set* training-set)
  (setf *test-set* test-set))

;; Learn a new grammar for the core roles in the PropBank annotation
(learn-propbank-grammar *full-corpus*
                        :cxn-inventory '*propbank-grammar-core-roles*
                        :fcg-configuration '((:learning-modes :core-roles)))

;; Optionally, store the grammar for later reuse
(cl-store:store *propbank-grammar-core-roles*
                (babel-pathname :directory '(".tmp")
                                :name (mkstr (downcase (name *propbank-grammar-core-roles*)))
                                :type "fcg"))



;; Using a learnt grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (activate-monitor trace-fcg)
;; (defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;;(get-configuration *propbank-grammar-core-roles* :heuristics)
;;(set-configuration *propbank-grammar-core-roles* :heuristics '(:edge-weight :nr-of-roles-integrated))
(set-configuration (visualization-configuration *propbank-grammar-core-roles*) :show-constructional-dependencies nil)
(comprehend-and-extract-frames "First, Moses told the people every command in the law."
                               :cxn-inventory *propbank-grammar-core-roles*)

(comprehend-and-extract-frames "The children sent him a cake."
                               :cxn-inventory *propbank-grammar-core-roles*)

(loop for propbank-utterance in (subseq *test-set* 2 4)
       do (comprehend-and-extract-frames propbank-utterance :cxn-inventory *propbank-grammar-core-roles* :timeout 90 :silent nil))


;; Inspecting a learnt grammar and its network
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(grammar-report *propbank-grammar-core-roles*)


(pprint (rolesets-for-schema '((:arg0  np)
                               (:v v)
                               (:arg2 np)
                               (:arg1  np))
                             *propbank-grammar-core-roles*))

(pprint (rolesets-for-schema '((:arg0  np)
                               (:v v)
                               (:arg1 ?y)
                               (:arg2  pp))
                              *propbank-grammar-core-roles*))


(pprint (find-schemata-for-roleset 'TELL.01 *propbank-grammar-core-roles*))

(pprint (graph-utils::closest-nodes 'TELL.01 (fcg::graph (categorial-network *propbank-grammar-core-roles*))
                                    :edge-type 'gram-sense))

 ;; Evaluating a learnt grammar
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comprehend-and-evaluate *test-set* *propbank-grammar-core-roles*
                         :core-roles-only t :include-word-sense t
                         :include-timed-out-sentences t
                         :include-sentences-with-incomplete-role-constituent-mapping t
                         :timeout 90 :silent nil
                         :per-frame-evaluation t) ;;:only-evaluate t



 ;; Plotting
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *grammar* (cl-store:restore (babel-pathname :directory '(".tmp")
                                :name "propbank-learned"
                                :type "fcg")))

(defparameter *sorted-freqs-fe-cxns* nil)
(defparameter *sorted-freqs-argst-cxns* nil)
(defparameter *sorted-freqs-roleset-cxns* nil)

(multiple-value-bind (fe-freqs argst-freqs roleset-freqs)
    (loop for cxn in (constructions-list *grammar*)
          if (eql (attr-val cxn :label) 'lexical-cxn)
            collect (attr-val cxn :score) into fe-cxn-freqs
          else if (eql (attr-val cxn :label) 'argument-structure-cxn)
                 collect (attr-val cxn :score) into argst-cxn-freqs
            else if (eql (attr-val cxn :label) 'word-sense-cxn)
                   collect (attr-val cxn :score) into roleset-cxn-freqs
          finally (return (values (sort fe-cxn-freqs #'>)
                                  (sort argst-cxn-freqs #'>)
                                  (sort roleset-cxn-freqs #'>))))
  (setf *sorted-freqs-fe-cxns* fe-freqs)
  (setf *sorted-freqs-argst-cxns* argst-freqs)
  (setf *sorted-freqs-roleset-cxns* roleset-freqs))



(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "fe-cxn-freqs"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *sorted-freqs-fe-cxns*) f))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "argst-cxn-freqs"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *sorted-freqs-argst-cxns*) f))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "roleset-cxn-freqs"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *sorted-freqs-roleset-cxns*) f))


(ql:quickload :plot-raw-data)

(plot-raw-data::raw-files->evo-plot  
 :raw-file-paths '((".tmp" "fe-cxn-freqs")
                   (".tmp" "argst-cxn-freqs")
                   (".tmp" "roleset-cxn-freqs"))
 :average-windows 1
 :logscale 'xy
 :y1-label "Construction Frequency (log)"
 :x-label "Rank (log)"
 :colors '("medium-blue"  "dark-pink" "dark-turquoise" )
 :captions '("Frame-evoking cxns" "Argument structure cxns" "Roleset cxns")
 :fsize 11
 :key-box t
 :key-location t
 :grid-line-width 0.1
 :line-width 2.5
 :step 1
 ;;:end 50
  )

(length *cxns-sorted-by-freq*)

;;*great-gnuplot-colors*
;;("#328888" "dark-goldenrod" "dark-red" "navy" "dark-green" "gray30" "light-red" "green" "dark-orange" "royalblue" "sea-green" "dark-pink" "purple" "orange-red" "gray50" "dark-khaki" "dark-turquoise" "salmon" "dark-magenta" "dark-yellow" "violet" "light-green")
