(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                             ;;
;; Functions and Methods supporting FCG processing or PropBank English grammar ;;
;;                                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*)
                                                (silent nil)
                                                (syntactic-analysis nil)
                                                (selected-rolesets nil)
                                                (timeout 60))
  "Comprehends an utterance and visualises the extracted frames."
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent :syntactic-analysis syntactic-analysis :selected-rolesets selected-rolesets :timeout timeout)
    (if (eql solution 'time-out)
      (values 'time-out 'time-out 'time-out)
      (let ((frames (extract-frames (car-resulting-cfs (cipn-car cipn)))))
        (unless silent
          (add-element `((h3 :style "margin-bottom:3px;") "Frame representation:"))
          (add-element (make-html frames  :expand-initially t)))
        (values solution cipn frames)))))


;; Comprehend Methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comprehend ((utterance conll-sentence) &key (cxn-inventory *fcg-constructions*) (silent nil) (selected-rolesets nil) (timeout 60) &allow-other-keys)
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory)))
    (set-data initial-cfs :annotation (propbank-frames utterance))
    (unless silent (notify parse-started (listify (sentence-string utterance)) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                                                    (comprehend-with-rolesets initial-cfs cxn-inventory selected-rolesets (sentence-string utterance) silent))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))


(defmethod comprehend ((utterance string) &key (syntactic-analysis nil) (cxn-inventory *fcg-constructions*)  (silent nil) (selected-rolesets nil) (timeout 60))
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory :syntactic-analysis syntactic-analysis)))
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                                                    (comprehend-with-rolesets initial-cfs cxn-inventory selected-rolesets utterance silent))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))

(defun comprehend-with-rolesets (initial-cfs cxn-inventory selected-rolesets utterance silent)
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (set-data initial-cfs :selected-rolesets selected-rolesets)
    (set-data initial-cfs :utterance utterance)
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (when solution
                          (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


;; Hash Methods ;;
;;;;;;;;;;;;;;;;;;

(defmethod hash ((construction construction)
                 (mode (eql :hash-lemma))
                 &key &allow-other-keys)
  "Returns the lemma from the attributes of the construction"
  (when (attr-val construction :lemma)
     (remove nil (list (attr-val construction :lemma)))))


(defmethod hash ((node cip-node)
                 (mode (eql :hash-lemma)) 
                 &key &allow-other-keys)
  "Checks all units for a lemma feature."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lemma = (if (equalp (unit-feature-value unit 'node-type) 'leaf)
                      (unit-feature-value unit 'lemma)
                      (or (unit-feature-value unit 'lemma) ;;for phrasals
                          (intern (upcase (unit-feature-value unit 'string)))))
        when lemma
        collect it))


;; Node Tests   ;;
;;;;;;;;;;;;;;;;;;

(defmethod cip-node-test ((node cip-node) (mode (eql :check-double-role-assignment)))
  "Node test that checks if there is a frame in the resulting meaning
in which there are duplicate role assignments (i.e. unit name of
frame-element filler occurs in more than one slot). "
  (let ((extracted-frames (group-by (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))
                                    #'third :test #'equalp)))
    (loop with double-role-assignments = nil
          for (frame-var . frame) in extracted-frames
          for frame-elements = (loop for predicate in frame
                                     when (equalp (first predicate) 'frame-element)
                                     collect predicate)
          
          when (or (> (length frame-elements)
                      (length (remove-duplicates frame-elements :key #'fourth :test #'equalp)))
                    (loop for fe in frame-elements
                          for other-fes = (remove fe frame-elements :key #'fourth :test #'equalp)
                          thereis (subconstituent-p (fourth fe) (mapcar #'fourth other-fes) (left-pole-structure (car-resulting-cfs (cipn-car node))))))
          do (push frame-var double-role-assignments)
          finally
          (return
           (if double-role-assignments
             ;;some frames contain frame-elements that have identical slot fillers
             (and (push 'double-role-assignment (statuses node)) nil)
             t)))))

(defun subconstituent-p (frame-element other-frame-elements unit-structure)
  (loop for ofe in other-frame-elements
        when (subconstituent-p-aux frame-element ofe unit-structure)
        do (return t)))

(defun subconstituent-p-aux (frame-element other-frame-element unit-structure)
  (let ((parent (cadr (find 'parent (unit-body (find frame-element unit-structure :key #'unit-name)) :key #'feature-name))))
    (cond ((null parent)
           nil)
          ((equalp parent other-frame-element)
           t)
          ((subconstituent-p-aux parent other-frame-element unit-structure)
           t))))


;; Goal tests   ;;
;;;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-valid-children)))
  "Checks whether there are no more applicable constructions when a node is
fully expanded and no constructions could apply to its children
nodes."
  (and (or (not (children node))
	   (loop for child in (children node)
                 never (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child))))))
       (fully-expanded? node)))

#|
(defmethod cip-goal-test ((cipn cip-node) (mode (eql :gold-standard-meaning)))
  "Returns true if no more valid children or gold standard meaning reached."
  (or (and (or (not (children cipn))
	   (loop for child in (children cipn)
                 never (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child))))))
       (fully-expanded? cipn))
      (let* ((extracted-frames (extract-frames (car-resulting-cfs (cipn-car cipn))))
             (selected-rolesets (get-data (car-resulting-cfs (cipn-car cipn)) :selected-rolesets))
             (annotated-frames (get-data (car-resulting-cfs (cipn-car cipn)) :annotation))
             (number-of-gold-standard-predictions (loop with number-of-gold-standard-predictions = 0
                                                        for frame in annotated-frames
                                                        if (or (null selected-rolesets)
                                                               (find (frame-name frame) selected-rolesets :test #'equalp))
                                                        do (loop for role in (frame-roles frame)
                                                                 do
                                                                 (setf number-of-gold-standard-predictions (+ number-of-gold-standard-predictions (length (indices role)))))
                                                        finally
                                                        return number-of-gold-standard-predictions))
             ;; Number of predication made by the grammar
             (number-of-predictions (loop with number-of-predictions = 0
                                          for frame in (frames extracted-frames)
                                          if (or (null selected-rolesets)
                                                 (find (symbol-name (frame-name frame)) selected-rolesets :test #'equalp))
                                          do
                                          ;; for frame-elements
                                          (loop for role in (frame-elements frame)
                                                do
                                                (setf number-of-predictions (+ number-of-predictions (length (indices role)))))
                                          ;; from frame-evoking-element
                                          (when (and (frame-evoking-element frame) (index (frame-evoking-element frame)))
                                            (setf number-of-predictions (+ number-of-predictions 1)))
                                          finally
                                          return number-of-predictions))
             ;; Number of correct predictions made
             (number-of-correct-predictions (loop with number-of-correct-predictions = 0
                                                  for predicted-frame in (frames extracted-frames)
                                                  ;; check whether we're interested in the frame
                                                  if (or (null selected-rolesets)
                                                         (find (symbol-name (frame-name predicted-frame)) selected-rolesets :test #'equalp))
                                                  do
                                                  ;; For frame elements
                                                  (loop for predicted-frame-element in (frame-elements predicted-frame)
                                                        for predicted-indices = (indices predicted-frame-element)
                                                        do (loop for index in predicted-indices
                                                                 when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                                                   annotated-frames)
                                                                 do (setf number-of-correct-predictions (+ number-of-correct-predictions 1))))
                                                  ;; For frame-evoking element
                                                  (when (correctly-predicted-fee-index-p (index (frame-evoking-element predicted-frame))
                                                                                         predicted-frame
                                                                                         annotated-frames)
                                                    (setf number-of-correct-predictions (+ number-of-correct-predictions 1)))
                                                  finally
                                                  return number-of-correct-predictions))
             (result (cond ((= 0 number-of-gold-standard-predictions)
                            `((:precision . ,(if (= 0 number-of-predictions) 1.0 0.0))
                              (:recall . 1.0)
                              (:f1-score . ,(float (* 2 (/ (* (if (= 0 number-of-predictions) 1.0 0.0)
                                                              1.0)
                                                           (+ (if (= 0 number-of-predictions) 1.0 0.0)
                                                              1.0)))))
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           ((= 0 number-of-predictions)
                            `((:precision . 1.0)
                              (:recall . 0.0)
                              (:f1-score . 0.0)
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           ((= 0 number-of-correct-predictions)
                            `((:precision . 0.0)
                              (:recall . 0.0)
                              (:f1-score . 0.0)
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           (t
                            `((:precision . ,(float (/ number-of-correct-predictions number-of-predictions)))
                              (:recall . ,(float (/ number-of-correct-predictions number-of-gold-standard-predictions)))
                              (:f1-score . ,(float (* 2 (/ (* (/ number-of-correct-predictions number-of-predictions)
                                                              (/ number-of-correct-predictions number-of-gold-standard-predictions))
                                                           (+ (/ number-of-correct-predictions number-of-predictions)
                                                              (/ number-of-correct-predictions number-of-gold-standard-predictions))))))
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))))
    
        (when (= (cdr (assoc :f1-score result)) 1.0)
          t))))


(defmethod cip-goal-test ((node cip-node) (mode (eql :no-double-role-assignment)))
  "Node test that checks if there is a frame in the resulting meaning
in which there are duplicate role assignments (i.e. unit name of
frame-element filler occurs in more than one slot). "
  (let ((extracted-frames (group-by (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))
                                    #'third :test #'equalp)))
    (loop with double-role-assignments = nil
          for (frame-var . frame) in extracted-frames
          for frame-elements = (loop for predicate in frame
                                     when (equalp (first predicate) 'frame-element)
                                     collect predicate)
          
          when (or (> (length frame-elements) (length (remove-duplicates frame-elements :key #'fourth :test #'equalp)))
                    (loop for fe in frame-elements
                          for other-fes = (remove fe frame-elements :key #'fourth :test #'equalp)
                          thereis (subconstituent-p (fourth fe) (mapcar #'fourth other-fes) (left-pole-structure (car-resulting-cfs (cipn-car node))))))
          do (push frame-var double-role-assignments)
          finally
          return
          (unless double-role-assignments
            t))))

|#
;; Browsing PropBank data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-rolesets-for-framenet-frame (framenet-frame-name)
  (loop for predicate in *pb-data*
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)


(defun all-sentences-annotated-with-roleset (roleset &key (split #'train-split) (corpus *ontonotes-annotations*)) ;;or #'dev-split
  (loop for sentence in (funcall split corpus)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'equalp)
        collect sentence))

;; Retrieve all sentences in training set for a given roleset:
;; (all-sentences-annotated-with-roleset "believe.01")

;; Retrieve all sentences in de development set for a given roleset (for evaluation):
;; (length (all-sentences-annotated-with-roleset "believe.01" :split #'dev-split)) ;;call #'length for checking number


(defun print-propbank-sentences-with-annotation (roleset &key (split #'train-split) (corpus *ontonotes-annotations*))
  "Print the annotation of a given roleset for every sentence of the
split to the output buffer."
  (loop for sentence in (funcall split corpus)
        for sentence-string = (sentence-string sentence)
        for selected-frame = (loop for frame in (propbank-frames sentence)
                                   when (string= (frame-name frame) roleset)
                                   return frame)
        when selected-frame ;;only print if selected roleset is present in sentence
        do (let ((roles-with-indices (loop for role in (frame-roles selected-frame)
                                       collect (cons (role-type role) (role-string role)))))
             (format t "~a ~%" sentence-string)
             (loop for (role-type . role-string) in roles-with-indices
                   do (format t "~a: ~a ~%" role-type role-string)
                   finally (format t "~%")))))


;; (print-propbank-sentences-with-annotation "believe.01")


;; Cleaning the grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-cxn-frequencies (hashed-cxn-inventory list-of-sentences &key (timeout 10))
  "Returns a hash table with as keys the cxns of the cxn-inventory and
as value the frequency of every construction in the application of the
grammar on the list-of-sentences"
  (let ((frequency-table ;;initialization
         (loop with freq-table = (make-hash-table)
               for cxn in (constructions-list hashed-cxn-inventory)
               do (setf (gethash (name cxn) freq-table) 0)
               finally (return freq-table)))
        (nr-of-time-outs 0))

    (loop for sentence in list-of-sentences
          for comprehension-result = (multiple-value-list
                                      (comprehend sentence :cxn-inventory hashed-cxn-inventory :silent t :timeout timeout))
          if (eq 'time-out (first comprehension-result))
          do (incf nr-of-time-outs)
          (format t "x")
          else do (format t ".")
          (loop for cxn in (applied-constructions (second comprehension-result))
                do (incf (gethash (name cxn) frequency-table))))
    
    (values frequency-table nr-of-time-outs)))


(defun sort-cxns-for-outliers (learned-propbank-grammar dev-corpus &key (nr-of-test-sentences 100) (timeout 10) (nr-of-training-sentences nil))
  "Run the learned grammar on a number of sentences of the dev-corpus in order to detect faulty cxns."
  (assert nr-of-training-sentences)
  (let* ((selected-test-sentences (subseq (shuffle dev-corpus) 0 nr-of-test-sentences))
         (test-frequencies-and-nr-of-timeouts
          (multiple-value-list (collect-cxn-frequencies learned-propbank-grammar
                                                        (mapcar #'sentence-string selected-test-sentences)
                                                        :timeout timeout)))
         (cxns-w-score
          (sort
           (loop for cxn in (constructions-list learned-propbank-grammar)
                 for cxn-test-frequency = (gethash (name cxn) (first test-frequencies-and-nr-of-timeouts))
                 when (> cxn-test-frequency 0)
                 collect (cons cxn `(/ ,(float (/ cxn-test-frequency ;;percentage of occurrence in testing
                                                  (- (length selected-test-sentences) (second test-frequencies-and-nr-of-timeouts)))
                                               )
                                       ,(float (/ (attr-val cxn :frequency) ;;percentage of occurrence in training
                                                  nr-of-training-sentences)))))
           #'> :key #'(lambda (cxn-w-score) (abs (eval (cdr cxn-w-score)))))))

    (loop for (cxn . score) in cxns-w-score
          unless (< (abs (eval score)) 0.02)
          do (format t "~a: ~a (~$) ~%" (name cxn) score (abs (eval score))))
    
    cxns-w-score))

(defun clean-grammar (grammar dev-corpus &key (destructive t) (nr-of-test-sentences 100) (timeout 10)
                              (cut-off 3000)) ;;
  (format t "~%>>Grammar size before cleaning: ~a ~%" (size grammar)) 
  (loop with cxn-inventory = (if destructive grammar (copy-object grammar))
        for (cxn . dev/train-ratio) in (sort-cxns-for-outliers cxn-inventory dev-corpus :timeout timeout
                                                               :nr-of-training-sentences (get-data (blackboard grammar) :training-corpus-size)
                                                               :nr-of-test-sentences nr-of-test-sentences)
        if (>= (eval dev/train-ratio) cut-off)
        do (with-disabled-monitor-notifications
             (delete-cxn cxn cxn-inventory :hash-key (attr-val cxn :lemma)))
        else do (return cxn-inventory)))
  
;(clean-grammar *propbank-learned-cxn-inventory* (shuffle *dev-sentences-all*) :nr-of-test-sentences 10 :destructive t )

(defun remove-cxns-under-frequency (grammar cutoff-frequency &key (destructive nil))
  (let ((cxn-inventory (if destructive
                         grammar
                         (copy-object grammar))))
    (loop for cxn in (constructions-list cxn-inventory)
          when (< (attr-val cxn :frequency) cutoff-frequency)
            do (with-disabled-monitor-notifications (delete-cxn cxn cxn-inventory))
            finally (return cxn-inventory))))

#|
(defun clean-grammar (grammar &key
                              (destructive t)
                              (remove-cxns-with-freq-1 nil)
                              (remove-faulty-cnxs nil))
  
  (let ((cxn-inventory (if destructive grammar (copy-object grammar))))
    
    (when remove-faulty-cnxs
      (with-disabled-monitor-notifications
        (remhash '-pron- (constructions-hash-table cxn-inventory))
        (remhash '-pron- (constructions-hash-table (processing-cxn-inventory cxn-inventory)))
        (let ((faulty-of-cxn (find-cxn 'OF\(IN\)-CXN cxn-inventory :hash-key 'OF))
              (faulty-by-cxn (find-cxn 'BY\(IN\)-CXN cxn-inventory :hash-key 'BY)))
          (when faulty-of-cxn
            (delete-cxn faulty-of-cxn cxn-inventory :hash-key 'OF :key #'name))
          (when faulty-by-cxn
            (delete-cxn faulty-by-cxn cxn-inventory :hash-key 'BY :key #'name)))))
  
    (when remove-cxns-with-freq-1
      (loop for cxn in (constructions-list cxn-inventory)
            when (= 1 (attr-val cxn :frequency))
            do (with-disabled-monitor-notifications (delete-cxn cxn cxn-inventory))
            finally return cxn-inventory))
    cxn-inventory))

|#


(defun clean-type-hierarchy (type-hierarchy &key
                                            (remove-edges-with-freq-smaller-than 2.0))
  "Cleans the type hierarchy of a learned grammar by removing edges
that have a weight smaller than a given frequency."
  (let ((edges (fcg::links type-hierarchy)))

    (format t "Edge count before cleaning: ~a ~%" (fcg::nr-of-links type-hierarchy))

    (loop for (n1 n2) in edges
          when (< (fcg::link-weight n1 n2 type-hierarchy)
                  remove-edges-with-freq-smaller-than)
          do (fcg::remove-link n1 n2 type-hierarchy))
    
    (format t "Edge count after cleaning: ~a ~%" (fcg::nr-of-links type-hierarchy))
    type-hierarchy))
   


(defun spacy-benepar-compatible-sentences (list-of-sentences rolesets &key (selected-role-types 'all))
  (remove-if-not #'(lambda (sentence)
                     (and (propbank-frames sentence)
                          (loop for roleset in (or rolesets (all-rolesets sentence))
                                always (spacy-benepar-compatible-annotation sentence roleset :selected-role-types selected-role-types))))
                 list-of-sentences))


;; Comparing Propbank Constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun roleset (cxn)
  "Retrieves the roleset of a cxn from the start of its name."
  (intern (subseq (mkstr (name cxn)) 0 (search "-" (mkstr (name cxn))))))

(defun fcg::equivalent-propbank-construction  (cxn-1 cxn-2)
  "Returns true if cxn-1 and cxn-2 are considered equivalent."
 
  (cond ((eq 'fcg::processing-construction (type-of cxn-1))
         (and (= (length (right-pole-structure cxn-1)) (length (right-pole-structure cxn-2)))
              (eql (roleset cxn-1) (roleset cxn-2))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))))
        ((eq (type-of cxn-1) 'fcg-construction)
         (and (= (length (conditional-part cxn-1)) (length (conditional-part cxn-2)))
              (eql (roleset cxn-1) (roleset cxn-2))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-1)))
                      (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-1)))
                      (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-1)))
                      (remove nil (mapcar #'(lambda (unit)
                                              (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                          (conditional-part cxn-2))))))))


;;;;;;;;

(defun nr-of-cxns-per-type (grammar)
  (let ((groups (group-by (constructions-list grammar) #'(lambda (cxn)
                                                           (attr-val cxn :label)))))
    (loop for group in groups
          collect (cons (first group) (length group)))))


(defun nr-of-cxns-per-type-with-frequency (grammar)
  (let ((groups (group-by (constructions-list grammar)
                          #'identity
                          :test #'(lambda (cxn1 cxn2)
                                    (equal (attr-val cxn1 :label)
                                           (attr-val cxn2 :label))))))
    (loop for group in groups
          collect (cons (attr-val (first group) :label) (sum (mapcar #'(lambda (cxn)
                                                                         (attr-val cxn :frequency)) group))))))


(defun schemata (grammar)
  (mapcar #'schema (constructions-list grammar)))

(defun schema (cxn)
  (cons (name cxn) (attr-val cxn :schema)))

(defun same-schema (cxn grammar &key (same-schema-function #'same-roles-and-realization))
  (loop with cxn-schema = (schema cxn)
        for other-cxn in (constructions-list grammar)
        for other-cxn-schema = (schema other-cxn)
        
        if (and (not (equal cxn other-cxn))
                (funcall same-schema-function (cdr cxn-schema) (cdr other-cxn-schema)))
        collect other-cxn))

(defun same-roles (cxn-schema-1 cxn-schema-2)
  ;; same semantic roles
  (equal (mapcar #'car cxn-schema-1)
         (mapcar #'car cxn-schema-2)))

(defun same-roles-and-realization (cxn-schema-1 cxn-schema-2)
  ;; same semantic roles
  (and (equal (mapcar #'car cxn-schema-1)
              (mapcar #'car cxn-schema-2))
       (equal (mapcar #'cdr (remove 'V cxn-schema-1 :key #'car))
              (mapcar #'cdr (remove 'V cxn-schema-2 :key #'car)))))
        
(defun find-cxns-with-schema (schema grammar &key (same-schema-function #'same-roles-and-realization))
  (loop for cxn in (constructions-list grammar)
        for cxn-schema = (schema cxn)
        if (funcall same-schema-function schema (cdr cxn-schema))
        collect cxn))

(defun print-cxns-with-schema (schema grammar &key (same-schema-function #'same-roles-and-realization))
  (let ((cxns (find-cxns-with-schema schema grammar :same-schema-function same-schema-function)))
    (format t "~%~%~a constructions found with schema ~a.~%~%" (length cxns) schema)
    (loop for cxn in (sort cxns #'> :key #'(lambda (cxn) (attr-val cxn :frequency)))
          unless (< (attr-val cxn :frequency) 2)
          do (format t "~a:~a~%~%" (name cxn) (attr-val cxn :utterance)))))
                                      
                                      

;; (print-cxns-with-schema `((arg0 np) (V . nil) (arg1 np) (arg2  ,(intern "PP(to)"))) *propbank-learned-cxn-inventory*)
;; (print-cxns-with-schema '((arg1 np) (V . nil)) *propbank-learned-cxn-inventory*)

(in-package :fcg)

(defmethod make-html-construction-title ((construction fcg-construction))
  `((span) 
    ,(format nil "~(~a~)" (name construction)) ))
   #| ,@(when (attributes construction)
        `(" "
          ((span :style "white-space:nowrap")
           ,(format nil "(~{~(~a~)~^ ~})" 
                    (loop for x in (attributes construction)
                          when (cdr x)
                          if (floatp (cdr x))
                          collect (format nil "~,2f" (cdr x))
                          else collect (mkstr (cdr x))))))))) |#
