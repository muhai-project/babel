;;;; grammar.lisp

(in-package :fcg)

(defun detach-punctuation (word)
  "This function will check if the input string (word)
   has a punctuation at the end of it (e.g. it?)
   and return a list of the word + the punctuation mark
   (e.g. '('it' '?')"
  (let ((last-char (char word (1- (length word)))))
    (if (punctuation-p last-char)
      (if (eq last-char #\?)
        (list (subseq word 0 (1- (length word))))
        (list (subseq word 0 (1- (length word)))
              (subseq word (1- (length word)))))
      (list word))))

(defun tokenize (utterance)
  "Split the utterance in words, downcase every word,
   remove the punctuation from the word"
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          append (detach-punctuation (downcase word)))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render (tokenize utterance) :de-render-string-meets-no-punct))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render utterance :de-render-string-meets))

(in-package :clevr-learning)

(defun empty-cxn-set (hide-type-hierarchy)
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions-with-type-hierarchy
                     ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:cxn-supplier-mode . :ordered-by-label-and-score) ;:hashed-scored-labeled 
                                        (:parse-order lexical item-based holophrase)
                                        (:parse-goal-tests :no-applicable-cxns
                                                           :connected-semantic-network
                                                           :no-strings-in-root)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :neighbours) ;:path-exists)
                                        (:update-th-links . t)
                                        ;(:hash-mode . :hash-string-meaning-lex-id)
                                        )
                   ;:hashed t
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-categorial-network . ,(not hide-type-hierarchy)))))))
    cxn-inventory))

(define-event lexicon-changed)

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "decrease the score of the cxn.
   remove it when it reaches 0"
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (progn (notify lexicon-changed)
        (with-disabled-monitor-notifications
          (delete-cxn-and-th-node cxn (grammar agent))))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun delete-cxn-and-th-node (cxn cxn-inventory)
  (let ((lex-class
         (loop for unit in (contributing-part cxn)
               for lex-class = (gl::lex-class-item-based unit)
               when lex-class return lex-class))
        (type-hierarchy (get-type-hierarchy cxn-inventory)))
    (delete-cxn cxn cxn-inventory)
    (notify lexicon-changed)
    (when lex-class
      (delete-category lex-class type-hierarchy))))

;;;;  COMPETITORS
;;;; -------------

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase)))
  ;; holophrase competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'lexical)))
  ;; lexical competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based)))
  ;; item-based cxns have no meaning competitors
  nil)


(defun combined-meaning-competitors (agent applied-cxns)
  ;; the current set of applied cxns might have some less
  ;; general alternatives, e.g. an item-based with fewer
  ;; slots or a holophrase cxn. These can be punished as
  ;; well. We find them through comprehend-all with a
  ;; simple queue as cxn supplier
  (set-configuration (grammar agent) :cxn-supplier-mode
                     :simple-queue :replace t)
  (multiple-value-bind (meanings cipns)
      (comprehend-all (utterance agent)
                      :cxn-inventory (grammar agent)
                      :silent t)
    (declare (ignorable meanings))
    (set-configuration (grammar agent) :cxn-supplier-mode
                       :ordered-by-label-and-score :replace t)
    (when (length> cipns 1)
      (remove-duplicates
       (loop for cipn in cipns
            for cipn-applied-cxns = (applied-constructions cipn)
            unless (permutation-of? applied-cxns cipn-applied-cxns
                                    :key #'name :test #'eql)
            append (loop for cxn in cipn-applied-cxns
                         unless (eql (get-cxn-type cxn) 'lexical)
                         collect (get-original-cxn cxn)))))))      

(defun get-meaning-competitors (agent applied-cxns)
  "Get cxns with the same form as cxn"
  (append
   ;; get competitors for each construction separately
   (loop for cxn in applied-cxns
         for cxn-type = (get-cxn-type cxn)
         for competitors = (meaning-competitors-for-cxn-type
                            cxn (grammar agent) cxn-type)
         append competitors)
   ;; get competitors for the combined applied cxns
   ;; (item-based + lexical might have a holophrase competitor
   ;;  or some item-based + lexical that is less general)
   (when (length> applied-cxns 1)
     (combined-meaning-competitors agent applied-cxns))))
