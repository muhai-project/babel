;;;; /lexicon.lisp

(in-package :roos)

;; -----------
;; + Lexicon +
;; -----------
                              
(defun add-lex-cxn (agent form meaning &key (score 0.5))
  "Add a form-meaning pair to the agent's lexicon"
  (let* ((cxn-name (make-symbol (string-append form "-cxn")))
         (unit-name (make-var (string-append form "-unit")))
         (current-interaction (current-interaction (experiment agent)))
         (i-number (if current-interaction (interaction-number current-interaction) 0))
         (new-var (make-var 'x)))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (referent ,new-var)
                          (args (,new-var))
                          (unit-type word)
                          (syn-cat (lex-class ,(intern (symbol-name (make-const (string-append form "-cat"))) :type-hierarchies))))
                         <-
                         (,unit-name
                          (HASH meaning (,(category->predicate meaning new-var)))
                          --
                          (HASH form ((string ,unit-name ,form)))))
                        :cxn-set lex
                        :cxn-inventory ',(grammar agent)
                        :attributes (:score ,score
                                     :form ,form
                                     :meaning ,(id meaning)
                                     :added ,i-number
                                     :cxn-type lexical-cxn
                                     :apply-sequentially t)))
      (declare (ignorable cxn-set))
      cxn)))                           
                           

(defmethod find-cxn-by-meaning (meaning agent (mode (eql :highest-score)))
  "Find cxn by meaning, return the one with highest score"
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning))))
    (extremum (find-all meaning-id
                        (constructions (grammar agent))
                        :key (lambda (cxn) (attr-val cxn :meaning)))
              :key (lambda (cxn) (attr-val cxn :score)))))

(defmethod find-cxn-by-meaning (meaning agent (mode (eql :random)))
  "Find cxn by meaning, return a random one"
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning))))
    (random-elt (find-all meaning-id
                          (constructions (grammar agent))
                          :key (lambda (cxn) (attr-val cxn :meaning))))))

(defmethod find-cxn-by-meaning (meaning agent (mode (eql :all)))
  "Find cxn by meaning, return all found"
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning))))
    (find-all meaning-id
              (constructions (grammar agent))
              :key (lambda (cxn) (attr-val cxn :meaning)))))

(defmethod find-cxn-by-form (form agent (mode (eql :highest-score)))
  (extremum (find-all form
                      (constructions (grammar agent))
                      :key (lambda (cxn) (attr-val cxn :form))
                      :test #'string=)
            :key (lambda (cxn) (attr-val cxn :score))))

(defmethod find-cxn-by-form (form agent (mode (eql :random)))
  (random-elt (find-all form
                        (constructions (grammar agent))
                        :key (lambda (cxn) (attr-val cxn :form))
                        :test #'string=)))

(defmethod find-cxn-by-form (form agent (mode (eql :all)))
  (find-all form
            (constructions (grammar agent))
            :key (lambda (cxn) (attr-val cxn :form))
            :test #'string=))

(defmethod find-cxn-by-form-and-meaning (form meaning agent (mode (eql :highest-score)))
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning)))
        (all-constructions (constructions (grammar agent))))
    (extremum (find-all form
                        (find-all meaning-id
                                  all-constructions
                                  :key (lambda (cxn) (attr-val cxn :meaning)))
                        :key (lambda (cxn) (attr-val cxn :form))
                        :test #'string=)
              :key (lambda (cxn) (attr-val cxn :score)))))

(defmethod find-cxn-by-form-and-meaning (form meaning agent (mode (eql :random)))
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning)))
        (all-constructions (constructions (grammar agent))))
    (random-elt (find-all form
                        (find-all meaning-id
                                  all-constructions
                                  :key (lambda (cxn) (attr-val cxn :meaning)))
                        :key (lambda (cxn) (attr-val cxn :form))
                        :test #'string=))))

(defmethod find-cxn-by-form-and-meaning (form meaning agent (mode (eql :all)))
  (let ((meaning-id (if (symbolp meaning) meaning (id meaning)))
        (all-constructions (constructions (grammar agent))))
    (find-all form
              (find-all meaning-id
                        all-constructions
                        :key (lambda (cxn) (attr-val cxn :meaning)))
              :key (lambda (cxn) (attr-val cxn :form))
              :test #'string=)))

(defun inc-score (cxn &key delta (upper-bound 1.0))
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-score (cxn agent &key delta (lower-bound 0.0) 
                                 (remove-on-lower-bound t))
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
        (delete-cxn cxn (grammar agent))
        (setf (attr-val cxn :score) lower-bound)))
  cxn)

(defun get-form-competitors (agent cxn)
  "Get all form competitors"
  (let* ((cxn-form (attr-val cxn :form))
         (cxn-meaning (attr-val cxn :meaning))
         (cxns-w-form (find-cxn-by-form cxn-form agent :all)))
    (remove-if (lambda (cxn)
                 (eql cxn-meaning (attr-val cxn :meaning)))
               cxns-w-form)))

(defun get-meaning-competitors (agent cxn)
  "Get all meaning competitors"
  (let* ((cxn-form (attr-val cxn :form))
         (cxn-meaning (attr-val cxn :meaning))
         (cxns-w-meaning (find-cxn-by-meaning cxn-meaning agent :all)))
    (remove-if (lambda (cxn)
                 (string= cxn-form (attr-val cxn :form)))
               cxns-w-meaning)))

(defun dec-competitor-score (applied-cxn agent &key delta)
  (let ((form-competitors (get-form-competitors agent applied-cxn))
        (meaning-competitors (get-meaning-competitors agent applied-cxn))
        (li-dec (if (not (null delta)) delta (get-configuration agent :li-dec))))
    (loop for cxn in form-competitors
          do (dec-score cxn agent :delta li-dec))
    (loop for cxn in meaning-competitors
          do (dec-score cxn agent :delta li-dec))
    (append form-competitors meaning-competitors)))