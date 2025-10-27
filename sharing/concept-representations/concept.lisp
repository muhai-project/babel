(in-package :concept-representations)

;; -----------
;; + Concept +
;; -----------

(defclass concept (irl:entity)
  ((id
    :initarg :id :accessor id :initform (make-id "CONCEPT") :type symbol
    :documentation "Id of the concept.")
   (representation 
    :accessor representation))
  (:documentation "Abstract class for representing concepts."))

;; --------------------------------------------------------------------
;; + Concept representation using weighted multivariate distributions +
;; --------------------------------------------------------------------
(defclass weighted-multivariate-distribution-concept (concept)
  ((representation
    :initarg :representation :accessor representation :initform (make-hash-table) :type hash-table))
  (:documentation "Class for multivariate weighted distributions."))

(defmethod create-concept-representation ((entities list) (mode (eql :weighted-multivariate-distribution)) &key (initial-weight 0))
  "Create a concept with a multivariate distribution representation for a list of given entities."
  (let ((representation (loop with example-entity = (first entities)
                              with weighted-distributions = (make-hash-table :test #'eq)
                              for feature-name being the hash-keys of (features example-entity)
                              for average-value = (average-value entities feature-name)
                              ;; create a distribution for each feature
                              for distribution = (make-distribution average-value)
                              ;; create a weighted-distribution for each feature
                              for weighted-distribution = (make-instance 'weighted-distribution
                                                                         :feature-name feature-name
                                                                         :weight-value initial-weight
                                                                         :distribution distribution)
                              do (setf (gethash feature-name weighted-distributions) weighted-distribution)
                              finally (return weighted-distributions))))
    ;; create the concept
    (make-instance 'weighted-multivariate-distribution-concept :representation representation)))

(defmethod get-weighted-distribution ((weighted-multivariate-distribution-concept weighted-multivariate-distribution-concept)
                                      (feature-name symbol))
  "Getter for a specific weighted distribution given a feature-name"
  (gethash feature-name (representation weighted-multivariate-distribution-concept)))

(defmethod get-weighted-distributions ((weighted-multivariate-distribution-concept weighted-multivariate-distribution-concept))
  "Getter for all weighted distributions."
  (hash-values (representation weighted-multivariate-distribution-concept)))

(defmethod calculate-sum-of-weights ((weighted-multivariate-distribution-concept weighted-multivariate-distribution-concept))
  "Calculates the sum of all weights of the concept."
  (loop for weighted-distribution in (get-weighted-distributions weighted-multivariate-distribution-concept)
        sum (weight weighted-distribution)))

;; --------------------
;; + Helper functions +
;; --------------------

(defmethod copy-object ((concept weighted-multivariate-distribution-concept))
  "Returns a copy of the concept."
  (make-instance 'weighted-multivariate-distribution-concept
                 :id (id concept)
                 :representation (copy-object (representation concept))))

(defmethod print-object ((concept weighted-multivariate-distribution-concept) stream)
  "Prints the concept."
  (pprint-logical-block (stream nil)
    (format stream "<Concept: [~{~a~^, ~}]>" (hash-values (representation concept)))))

(defun average-value (entities feature-name)
  (let ((exemplar (gethash feature-name (features (first entities)))))
    (typecase exemplar
      ;; if its a symbol, then just return the first
      (symbol (gethash feature-name (features (first entities))))
      ;; if its a number, average over entities
      (number (loop for entity in entities and count from 1
                     for feature-value = (gethash feature-name (features entity))
                     sum feature-value into total
                     finally (return (/ total count)))))))

;; --------------------------
;; + Weighted Distributions +
;; --------------------------

(defclass weighted-distribution ()
  ((feature-name
    :initarg :feature-name :accessor feature-name :initform nil :type symbol)
   (weight-value
    :initarg :weight-value :accessor weight-value :initform nil :type number)
   (distribution
    :initarg :distribution :accessor distribution :initform nil :type distribution))
  (:documentation "A weighted distribution is a weighted mapping between a feature name and a distribution."))

(defmethod weight ((weighted-distribution weighted-distribution))
  "Calculcates the true weight by mapping the weight-value through to the sigmoid function."
  (sigmoid (weight-value weighted-distribution)))

(defun sigmoid (x &key (c 0.5) (limit 35.0))
  "Sigmoid function where c changes the slope of the function. 

   When c is a fraction the slope is less steep, when c is a larger the slope is steeper."
  (let* ((z (* c x))
         (z (max (- limit) (min limit z)))) ;; improves numerical stability
    (/ 1.0 (+ 1.0 (exp (- z))))))

(defmethod update-weight (weighted-distribution reward)
  "Update the step (weight-value) of a given weighted distribution with a given reward."
  (let* ((old-value (weight-value weighted-distribution)))
    (setf (weight-value weighted-distribution) (+ old-value reward))))

;; --------------------
;; + Helper functions +
;; --------------------
(defmethod copy-object ((weighted-distribution weighted-distribution))
  (make-instance 'weighted-distribution
                 :feature-name (feature-name weighted-distribution)
                 :weight-value (copy-object (weight-value weighted-distribution))
                 :distribution (copy-object (distribution weighted-distribution))))

(defmethod print-object ((weighted-distribution weighted-distribution) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Weighted distribution:~
                        ~:_ feature-name: ~a,~:_ weight ~,2f,~:_ type: ~a~:_>"
            (feature-name weighted-distribution) (weight weighted-distribution) (type-of (distribution weighted-distribution)))))
