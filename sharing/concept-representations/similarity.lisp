(in-package :concept-representations)

;; --------------------------------
;; + Comparing CONCEPT <-> ENTITY +
;; --------------------------------

(defmethod concept-entity-similarity ((concept weighted-multivariate-distribution-concept) (entity entity))
  (loop with sum-of-weights = (calculate-sum-of-weights concept)
        for weighted-distribution in (get-weighted-distributions concept)
        for weight = (weight weighted-distribution)
        for distribution = (distribution weighted-distribution)
        for feature-value = (get-feature-value entity (feature-name weighted-distribution))
        for similarity = (distribution-feature-similarity distribution feature-value)
        if (and similarity (not (zerop sum-of-weights)))
          sum (* (/ weight sum-of-weights) similarity) into total-similarity
        finally (return total-similarity)))

;; --------------------------------------
;; + Comparing FEATURE <-> DISTRIBUTION +
;; --------------------------------------

(defgeneric distribution-feature-similarity (distribution feature-value &key &allow-other-keys)
  (:documentation "Measures the similarity between an feature-value and a distribution."))

(defmethod distribution-feature-similarity ((distribution gaussian) (feature-value number) &key &allow-other-keys)
  "Measures the similarity between a feature value (a number) and a gaussian distribution."
  (let* ((mean (mean distribution))
         (st-dev (st-dev distribution))
         (z-score (if (not (zerop st-dev))
                    (/ (- feature-value mean) st-dev)
                    0)))
    (exp (- (abs z-score)))))

(defmethod distribution-feature-similarity ((distribution categorical) (feature-value string) &key (laplace-smoother 1) &allow-other-keys)
  "Measures the similarity between a feature value (a category) and a categorical distribution."
  (let* ((total (nr-of-samples distribution))
         (frequency (gethash feature-value (frequencies distribution)))
         ;; additive smoothing to avoid categories with 0 occurences
         (probability
          (if frequency
            (/ (+ frequency laplace-smoother)
               (+ total (* laplace-smoother (number-of-categories distribution))))
            (/ laplace-smoother
               (+ total (* laplace-smoother (+ (number-of-categories distribution) 1)))))))
    probability))

;; ---------------------------------
;; + Comparing CONCEPT <-> CONCEPT +
;; ---------------------------------
(defmethod concept-similarity ((concept1 weighted-multivariate-distribution-concept) (concept2 weighted-multivariate-distribution-concept))
  "Compute the similarity between two concepts.
   
   The overall similarity is computed by summing the weighted similarity between
    all pairs of weighted distributions from the two concepts."
  (loop with sum-of-weights1 = (calculate-sum-of-weights concept1)
        with sum-of-weights2 = (calculate-sum-of-weights concept2)
        for wd1 in (get-weighted-distributions concept1)
        for wd2 = (get-weighted-distribution concept2 (feature-name wd1))
        if (and (not (zerop sum-of-weights1)) (not (zerop sum-of-weights2)))
          sum (weighted-distribution-similarity wd1 wd2 sum-of-weights1 sum-of-weights2)))

(defmethod weighted-distribution-similarity ((wd1 weighted-distribution) (wd2 weighted-distribution) (sum-of-weights1 number) (sum-of-weights2 number))
  "Calculates the similarity between two weighted distributions.
   
   The similarity corresponds to a product t-norm of
    1. the average of the normalised weight
    2. the similarity of the normalised weights
    3. the complement of the f-divergence between the distributions (i.e. the similarity)"
  (let (;; take the average weight
        (average-weight (/ (+ (/ (weight wd1) sum-of-weights1)
                              (/ (weight wd2) sum-of-weights2))
                           2))
        ;; similarity of the weights
        (weight-similarity (- 1 (abs (- (/ (weight wd1) sum-of-weights1) (/ (weight wd2) sum-of-weights2)))))
        ;; take complement of distance (1-f) so that it becomes a similarity metric
        (distribution-similarity (- 1 (f-divergence (distribution wd1) (distribution wd2)))))
    ;; multiple all three
    (* average-weight weight-similarity distribution-similarity)))
