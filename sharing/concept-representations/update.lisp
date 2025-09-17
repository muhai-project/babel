(in-package :concept-representations)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod update-concept ((concept weighted-multivariate-distribution-concept) (entity entity) (context list) &key (weight-incf 1) (weight-decf -1))
  "Update a concept based on a new observed entity.

  Updates 1. each distribution
          2. the weights of distributions positively and others negatively."
  ;; 1. update the prototypical values
  (loop for weighted-distribution in (get-weighted-distributions concept)
        for distribution = (distribution weighted-distribution)
        for feature-name = (feature-name weighted-distribution)
        for feature-value = (get-feature-value entity feature-name)
        do (update-distribution distribution feature-value))
  
  ;; 2. determine which features weights should get an increase
  ;;    and which should get a decrease.
  (let* ((discriminating-features (find-discriminating-features concept entity context)))

    ;; when no subset is found, use all weighted-distributions 
    (when (null discriminating-features)
      (setf discriminating-features (get-weighted-distributions concept)))

    ;; 3. actually update the weight scores
    (loop for weighted-distribution in (get-weighted-distributions concept)
          for feature-name = (feature-name weighted-distribution)
          ;; if part of the contributing weighted-distributions -> reward
          if (member feature-name discriminating-features)
            do (update-weight weighted-distribution weight-incf)
            ;; otherwise -> punish
          else
            do (update-weight weighted-distribution weight-decf))))


(defmethod find-discriminating-features ((concept weighted-multivariate-distribution-concept) (entity entity) (context list))
  "Finds all features that are discriminating for the given entity in the context."
  (loop with other-entities = (remove entity context)
        with discriminating-features = nil
        for weighted-distribution in (get-weighted-distributions concept)
        for feature-name = (feature-name weighted-distribution)
        for distribution = (distribution weighted-distribution)
        for topic-similarity = (distribution-feature-similarity distribution (get-feature-value entity feature-name))
        for best-other-similarity = (loop for other-entity in other-entities
                                          maximize (distribution-feature-similarity distribution (get-feature-value other-entity feature-name)))
        when (> topic-similarity best-other-similarity)
          do (push feature-name discriminating-features)
        finally (return discriminating-features)))
