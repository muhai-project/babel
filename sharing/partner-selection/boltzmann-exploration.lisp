(in-package :partner-selection)

;; sub-class your agent class with the 'neighbour-q-values'-class

;; ------------
;; + Q-values +
;; ------------

(defclass neighbour-q-values ()
  ((neighbour-q-values
    :documentation "Stores the q-values associated to its neighbours."
    :type hash-table :accessor neighbour-q-values :initform (make-hash-table))))

;; -------------------------
;; + Boltzmann Exploration +
;; -------------------------

(defmethod choose-partner (agent (neighbours list) (tau number))
  "Choose a partner for a given agent using Boltzmann exploration."

  (let* ((tuples (loop for neighbours in neighbours
                       collect (cons (id neighbours)
                                     (gethash (id neighbours) (neighbour-q-values agent)))))
         (neighbours-ids (mapcar #'car tuples))
         (q-values (mapcar #'cdr tuples))
         (probabilities (boltzmann-exploration q-values tau))
         ;; sample a partner (by id)
         (partner-id (sample-partner neighbours-ids probabilities))
         ;; match id to the other agent
         (partner (find partner-id neighbours :test (lambda (x y) (eq x (id y))))))

    partner))

(defun boltzmann-exploration (q-values tau)
  "Boltzmann exploration for partner selection.

   tau corresponds to an inverse temperature:
     if tau = 0: no preference, random sampling
     if tau > 0: preference to select partners you understand well
     if tau < 0: curiosity-driven partner selection

    Inspired by Leung's et al. paper on curiosity-driven partner selection (2025)."
  (let* ((exp-values (mapcar (lambda (q) (exp (* tau q))) q-values))
         (total-sum (reduce #'+ exp-values)))
    (mapcar (lambda (exp-value) (/ exp-value total-sum)) exp-values)))

(defun sample-partner (neighbours probabilities)
  "Randomly sample a partner from the neighbours using the given probabilities."
  (loop with r = (random 1.0)
        with cumulative = 0.0
        for neighbours in neighbours
        for probability in probabilities
        do (setf cumulative (+ cumulative probability))
        when (<= r cumulative)
          return neighbours
        ;; the sum of probabilities can be smaller to 1 (due to some numerical stability)
        ;; if the sampled point r is bigger than the sum, no decision will have been made
        ;; this remaining probability mass is associated to last agent
        ;; we thus make a decision and pick the last agent
        finally (return (car (last neighbours)))))

;; ------------------------------
;; + Initialisations + updating +
;; ------------------------------

(defmethod initialise-neighbours-q-values (agent neighbours &key (default-q 0.5))
  "Initialise a q-value for a given agent's neighbours."
  (let ((q-values (neighbour-q-values agent)))
    (when (not (gethash (id neighbours) q-values))
      (setf (gethash (id neighbours) q-values) default-q))))

(defmethod update-neighbour-q-value (agent neighbour (reward number) (lr float))
  "Update the partner selection q-value."
  (let* ((q-values (neighbour-q-values agent))
         (q-old (gethash (id neighbour) q-values))
         (q-new (calculate-new-q-value q-old reward lr)))
    (setf (gethash (id neighbour) q-values) q-new)))

(defun calculate-new-q-value (q-value reward lr)
  (+ q-value (* lr (- reward q-value))))