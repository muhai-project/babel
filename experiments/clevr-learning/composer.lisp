;;;; composer.lisp

(in-package :clevr-learning)

;; ------------
;; + Composer +
;; ------------

(defparameter *max-irl-program-length-per-challenge-level*
  '((1 . 8) (2 . 25) (3 . 25)))

(defun make-default-composer (agent target-category
                              &key partial-program
                              max-program-length)
  (let* ((current-challenge-level
          (get-configuration agent :current-challenge-level))
         (max-irl-program-length
          (if max-program-length max-program-length
            (rest (assoc current-challenge-level
                         *max-irl-program-length-per-challenge-level*))))
         (target-category-type
          #+lispworks (type-of target-category)
          #-lispworks (if (numberp target-category)
                          'integer (type-of target-category))
          )
         ;; extract bind statements and other primitives
         ;; from the partial program
         (partial-program-bindings
          (find-all 'bind partial-program :key #'first))
         (partial-program-primitives
          (set-difference partial-program partial-program-bindings))
         ;; make a list of open vars
         (open-vars
          (append `((?answer . ,target-category-type))
                  (when partial-program-primitives
                    (loop for var in (get-open-vars partial-program-primitives)
                          for binding? = (find var partial-program-bindings :key #'third)
                          if binding? collect (cons var (second binding?))
                          else collect (cons var 'category)))))
         ;; initial chunk
         (initial-chunk
          (make-instance 'chunk :id 'initial
                         :target-var `(?answer . ,target-category-type)
                         :open-vars open-vars
                         :irl-program (when partial-program-primitives
                                        partial-program-primitives)))
         ;; when partial bindings available, add
         ;; :check-bindings to the check chunk
         ;; evaluation result modes
         (check-chunk-evaluation-result-modes
          (when partial-program-bindings
            '(:check-bindings)))
         ;; make the chunk composer
         (composer
          (make-chunk-composer
           :topic target-category
           :meaning partial-program-primitives
           :initial-chunk initial-chunk
           :chunks (composer-chunks agent)
           :ontology (ontology agent)
           :primitive-inventory (available-primitives agent)
           :configurations `((:max-irl-program-length . ,max-irl-program-length)
                             (:check-node-modes ;; limit the length of the irl program
                                                :limit-irl-program-length
                                                
                                                ;; no duplicates
                                                :check-duplicate
                                                
                                                ;; no predicates with multiple times
                                                ;; the same variable
                                                :no-circular-primitives
                                                
                                                ;; meaning has to be fully connected
                                                :fully-connected-meaning
                                                
                                                ;; limit on the nr of times each primitive
                                                ;; can occur (clevr specific)
                                                :clevr-primitive-occurrence-count
                                                
                                                ;; limit to which primitives get-context
                                                ;; can connect (clevr specific)
                                                :clevr-context-links
                                                
                                                ;; the last variable of certain predicates
                                                ;; has to be an open variable (clevr specific)
                                                :clevr-open-vars
                                                
                                                ;; a filter group can be maximally 4 long
                                                ;; (clevr specific)
                                                :clevr-filter-group-length)
                             ;; default expand mode
                             (:expand-chunk-modes :combine-program)
                             ;; prefer less depth, less open vars
                             ;; less duplicate primitives and prefer
                             ;; chain-type over tree-type programs
                             (:node-rating-mode . :clevr-node-rating)
                             ;; remove unwanted chunk eval results
                             (:check-chunk-evaluation-result-modes
                              ,@check-chunk-evaluation-result-modes))
           ;; some extreme optimisations that are CLEVR specific
           ;; also, they will not work for level 3 questions!
           :primitive-inventory-configurations '((:node-tests :remove-clevr-incoherent-filter-groups
                                                  :remove-clevr-filter-permutations))
           )))
    ;; when partial bindings, add them to the composer's
    ;; blackboard because we cannot access them otherwise
    ;; in the check chunk evaluation result mode
    (when partial-program-bindings
      (set-data composer 'irl::partial-bindings partial-program-bindings))
    composer))

;; + compose-until +
(defun compose-until (composer fn &key (max-attempts nil))
  "Generate composer solutions until the
   function 'fn' returns t on the solution
   or the composer runs out of solutions"
  (loop with the-solution = nil
        for attempt from 1
        for next-solutions = (get-next-solutions composer)
        do (loop for solution in next-solutions
                 for i from 1
                 when (funcall fn solution i)
                 do (progn (setf the-solution solution) (return)))
        until (or (null next-solutions)
                  (not (null the-solution))
                  (and max-attempts (> attempt max-attempts)))
        finally (return the-solution)))

(defun compose-minimize (composer fn)
  "Run 'fn' on the composer solutions and choose
   the one that minimizes the result. If there is
   no result, compose the next solution."
  (loop with the-solution = nil
        for next-solutions = (get-next-solutions composer)
        do (loop with minimum-solution = nil
                 with minimum-value = nil
                 for solution in next-solutions
                 for i from 1
                 for result = (funcall fn solution i)
                 when (and result (or (null minimum-solution) (< result minimum-value)))
                 do (setf minimum-solution solution)
                 (setf minimum-value result)
                 finally (setf the-solution minimum-solution))
        until (or (null next-solutions)
                  (not (null the-solution)))
        finally (return the-solution)))
                

;; + store past scenes +
(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))

(defun check-past-scene (scene-index answer irl-program agent)
  (let* ((world (world (experiment agent)))
         (path (nth scene-index (scenes world)))
         (scene (load-clevr-scene path))
         (image-filename (file-namestring (image scene)))
         (server-address
          (find-data (ontology agent) 'hybrid-primitives::server-address))
         (cookie-jar
          (find-data (ontology agent) 'hybrid-primitives::cookie-jar)))
    (set-data (ontology agent) 'clevr-context scene)
    (when (eql (get-configuration agent :primitives) :hybrid)
      (load-image server-address cookie-jar image-filename))
    (let* ((all-solutions
            (evaluate-irl-program irl-program (ontology agent) :silent t
                                  :primitive-inventory (available-primitives agent)))
           (solution
            (when (length= all-solutions 1)
              (first all-solutions)))
           (found-answer
            (when solution
              (get-target-value irl-program solution))))
      (equal-entity found-answer answer))))

(defun check-past-scenes (solution solution-index list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let* ((current-clevr-context (find-data (ontology agent) 'clevr-context))
         (irl-program (append (irl-program (chunk solution))
                              (bind-statements solution)))
         (success t))
    (notify check-samples-started list-of-samples solution-index)
    ;; check the list of samples
    (setf success
          (loop for (scene-index . stored-answer) in list-of-samples
                always (check-past-scene scene-index stored-answer irl-program agent)))
    ;; afterwards, restore the data for the current scene
    (set-data (ontology agent) 'clevr-context current-clevr-context)
    success))

(defun hybrid-check-past-scenes (solution solution-index list-of-samples agent)
  (let* ((original-clevr-context
          (find-data (ontology agent) 'clevr-context))
         (original-image-filename
          (file-namestring (image original-clevr-context)))
         (irl-program
          (append (irl-program (chunk solution))
                  (bind-statements solution)))
         (server-address
          (find-data (ontology agent) 'hybrid-primitives::server-address))
         (cookie-jar
          (find-data (ontology agent) 'hybrid-primitives::cookie-jar))
         (max-failed-past-scenes
          (get-configuration agent :hybrid-check-past-scenes-errors-allowed))
         (actual-limit
          (if (> (length list-of-samples) max-failed-past-scenes)
            max-failed-past-scenes
            0))
         (failed-past-scenes 0))
    (notify check-samples-started list-of-samples solution-index)
    (loop for (scene-index . stored-answer) in list-of-samples
          for scene-ok? = (check-past-scene scene-index stored-answer irl-program agent)
          unless scene-ok?
          do (incf failed-past-scenes)
          when (> failed-past-scenes actual-limit)
          do (setf failed-past-scenes nil) (return nil))
    ;; afterwards, restore the data for the current scene
    (set-data (ontology agent) 'clevr-context original-clevr-context)
    (load-image server-address cookie-jar original-image-filename)
    failed-past-scenes))
  

(define-event composer-solution-found (composer irl::chunk-composer)
  (solution chunk-evaluation-result))

(defmethod compose-program ((agent clevr-learning-learner)
                            target-category utterance
                            (strategy (eql :store-past-scenes))
                            &key partial-program)
  (let* ((composer
          (make-default-composer agent target-category
                                 :partial-program partial-program))
         (utterance-hash-key
          (sxhash utterance))
         (past-scenes-with-same-utterance
          (gethash utterance-hash-key (memory agent)))
         (composer-solution
          (if past-scenes-with-same-utterance
            (if (eql (get-configuration agent :primitives) :hybrid)    
              (compose-minimize
               composer (lambda (solution idx)
                          (hybrid-check-past-scenes
                           solution idx
                           past-scenes-with-same-utterance
                           agent)))
              (compose-until
               composer (lambda (solution idx)
                          (check-past-scenes
                           solution idx
                           past-scenes-with-same-utterance
                           agent))))
            (first (get-next-solutions composer)))))
    (when composer-solution
      (notify composer-solution-found composer composer-solution))
    composer-solution))
