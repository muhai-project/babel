(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;
;; Recipe Solutions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass solution ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (smatch-score :accessor smatch-score :initform '())
   (subgoals-ratio :accessor subgoals-ratio :initform '())
   (dish-score :accessor dish-score :initform '())
   (execution-time :accessor execution-time :initform '()))
  (:documentation "Class used for storing a recipe solution and its score.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading/Writing Solutions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-solutions-file (filepath)
  "Read all recipe solutions from the file at the given filepath. 
   Solutions are represented by a line containing the ID of a recipe, i.e., #recipe_ID, 
   followed by a sequence of lines each containing a primitive operation from the meaning network for the aforementiond recipe."
  (with-open-file (stream filepath)
    (let ((solutions '()))
      (loop for line = (read-line stream nil)
            while line
            do
              (cond ((not (find #\space line :test-not #'eql))
                     ; (print "Empty line is skipped")
                     nil)
                    ((char= (char line 0) #\;)
                     ; (print "Comment line is skipped")
                     nil)
                    ((char= (char line 0) #\#)
                     ; first check the previous solution
                     (when solutions
                       (multiple-value-bind (error-status messages) (check-recipe-program (meaning-network (first solutions)) *irl-primitives*)
                         (unless error-status
                           (error "Invalid IRL program in solution ~S. Error was thrown: ~a" (recipe-id (first solutions)) (format nil "~{~a~}" messages)))))
                       ; we are starting a new solution
                     (push (make-instance 'solution :recipe-id (read-from-string (subseq line 1))) solutions))
                     ; we are adding a new primitive operation to the current solution's meaning network
                    ((char= (char line 0) #\()
                     (let ((current-solution (first solutions)))
                       (unless current-solution
                         (error "The file should start with a Recipe ID (#recipe-id)!"))
                       (setf (meaning-network current-solution) (nconc (meaning-network current-solution) (list (read-from-string line))))))
                    (t
                     (error "A line should either contain a recipe ID (#recipe-id) or a primitive operation (op ?a ?b), but ~S was found" line))))
              (when solutions
                (multiple-value-bind (error-status messages) (check-recipe-program (meaning-network (first solutions)) *irl-primitives*)
                  (unless error-status
                    (error "Invalid IRL program in solution ~S. Error was thrown: ~a" (recipe-id (first solutions)) (format nil "~{~a~}" messages)))))
              solutions)))

(defun check-solutions (solutions gold-standard-solutions)
  "Check if the given solutions don't contain duplicate solutions or solutions that are unavailable in the simulation environment."
  (loop for solution in solutions
        when (not (find (recipe-id solution) gold-standard-solutions :key #'(lambda (sim-env) (recipe-id sim-env))))
          do (error "Solution contains recipe ~S which is currently unsupported" (recipe-id solution))
        when (> (count (recipe-id solution) solutions :key #'(lambda (sol) (recipe-id sol))) 1)
          do (error "Duplicate entry found for recipe ~S" (recipe-id solution))))

(defun check-recipe-program (irl-program primitive-inventory)
  "Checks a recipe irl-program for mistakes.
   This function is based on the check-irl-program function from the IRL package,
   with unnecessary checks being removed and additional checks being added."
  (let ((messages '()))
    
    ;; first check, everything should be a non-empty list
    (loop for expr in irl-program
          unless (and (listp expr) expr)
          do (push (format nil "The expression should be a non-empty list, got: ~a." expr) messages))

    ;; then check, the irl-program should contain exactly one get-kitchen    
    (let ((get-kitchen-count (count 'get-kitchen (mapcar #'first irl-program))))
      (unless (= get-kitchen-count 1)
        (push (format nil "The recipe should contain exactly one get-kitchen operation, but ~d were found" get-kitchen-count) messages)))
                                                         
    ;; lastly we check all primitives
    (loop for expr in irl-program
          for variables = (cdr expr)
          unless (= (length variables) (length (remove-duplicates variables)))
            do (push (format nil "In ~a variables appear at least twice as argument." expr) messages)
          ;; primitive must be found
          unless (find-primitive (first expr) primitive-inventory)
            do (push (format nil "Primitive ~a is not defined " (car expr)) messages)
          do
          (let ((primitive (find-primitive (first expr) primitive-inventory)))            
            ;; check that the number of variables matches the
            ;; number of slot-specs:
            (unless (= (length (irl::slot-specs primitive))
                       (length variables))
              (push (format nil "Error while reading primitive expression~%  ~a.~
                             ~%The number of given variables does not match ~
                             the number of slots."
                             expr) messages)
            ;; check that the given parameters are proper variable identifiers:
            (loop for var in variables
                  unless (or (variable-p var) ; regular variable
                             (numberp var) ; quantity
                             (listp var) ; list-of-kitchen-entities
                             (find-class var)) ; concepts
                  do (push (format nil "Error while reading primitive expression ~a.~
                                    ~%Expected a variable identifier, number, list or an ontology class but got ~a."
                                    expr var) messages)))))
    ;; all test succeeded, return t
    (if messages
      (values nil messages)
      (values t messages))))

(defun write-solutions-to-csv (solutions filepath metrics)
    "Write away the given solutions to a csv file with the specified filepath."

     ; check if it is a csv filepath
    (unless (uiop:string-suffix-p filepath ".csv")
      (print "Warning: Specified file is not a CSV file"))

    ; remove unsupported metrics
    (setf metrics (intersection (append (list 'none) *all-metrics*) metrics))
    
    ; create all directories in the specified path if they do not exist yet
    (ensure-directories-exist filepath)

    (let* ((output-stream (open filepath
                               :if-does-not-exist :create
                               :direction :output
                               :if-exists :supersede)))

      ; add header row
      (format output-stream "recipe-id")
      (when (find 'smatch-score metrics)
        (format output-stream ",smatch-score"))
      (when (find 'goal-condition-success metrics)
        (format output-stream ",goal-condition-success"))
      (when (find 'dish-approximation-score metrics)
        (format output-stream ",dish-approximation-score"))
      (when (find 'execution-time metrics)
        (format output-stream ",execution-time"))
      (format output-stream "~%")

      (dolist (solution solutions)
        (let ((metrics-left metrics)) ; in case there are duplicate entries
          (format output-stream "~(~a~)," (recipe-id solution))
          (when (find 'smatch-score metrics-left)
            (setf metrics-left (remove 'smatch-score metrics-left))
            (format output-stream (concatenate 'string "~$" (if metrics-left "," "~%")) (smatch-score solution)))
          (when (find 'goal-condition-success metrics-left)
            (setf metrics-left (remove 'goal-condition-success metrics-left))
            (format output-stream (concatenate 'string "~$" (if metrics-left "," "~%")) (subgoals-ratio solution)))
          (when (find 'dish-approximation-score metrics-left)
            (setf metrics-left (remove 'dish-approximation-score metrics-left))
            (format output-stream (concatenate 'string "~$" (if metrics-left "," "~%")) (dish-score solution)))      
          (when (find 'execution-time metrics-left)
            (setf metrics-left (remove 'execution-time metrics-left))
            (format output-stream (concatenate 'string "~d" (if metrics-left "," "~%")) (execution-time solution)))))
      
      (close output-stream)))

(defun print-solutions (solutions metrics)
  "Print out the solutions' scores."
  
  (dolist (solution solutions)
    (format t "RECIPE: ~(~a~)~%" (recipe-id solution))
    (when (find 'smatch-score metrics)
      (format t "Smatch Score: ~$~%" (smatch-score solution)))
    (when (find 'goal-condition-success metrics)
      (format t "Goal-Condition Success: ~$~%" (subgoals-ratio solution)))
    (when (find 'dish-approximation-score metrics)
      (format t "Dish Approximation Score: ~$~%" (dish-score solution)))
    (when (find 'execution-time metrics)
      (format t "Recipe Execution Time: ~d~%" (execution-time solution)))
    (format t "~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measuring Solution Correctness ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SMATCH score ;;

; Command: python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)'
; Example calls:
; (compute-smatch-score '((boy ?x) (paul ?x)) '((boy ?x) (paul ?x)))
; (compute-smatch-score '((paul ?x) (boy ?x) ) '((boy ?x) (paul ?x)))

(defun write-network-to-file (meaning-network filepath)
  "Write away the given meaning network to a file with the specified filepath."
  ; create all directories in the specified path if they do not exist yet
  (ensure-directories-exist filepath)
  
  ; write the output to the file at the specified path
  (let ((output-stream (open filepath
                             :if-does-not-exist :create
                             :direction :output
                             :if-exists :supersede)))
    (dolist (prim-op meaning-network)
      (format output-stream "~(~a~)~%" prim-op))
    (close output-stream)))

(defun compute-smatch-score (mn1 mn2
                             &key use-temp-files
                             (smatch-dir (babel-pathname :directory '("applications"
                                                                      "muhai-cookingbot"
                                                                      "recipe-execution-benchmark"
                                                                      "libs"
                                                                      "smatch"))))
  "Calls the python smatch program which calculates the smatch score
   of a parsed meaning network and a gold standard meaning.
   Temporary files will be used to store the meaning networks
   in case they are too big to give via command line.
   The argument lib-dir can be used to set the path to the required
   library folder in case this function is used outside of a Babel setup."
  (assert (progn (listp mn1) (listp mn2)))
  (assert (directory-pathname-p smatch-dir))
  (assert (program-installed-p "python3"))
  
  (let ((L1 (copy-list mn1))
        (L2 (copy-list mn2)))
    (setf L1 (sort L1 #'string-lessp :key #'first))
    (setf L2 (sort L2 #'string-lessp :key #'first))
    (setf L1 (format nil "~{~a~^ ~}" L1))
    (setf L2 (format nil "~{~a~^ ~}" L2))
    (let* ((smatch (merge-pathnames
                    (make-pathname :name "smatch" :type "py")
                    smatch-dir))
           (stream (pipe-input "python3" :args (list (namestring smatch) "-m"
                                                     (format nil "~s" L1)
                                                     (format nil "~s" L2))))
           (output (read-line stream)))
      (close stream)
      (if (search "F-score:" output)
        (read-from-string (second (split output #\:)))
        (error "Error during the execution of smatch. Arguments where ~a"
               (list (namestring smatch)
                     (format nil "\"~a\"" L1)
                     (format nil "\"~a\"" L2)))))))

;; Subgoal Evaluation (Goal Condition Testing) ;;

(defclass located-entity ()
  ((entity :type kitchen-entity :initarg :entity :accessor entity)
   (location :type list :initarg :location :accessor location))
  (:documentation "Wrapper class that wraps a kitchen entity with its location.")) 

(defmethod get-located-output-entity ((irl-node irl::pip-node))
  "Get the value belonging to the binding for the primary output of this node's primitive under evaluation 
   and wrap it together with its location in output kitchen state."
  (let ((output (get-output-value irl-node)) 
        (ks (get-output-kitchen-state irl-node)))
    (make-instance 'located-entity :entity output :location (find-location output ks))))

(defmethod compute-subgoal-success-ratio ((sol-node irl::pip-node) (gold-node irl::pip-node))
  "Compute the ratio of the subgoals that have been reached to the total number of subgoals that are present.
   The given IRL nodes are expected to be the final subnodes of the IRL evaluation."
  (multiple-value-bind (goals-reached goals-failed) (evaluate-subgoals sol-node gold-node)
    (/ (length goals-reached) (+ (length goals-reached) (length goals-failed)))))

(defmethod evaluate-subgoals ((sol-node irl::pip-node) (gold-node irl::pip-node))
  "Get a list of the subgoals that have been reached and a list of the subgoals that have failed.
   A subgoal is defined as the primary output of the golden standard node and is reached if a similar primary output is present in a solution node."
  (let* ((gold-nodes (get-full-node-sequence gold-node))
         ; no need to check the initial kitchen-states as these will always be correct
         (filtered-gold-nodes (remove-if #'(lambda (node) (eql (get-predicate-name node) 'get-kitchen))
                                                           gold-nodes))
         (sol-nodes (get-full-node-sequence sol-node))
         (filtered-sol-nodes (remove-if #'(lambda (node) (or
                                                           (eql (get-predicate-name node) 'get-kitchen)
                                                           (subtypep (type-of (get-output-value node)) 'failed-object))) sol-nodes))
         (gold-entities (mapcar #'get-located-output-entity filtered-gold-nodes))
         (sol-entities  (mapcar #'get-located-output-entity filtered-sol-nodes))
         (goals-reached '())
         (goals-failed '()))   
    (loop for gold-entity in gold-entities
          for sol-entity = (find-if #'(lambda (sol-entity)
                                        ; for a subgoal to be reached both the location and the entity itself should be similar 
                                        (and (similar-entities (entity gold-entity) (entity sol-entity))
                                             (similar-locations (location gold-entity) (location sol-entity))))
                                        sol-entities)
          if sol-entity
            do
              ; each subgoal can only be matched once (in case the same subgoal would be repeated multiple times)
              (setf sol-entities (remove sol-entity sol-entities))
              (push gold-entity goals-reached)
          else do (push gold-entity goals-failed))
    (values goals-reached goals-failed)))

;; Dish Score Computation ;;

; A wrapper class for ingredient that also contains a pointer to the mixture it belongs. This class is used in "dish unfolding".
; This pointer is not added to ingredient directly to prevent infinite loop issues in web visualization.
(defclass hierarchy-ingredient ()
  ((ingredient :type ingredient :initarg :ingredient :accessor ingredient)
   (part-of :initarg :part-of :accessor part-of :initform nil))
  (:documentation "Wrapper class that wraps an ingredient with the hierarchy of mixtures it belongs."))

(defmethod get-mixture-hierarchy ((hierarchy-ingredient hierarchy-ingredient))
  "Extract a list of mixtures that this ingredient is directly or indirectly used in (in the order from first to last mixture creation)."
  (let ((mixture (part-of hierarchy-ingredient))
        (mixtures '()))    
    (loop while mixture
            do
              (setf mixtures (append (list (ingredient mixture)) mixtures))
              (setf mixture (part-of mixture)))
    mixtures))

(defclass similarity-score ()
  ((points :type number :initarg :points :accessor points :initform 0)
   (max-points :type number :initarg :max-points :accessor max-points :initform 0))
  (:documentation "Similarity score of two dishes based on the number of awarded points and the maximum number of possible points."))

(defmethod add-points ((similarity-score similarity-score) (awarded-points number) (possible-points number))
  "Change the similarity score by incrementing points and maximum attainable points by the given numbers"
  (setf (points similarity-score) (+ (points similarity-score) awarded-points))
  (setf (max-points similarity-score) (+ (max-points similarity-score) possible-points)))

(defmethod add-similarity-score-to ((similarity-score similarity-score) (to-similarity-score similarity-score))
  "Increment a similarity score by adding another similarity score to it."
  (setf (points to-similarity-score) (+ (points similarity-score) (points to-similarity-score)))
  (setf (max-points to-similarity-score) (+ (max-points similarity-score) (max-points to-similarity-score))))

(defmethod compute-ratio ((similarity-score similarity-score))
  "Compute the actual dish-score as the ratio of the awarded points to the maximum number of points that could be reached."
  (if (= (max-points similarity-score) 0)
    1
    (/ (points similarity-score) (max-points similarity-score))))

; made so comparison code doesn't have to make certain explicit distinctions between a container and a list-of-kitchen-entities object
(defgeneric contents-or-items (object)
  (:documentation "Convenience function to get the contents/items from a container of list-of-kitchen-entities."))

(defmethod contents-or-items ((container container))
  (contents container))

(defmethod contents-or-items ((list-of-kitchen-entities list-of-kitchen-entities))
  (items list-of-kitchen-entities))

(defun get-ingredient-additions (ingredient)
  "Convenience function to get all the additional ingredients that have indirectly been added to an ingredient."
  (let ((additions '()))
    (when (and (subtypep (type-of ingredient) 'can-be-sprinkled-on)
               (sprinkled-with ingredient))
      (push (sprinkled-with ingredient) additions))
    (when (and (subtypep (type-of ingredient) 'dippable)
               (dipped-in ingredient))
      (push (dipped-in ingredient) additions))
    additions))

(defun all-ingredients (object)
  "Convenience function to get all the ingredients from a container or list-of-kitchen-entities, including sprinkles and dips."
  (let ((ingredients (contents-or-items object)))
    (loop for ingredient in ingredients
          for additions = (get-ingredient-additions ingredient)
          when additions
            do
              (setf ingredients (append additions ingredients)))
    ingredients))

(defmethod unfold-mixture ((mixture-to-unfold hierarchy-ingredient))
  "Unfold the given mixture into a list of all the base ingredients that are contained in it."
  (let* ((inner-mixture (ingredient mixture-to-unfold))
         (mixture-value (value (quantity (amount inner-mixture))))
         (unfolded-comps '()))
    (loop for comp in (components inner-mixture)
          do
            ; transform the percentage amount to the real amount in grams
            (setf (amount comp)
                  (make-instance 'amount
                                 :quantity (make-instance 'quantity
                                                          :value (* (value (quantity (amount comp))) mixture-value))
                                 :unit (unit (copy-object (amount inner-mixture)))))
            (cond ((subtypep (type-of comp) 'mixture)
                   (let ((unfolded-sub-comps (unfold-mixture (make-instance 'hierarchy-ingredient
                                                                            :ingredient comp
                                                                            :part-of mixture-to-unfold))))
                     (setf unfolded-comps (append unfolded-comps unfolded-sub-comps))))
                  ((subtypep (type-of comp) 'ingredient)
                   (setf unfolded-comps (append unfolded-comps (list (make-instance 'hierarchy-ingredient
                                                                                    :ingredient comp
                                                                                    :part-of mixture-to-unfold)))))
                  (t (print "unsupported component of class ~a" (type-of comp))))

            ; also take additional ingredients into account that might be present in the form of sprinkles or dips
            (loop for addition in (get-ingredient-additions comp)
                  do
                    (cond ((subtypep (type-of addition) 'mixture)
                           (let ((unfolded-sub-comps (unfold-mixture (make-instance 'hierarchy-ingredient
                                                                                    :ingredient addition
                                                                                    :part-of '()))))
                             (setf unfolded-comps (append unfolded-comps unfolded-sub-comps))))
                          ((subtypep (type-of addition) 'ingredient)
                           (setf unfolded-comps (append unfolded-comps (list (make-instance 'hierarchy-ingredient
                                                                                            :ingredient addition
                                                                                            :part-of '())))))
                          (t (print "unsupported addition of class ~a" (type-of addition))))))
    unfolded-comps))

(defun unfold-dish (dish)
  "Unfold the contents of the given container into a list of all the base ingredients that are contained in it."
  (unless (or (subtypep (type-of dish) 'container) (subtypep (type-of dish) 'list-of-kitchen-entities))
    (error "unfold-dish expects either a container or a list-of-kitchen-entities, but got ~a" (type-of dish)))
  (let* ((dish-copy (copy-object dish))
         (unfolded-contents '())
         (merged-contents '()))
    ; unfold every item that is in the dish     
    (loop for item in (all-ingredients dish-copy)
            do (cond ((subtypep (type-of item) 'mixture)
                      (setf unfolded-contents (append unfolded-contents (unfold-mixture
                                                                         (make-instance 'hierarchy-ingredient
                                                                                        ; compare everything in g
                                                                                        :ingredient (convert-to-g item))))))
                     ((subtypep (type-of item) 'ingredient)
                      (setf unfolded-contents (append unfolded-contents 
                                                      (list (make-instance 'hierarchy-ingredient
                                                                           ; compare everything in g
                                                                           :ingredient (convert-to-g item))))))))
    ; we take together the items that are the same and consider them to be one big item (for better comparison)
    (loop while unfolded-contents
          for item = (first unfolded-contents)
          for matching-ings = (find-all-if #'(lambda (sim-ing)
                                               (and (similar-entities (ingredient item) (ingredient sim-ing) '(id persistent-id amount))
                                                    (= (length (get-mixture-hierarchy item)) (length (get-mixture-hierarchy sim-ing)))
                                                    (loop for mixture-1 in (get-mixture-hierarchy item)
                                                          for mixture-2 in (get-mixture-hierarchy sim-ing)
                                                          always (similar-entities mixture-1 mixture-2 '(id persistent-id amount)))))
                                           (rest unfolded-contents))
          if matching-ings
              do
              (setf (value (quantity (amount (ingredient item)))) (+ (value (quantity (amount (ingredient item))))
                                                                     (loop for matching-ing in matching-ings
                                                                           for current-value = (value (quantity (amount (ingredient matching-ing))))
                                                                           sum current-value)))
              ; amount of the mixtures in the hierarchy are currently not directly checked anywhere,
              ; so they don't have to be summed together (to save computing time)
              (push item merged-contents)
              (setf unfolded-contents (set-difference unfolded-contents (append (list item) matching-ings)))
          else
              do
              (push item merged-contents)
              (setf unfolded-contents (remove item unfolded-contents)))
    
    merged-contents))

(defmethod compare-hierarchy-ingredient ((sol-ingredient hierarchy-ingredient) (gold-ingredient hierarchy-ingredient))
  "Compute a similarity score for the given ingredients."
  (let* ((sol-ing (ingredient sol-ingredient))
         (sol-dish-slots (get-slotnames sol-ing '(id persistent-id)))
         (sol-mixtures (get-mixture-hierarchy sol-ingredient))
         (gold-ing (ingredient gold-ingredient))
         (gold-dish-slots (get-slotnames gold-ing '(id persistent-id)))
         (gold-mixtures (get-mixture-hierarchy gold-ingredient))
         (ingredient-similarity-score (make-instance 'similarity-score))
         (hierarchy-similarity-score (make-instance 'similarity-score)))
    ; class doesn't have to be checked, since this function will currently only be called for ingredients that are the same     
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-ing slot)
                                    (slot-value gold-ing slot)))
            do (add-points ingredient-similarity-score 1 1)
          else
            do (add-points ingredient-similarity-score 0 1))
    ; check mixture hierarchy composition
    (loop for gold-mixture in gold-mixtures
          for sol-mixture in sol-mixtures
          do (add-points hierarchy-similarity-score (compute-ratio (compare-mixture sol-mixture gold-mixture)) 1))
    ; adjust the average of the similarity score based on the surplus or shortage of mixtures
    (setf (max-points hierarchy-similarity-score) (+ (max-points hierarchy-similarity-score)
                                                     (abs (- (length gold-mixtures) (length sol-mixtures)))))

    ; compute the final similarity-score for this ingredient, with ingredient composition being a bit more important than mixture hierarchy
    (make-instance 'similarity-score
                   :points (+ (* 0.6 (compute-ratio ingredient-similarity-score))
                              (* 0.4 (compute-ratio hierarchy-similarity-score)))
                   :max-points 1)))

(defmethod compare-mixture ((sol-mixture mixture) (gold-mixture mixture))
  "Compute a similarity score for the given mixtures."
   ; amount will only differ if components are different, which is already checked and rewarded/punished through other routes
  (let ((sol-dish-slots (get-slotnames sol-mixture '(id persistent-id components amount)))
        (gold-dish-slots (get-slotnames gold-mixture '(id persistent-id components amount)))
        (mixture-similarity-score (make-instance 'similarity-score)))    
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-mixture slot)
                                    (slot-value gold-mixture slot)))
            do (add-points mixture-similarity-score 1 1)
          else
            do (add-points mixture-similarity-score 0 1))
    ; check if it is the same type of mixture (heterogeneous or homogeneous)
    (if (eq (type-of sol-mixture) (type-of gold-mixture))
      (add-points mixture-similarity-score 1 1)
      (add-points mixture-similarity-score 0 1))
    mixture-similarity-score))

(defmethod compare-node-dishes ((sol-dish irl::pip-node) (gold-dish irl::pip-node))
  "Compute a similarity score for the final dish that was made when reaching this node."
  ; skip comparison with failed output objects
  (if (has-failed-objects (get-output-value sol-dish))
    (make-instance 'similarity-score
                   :points 0
                   :max-points 1)
    (let* ((sol-value (get-output-value sol-dish))
           (sol-location (find-location sol-value (get-output-kitchen-state sol-dish)))
           (sol-dish-slots (get-slotnames sol-value '(persistent-id id contents items))) ; all slots except contents/items
           (gold-value (get-output-value gold-dish))
           (gold-location (find-location gold-value (get-output-kitchen-state gold-dish)))
           (gold-dish-slots (get-slotnames gold-value '(persistent-id id contents items))) ; all slots except contents/items
           (container-score (make-instance 'similarity-score))
           (contents-score (make-instance 'similarity-score)))
    ; check if this node actually returns a container with ingredients or a list-of-kitchen-entities
      (if (not (and (or (subtypep (type-of sol-value) 'container) (subtypep (type-of sol-value) 'list-of-kitchen-entities))
                    (all-ingredients sol-value)
                    (loop for item in (all-ingredients sol-value) always (subtypep (type-of item) 'ingredient))))
        (make-instance 'similarity-score
                       :points 0
                       :max-points 1)
        (progn
          ;; container specific scoring
        ; location of dish is worth a score of 1
          (if (similar-locations sol-location gold-location)
            (add-points container-score 1 1)
            (add-points container-score 0 1))
         ; type of container is worth a score of 1
          (if (eq (type-of sol-value) (type-of gold-value))
            (add-points container-score 1 1)
            (add-points container-score 0 1))
        ; each slot that is in common is worth a score of 1  
          (loop for slot in gold-dish-slots
                if (and (member slot sol-dish-slots)
                        (similar-entities (slot-value sol-value slot)
                                          (slot-value gold-value slot)))
                  do (add-points container-score 1 1)
                else
                  do (add-points container-score 0 1))
        ; number of portions in the dish is worth a score of 1
        ; (only 1 because right now an end dish will always be portions of the same mixture, so just one portioning operation might be missing)
          (if (= (length (contents-or-items sol-value)) (length (contents-or-items gold-value)))
            (add-points container-score 1 1)
            (add-points container-score 0 1))
          ;; contents specific scoring
          (let ((unfolded-dish-sol (unfold-dish sol-value))
                (unfolded-dish-gold (unfold-dish gold-value))
                (missing-ingredients '()))
            (loop for unfolded-ing-gold in unfolded-dish-gold
                  for matching-ings-sol = (find-all (type-of (ingredient unfolded-ing-gold)) unfolded-dish-sol
                                                    :key #'(lambda (sim-ing) (type-of (ingredient sim-ing))))
                  if matching-ings-sol
                  ; same ingredient can occur multiple times in slightly different forms, 
              ; so check with which ingredient maximum similarity is found and use that one for score computation
                    do (let* ((sim-scores (loop for matching-ing-sol in matching-ings-sol
                                                collect (compare-hierarchy-ingredient matching-ing-sol unfolded-ing-gold)))
                              (match-scores (mapcar #'compute-ratio sim-scores))
                              (max-score (apply #'max match-scores))
                              (max-position (position max-score match-scores))
                              (max-ing (nth max-position matching-ings-sol)))
                       
                         (add-points contents-score (compute-ratio (nth max-position sim-scores)) 1)
                         (setf unfolded-dish-sol (remove max-ing unfolded-dish-sol))) 
                  else
                    do (push unfolded-ing-gold missing-ingredients))
          ; adjust the average of the similarity score based on the shortage of mixtures
            (setf (max-points contents-score) (+ (max-points contents-score)
                                                 (length missing-ingredients)))
          ; adjust the average of the similarity score based on the surplus of mixtures
            (setf (max-points contents-score) (+ (max-points contents-score)
                                                 (length unfolded-dish-sol))))

         ; compute the final similarity-score for this dish, contents are much more important than container characteristics
          (make-instance 'similarity-score
                         :points (+ (* 0.98 (compute-ratio contents-score))
                                    (* 0.02 (compute-ratio container-score)))
                         :max-points 1))))))

(defmethod find-best-dish-score ((sol-final-node irl::pip-node) (gold-output-node irl::pip-node))
  "Compute a similarity score for all nodes in the solutions and return the best one."
  (let ((node sol-final-node)
        (encountered-variables '())
        (scores '()))
    (loop for sol-output-var = (second (irl::primitive-under-evaluation node))
          for score = 0
          do
            (when (not (member sol-output-var encountered-variables))
              (let ((score (compute-ratio (compare-node-dishes node gold-output-node))))
                (push score scores)
                (setf encountered-variables (append (all-variables (irl::primitive-under-evaluation node)) encountered-variables))))
            (setf node (parent node))
          while (and node (irl::primitive-under-evaluation node) (< score 1)))
    (apply #'max scores)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution File Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *all-metrics*
  '(smatch-score goal-condition-success dish-approximation-score execution-time))

(defparameter *all-simulation-metrics*
  '(goal-condition-success dish-approximation-score execution-time))

(defun evaluate-solutions (solutions-file gold-standard-solutions &key (metrics *all-metrics*))
  ; read in the solutions file
  (let ((solutions (parse-solutions-file solutions-file)))
    ; check if the solutions file contains all the needed solutions
    (check-solutions solutions gold-standard-solutions)
    ; compare the solutions against the gold standards
    (loop for current-solution in solutions
          for solution-mn = (meaning-network current-solution)  
          for current-id = (recipe-id current-solution)
          for current-sim-env = (find current-id gold-standard-solutions
                                      :key #'(lambda (env) (recipe-id env)))
          for gold-mn = (meaning-network current-sim-env)
          for final-gold-node = (final-node current-sim-env) 
          for gold-output-node = (output-node current-sim-env)
          do
            ; compute the smatch score (no simulation needed for this part)
            (when (member 'smatch-score metrics)
              (setf (smatch-score current-solution)
                    (compute-smatch-score solution-mn gold-mn)))

            ; simulate and score recipe "execution"    
            (init-kitchen-state current-sim-env)
            (let ((extended-mn (append-meaning-and-irl-bindings solution-mn nil)))
              (multiple-value-bind (sol-bindings sol-nodes) (evaluate-irl-program extended-mn nil)
                (if (and sol-bindings sol-nodes)
                  (progn ; compute subgoal success ratio
                    (when (member 'goal-condition-success metrics)
                      (setf (subgoals-ratio current-solution)
                            (if sol-nodes
                              (compute-subgoal-success-ratio (first sol-nodes) final-gold-node)
                              0)))
                    ; compute the dish score
                    ; if all subgoals are reached, then the dish score is not necessarily maximal,
                    ; since we could have altered the final dish further after reaching all subgoals
                    (when (member 'dish-approximation-score metrics)
                      (setf (dish-score current-solution)
                            (if sol-nodes
                              (find-best-dish-score (first sol-nodes) gold-output-node)
                              0)))
                     ; compute the ratio of needed execution time to the execution time of the golden standard
                    (when (member 'execution-time metrics)
                      (setf (execution-time current-solution)
                            (if sol-bindings
                              (compute-execution-time (first sol-bindings))
                              0))))
                  (progn
                   (unless sol-bindings (print "no sol bindings"))
                   (unless sol-nodes (print "no sol nodes")))))))
    solutions))