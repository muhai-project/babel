(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; The personal dynamic memory of the cooking robot. ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PDM class and basic methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass personal-dynamic-memory ()
  ((grammar :type fcg-construction-set
            :accessor grammar
            :initarg :grammar
            :initform *fcg-constructions*
            :documentation "The grammar of the agent.")
   (primitives :type primitive-inventory
               :accessor primitives
               :initarg :primitives
               :initform *irl-primitives*
               :documentation "The primitives of the agent.")
   (world-states :type list
                 :accessor world-states
                 :initarg :world-states
                 :initform '()
                 :documentation "The stack of world-states."))
  (:documentation "The personal dynamic memory of the agents contains all its knowledge."))

(defmethod  initialize-instance :after ((pdm personal-dynamic-memory) &key &allow-other-keys)
  "Upon initialisation, set the pdm slotof all world-states."
  (loop for ws in (world-states pdm)
        do (setf (personal-dynamic-memory ws) pdm)))

(defun initialise-personal-dynamic-memory (grammar initial-irl-program &key (primitive-inventory *irl-primitives*))
  "Helper function for initialising a pdm."
  (set-data (blackboard grammar) :ontology-hash-table (make-ontology-vectors))
  (let* ((initial-accessible-entities
          (first (evaluate-irl-program initial-irl-program nil
                                       :primitive-inventory primitive-inventory)))
         (initial-world-state
          (make-instance 'world-state :accessible-entities initial-accessible-entities)))
    (make-instance 'personal-dynamic-memory
                   :world-states (list initial-world-state)
                   :grammar grammar
                   :primitives primitive-inventory)))

(defclass world-state ()
  ((accessible-entities :type list
                        :accessor accessible-entities
                        :initarg :accessible-entities
                        :initform '()
                        :documentation "The entities in the world-model that are currently accessible (list-of-bindings).")
   (personal-dynamic-memory :accessor personal-dynamic-memory
                            :initarg :personal-dynamic-memory
                            :documentation "A pointer back to the personal dynamic memory that the world state belongs to.")))


;; PDM utility functions / methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-utterances (list-of-utterances personal-dynamic-memory &key silent)
  (loop with cxn-inventory = (grammar personal-dynamic-memory)
        with primitive-inventory = (primitives personal-dynamic-memory)
        with meaning-network = nil
        with final-set-of-bindings = nil
        for utterance in list-of-utterances
        for current-world-state = (first (world-states personal-dynamic-memory))
        do (multiple-value-bind (bindings-lists parsed-meaning)
               (understand-and-execute utterance current-world-state cxn-inventory primitive-inventory :silent silent)
             (loop for bindings-list in bindings-lists
                   when bindings-list
                     do (push (make-instance 'world-state
                                           :accessible-entities bindings-list
                                           :personal-dynamic-memory personal-dynamic-memory)
                            (world-states personal-dynamic-memory))
                        (setf meaning-network (append meaning-network (reverse parsed-meaning)))
                        (setf final-set-of-bindings bindings-list)))
        finally (return (values final-set-of-bindings meaning-network))))

        
(defun understand-and-execute (utterance
                               world-state
                               cxn-inventory
                               primitive-inventory
                               &key silent)
  (let* ((comprehension-result
          (multiple-value-list (understand utterance cxn-inventory world-state :silent silent)))
         (parsed-meaning (first comprehension-result))
         (existing-bindings (accessible-entities world-state))
         (extended-meaning (append-meaning-and-irl-bindings parsed-meaning existing-bindings))
         (resulting-bindings-lists
          (evaluate-irl-program extended-meaning nil :primitive-inventory primitive-inventory))
         (open-variables (get-unconnected-vars extended-meaning))
         (resulting-bindings-with-open-variables
          (mapcar #'(lambda (bindings-list)
                      (loop for v in open-variables
                            when (and (or (find v existing-bindings :key #'var)
                                          (find v (all-variables parsed-meaning)))
                                      (available-at (find v bindings-list :key #'var)))
                              collect (find v bindings-list :key #'var)))
                  resulting-bindings-lists)))
    (values resulting-bindings-with-open-variables parsed-meaning)))




(defun append-meaning-and-irl-bindings (irl-program irl-bindings)
  "Macroexpands an irl program and appends bind statements from accessible-entities in PDM."
  (append (irl-bindings-to-bind-statements irl-bindings)
          (instantiate-non-variables-in-irl-program irl-program)))


(defun instantiate-non-variables-in-irl-program (irl-program)
  (mapcar #'instantiate-non-variables-in-irl-primitive irl-program))

(defun instantiate-non-variables-in-irl-primitive (irl-primitive)
  (if (eql (first irl-primitive) 'bind)
    (list 'bind (second irl-primitive)
          (third irl-primitive)
          (case (second irl-primitive)
            (quantity (make-instance 'quantity :value (fourth irl-primitive)))
            (ingredient (make-instance (fourth irl-primitive) :is-concept t))
            (t (make-instance (fourth irl-primitive)))))
    (loop for el in (rest irl-primitive)
          if (variable-p el)
            collect el into args
          else collect
              (cond ((numberp el)
                     (make-instance 'quantity :value el))
                    ((listp el)
                     (make-instance 'list-of-kitchen-entities :items el))
                    (t
                     (make-instance el :is-concept t)))
              into args
          finally (return (cons (first irl-primitive) args)))))

(defun irl-bindings-to-bind-statements (list-of-irl-bindings)
  "Transforms a list of IRL bindings into a ((bind class ?var object)) statement."
  (mapcar #'irl-binding-to-bind-statement list-of-irl-bindings))

(defun irl-binding-to-bind-statement (irl-binding)
  "Transforms an IRL binding into a (bind class ?var object) statement."
  `(bind ,(type-of (value irl-binding)) ,(var irl-binding) ,(value irl-binding) ,(available-at irl-binding)))

(defun find-object-in-pdm (object pdm &key (test #'eql) (key #'id))
  "Finds an object in a pdm."
  (find object (all-objects-in-current-kitchen-state pdm) :test test :key key))

(defun all-objects-in-current-kitchen-state (pdm)
  "Returns all objects in the current kitchen state of the pdm, including the kitchen state itself."
  (let* (;; First find current kitchen state
         (current-kitchen-state (first (world-states pdm)))
         ;; Then find top-level-containers
         (top-level-containers (loop for slot in (closer-mop:class-slots (find-class 'kitchen-state))
                                     when (closer-mop:subtypep (find-class
                                                                (closer-mop:slot-definition-type slot))
                                                               'container)
                                  collect (slot-value current-kitchen-state (closer-mop:slot-definition-name slot)))))
    ;; Now recursively traverse these top-level-containers
    (loop for container in top-level-containers
          append (all-objects-in-container container)
          into subcontainers
          finally (return (append (list current-kitchen-state) top-level-containers subcontainers)))))

(defun all-objects-in-container (container)
  "Returns all the objects in a container (recursively!), including the container itself."
  (loop for object in (contents container)
        if (subtypep (class-of object) 'container)
        append (append (list object) (all-objects-in-container object))
        else
        collect object))

