
(in-package :irl)

;; ############################################################################
;; primitive inventory definition:
;; ----------------------------------------------------------------------------

(export '(primitives-list primitive-inventory-size add-primitive delete-primitive
          find-primitive set-configuration get-configuration
          def-irl-primitives))
          

(defclass primitive-inventory ()
  ((name
    :type symbol :initform (gensym "PRIMITIVES")
    :initarg :name :accessor name
    :documentation "The name of the construction inventory. Typically,
                    this is the name that you use in the
                    def-irl-primitives macro")
   (configuration
    :type configuration :initform (make-config)
    :initarg :configuration :accessor configuration
    :documentation "Determines the behavior of the
                    primitive inventory in processing")
   (visualization-configuration
    :type configuration :initform (make-config)
    :initarg :visualization-configuration
    :accessor visualization-configuration
    :documentation "Determines the behavior of the
                    visualizations in the web-interface")
   (data
    :type blackboard :initform (make-blackboard)
    :initarg :blackboard :accessor blackboard)
   (primitives
    :type list :initform nil
    :initarg :primitives
    :documentation "The list of primitives"))
  (:documentation "The primitive inventory organises the primitives
       and determines the behaviour during processing"))


(defmethod initialize-instance :after ((primitive-inventory primitive-inventory)
                                       &key &allow-other-keys)
  "Initializing the primitive inventory. Here, we set the configurations to default values"
  (set-configuration primitive-inventory
                     :goal-tests '(:no-primitives-remaining
                                   :all-variables-bound))
  (set-configuration primitive-inventory
                     :node-tests '(:no-duplicate-solutions))
  (set-configuration primitive-inventory
                     :search-algorithm :best-first)
  (set-configuration primitive-inventory
                     :heuristics '(:nr-of-evaluated-primitives))
  (set-configuration primitive-inventory
                     :heuristic-value-mode :sum-heuristics-and-parent)
  (set-configuration primitive-inventory
                     :primitive-supplier-mode :simple-queue)
  (set-configuration primitive-inventory
                     :node-expansion-mode :full-expansion)
  (set-configuration primitive-inventory
                     :shuffle-primitives-before-evaluation t)
  (set-configuration primitive-inventory
                     :check-irl-program-before-evaluation t))
  
  
  

;; ------------------------------------------------------------------------
;; primitives

(defgeneric primitives (primitive-inventory &key &allow-other-keys)
  (:documentation "Return the list of primitives stored in the primitive-inventory"))

(defmethod primitives ((primitive-inventory primitive-inventory) &key)
  (slot-value primitive-inventory 'primitives))

(defgeneric (setf primitives) (primitive-list primitive-inventory &key &allow-other-keys)
  (:documentation "Sets the primitives of a primitive inventory"))

(defmethod (setf primitives) (primitive-list (primitive-inventory primitive-inventory) &key)
  (setf (slot-value primitive-inventory 'primitives) primitive-list))

;; ------------------------------------------------------------------------
;; primitives-list

(defgeneric primitives-list (primitive-inventory &key &allow-other-keys)
  (:documentation "Return the list of primitives stored in the primitive-inventory"))

(defmethod primitives-list ((primitive-inventory primitive-inventory) &key)
  (slot-value primitive-inventory 'primitives))

;; ------------------------------------------------------------------------
;; primitive-inventory-size / number-of-primitives

(defgeneric primitive-inventory-size (primitive-inventory &key &allow-other-keys)
  (:documentation "Return the size of the primitive inventory"))

(defmethod primitive-inventory-size ((primitive-inventory primitive-inventory) &key)
  (length (primitives primitive-inventory)))

(defgeneric number-of-primitives (primitive-inventory &key &allow-other-keys)
  (:documentation "Returns the size of the primitive inventory"))

(defmethod number-of-primitives ((primitive-inventory primitive-inventory) &key)
  (length (primitives primitive-inventory)))

;; ------------------------------------------------------------------------
;; find-primitive

(defgeneric find-primitive (primitive primitive-inventory &key test key)
  (:documentation "Finds a primitive in the primitive inventory"))

(defmethod find-primitive ((primitive primitive)
                           (primitives list)
                           &key (key #'id) (test #'eql))
  (find (funcall key primitive) primitives
        :key key :test test))

(defmethod find-primitive ((primitive primitive)
                           (primitive-inventory primitive-inventory)
                           &key (key #'id) (test #'eql))
  (find (funcall key primitive)
        (primitives primitive-inventory)
        :key key :test test))

(defmethod find-primitive ((primitive symbol)
                           (primitive-inventory primitive-inventory)
                           &key (key #'id) (test #'irl-equal))
  (find primitive
        (primitives primitive-inventory)
        :key key :test test))

;; ------------------------------------------------------------------------
;; add-primitive

(defgeneric add-primitive (primitive primitive-inventory &key &allow-other-keys)
  (:documentation "Adds a primitive to a primitive inventory"))

(defmethod add-primitive :before ((primitive primitive)
                                  (primitive-inventory primitive-inventory)
                                  &key (replace-when-equivalent t)
                                  (equivalent-key #'id)
                                  (equivalent-test #'eql))
  (when replace-when-equivalent
    (delete-primitive primitive primitive-inventory
                      :key equivalent-key :test equivalent-test)))

(defmethod add-primitive ((primitive primitive)
                          (primitive-inventory primitive-inventory)
                          &key (replace-when-equivalent t)
                          (equivalent-key #'id)
                          (equivalent-test #'eql))
  (declare (ignorable replace-when-equivalent
                      equivalent-key
                      equivalent-test))
  (push primitive (primitives primitive-inventory))
  (values primitive-inventory primitive))

;; ------------------------------------------------------------------------
;; delete-primitive

(defgeneric delete-primitive (primitive primitive-inventory &key test key)
  (:documentation "Deletes a primitive from the primitive inventory.
       Returns the deleted primitive or nil when it could not be found."))

(defmethod delete-primitive ((primitive primitive)
                             (primitive-inventory primitive-inventory)
                             &key (key #'id) (test #'eql))
  (let ((to-delete (find-primitive primitive primitive-inventory :key key :test test)))
    (when to-delete
      (setf (primitives primitive-inventory)
            (remove to-delete (primitives primitive-inventory)))
      to-delete)))

;; ------------------------------------------------------------------------
;; copy-object-content

(defmethod copy-object-content ((source primitive-inventory)
                                (target primitive-inventory))
  (setf (configuration target)
        (copy-object (configuration source)))
  (setf (visualization-configuration target)
        (copy-object (visualization-configuration source)))
  (setf (blackboard target)
        (copy-object (blackboard source)))
  (setf (primitives target)
        (copy-object (primitives source))))

;; ------------------------------------------------------------------------
;; set/get configuration

(defmethod set-configuration ((primitive-inventory primitive-inventory)
                              key value &key (replace t))
  (set-configuration (configuration primitive-inventory)
                     key value :replace replace))

(defmethod get-configuration ((primitive-inventory primitive-inventory) key &key)
  (get-configuration (configuration primitive-inventory) key))

;; ------------------------------------------------------------------------
;; print-object

(defmethod print-object ((primitive-inventory primitive-inventory) stream)
  (format stream "<~(~a~): ~a primitives>" 
          (class-name (class-of primitive-inventory))
          (primitive-inventory-size primitive-inventory)))

;; ------------------------------------------------------------------------
;; def-irl-primitives macro

(defun eval-when-bound (sexp)
  "evaluates sexp, and if it is an unbound atom, doesn't evaluate it"
  (if (and (atom sexp) (not (boundp sexp)))
    sexp
    (eval sexp)))


(defun find-key-arg (arguments key)
  "find key in arguments and return its value"
  (loop for (arg . remaining-args) on arguments
        when (eq arg key)
        return (first remaining-args)))


(defun remove-key-args (arguments)
  (loop for (arg . remaining-args) on arguments by #'cddr
        unless (keywordp arg)
        return (cons arg remaining-args)))


(defun check-def-irl-primitives-keys (keys-and-defs)
  (let ((accepted-keys '(:irl-configurations
                         :primitive-inventory
                         :visualization-configurations
                         ;:hashed 
                         ;:primitive-inventory-type
                         )))
    (dolist (x keys-and-defs)
      (when (keywordp x)
        (unless (member x accepted-keys)
          (error "Unknown keyword ~a. Accepted keywords are: ~a" x accepted-keys))))))


(defmacro def-irl-primitives (name &body keys-and-defs)
  "Create an IRL primitive inventory, setting all configurations"
  (check-def-irl-primitives-keys keys-and-defs)
  (let* ((name (eval-when-bound name))
         (creator-fn-name (internal-symb 'make- name '-primitives))
         (primitive-inventory (or (find-key-arg keys-and-defs :primitive-inventory)
                                  '*irl-primitives*)))
    `(progn
       (with-disabled-monitor-notifications
         (defun ,creator-fn-name ()
           (setf ,primitive-inventory
                 (make-instance 'primitive-inventory
                                :name ',name))
           ,@(loop for configuration in (find-key-arg keys-and-defs :irl-configurations)
                   collect `(set-configuration ,primitive-inventory
                                               ,(first configuration)
                                               ',(cdr configuration)))
           (let ((*irl-primitives* ,primitive-inventory))
             ,@(remove-key-args keys-and-defs)
             *irl-primitives*))
         (,creator-fn-name))
       ,primitive-inventory)))
  