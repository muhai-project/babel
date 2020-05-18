(in-package :propbank-english)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;Learning constructions based on Propbank annotated corpora.   ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning a single cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory)
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let* ((frame (find roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'string=))
         (unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence) :de-render-constituents-dependents)))
         (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                for role-start = (first (indices role))
                                for role-end = (+ (last-elt (indices role)) 1)
                                for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                collect (cons role unit)))
         (cxn-name-list (loop for (role . unit) in units-with-role
                              collect (format nil "~a:~a" (role-type role) ;;create a name based on role-types and lex-class/phrase-type
                                              (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                                (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
         (contributing-unit (make-propbank-contributing-unit units-with-role frame))
         (cxn-units-with-role (loop for unit in units-with-role collect (make-propbank-conditional-unit-with-role unit)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role unit-structure)))

    ;;create a new construction and add it to the cxn-inventory
    (eval `(def-fcg-cxn ,(make-id (format nil "~{~a~^+~}-cxn" cxn-name-list))
                        (,contributing-unit
                         <-
                         ,@cxn-units-with-role
                         ,@cxn-units-without-role)
                        :cxn-inventory ,cxn-inventory))))


(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun make-propbank-contributing-unit (units-with-role frame)
  "Make a contributing unit based on a frame and units-with-role."
  (let* ((v-unit (cdr (assoc "V" units-with-role :key #'role-type :test #'string=)))
         (unit-name (variablify (unit-name v-unit)))
         (args (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect '(referent ?f)
                     else collect `(,(make-kw (role-type r)) ,(variablify (unit-name (cdr (assoc r units-with-role)))))))
         (meaning (loop for r in (frame-roles frame)
                     if (string= (role-type r) "V")
                     collect `(frame ,(intern (upcase (frame-name frame))) ?f)
                     else collect `(frame-element ,(intern (upcase (role-type r))) ?f
                                                  ,(variablify (unit-name (cdr (assoc r units-with-role))))))))
    `(,unit-name
      (args ,@args)
      (frame-evoking +)
      (meaning ,meaning))))

(defun make-propbank-conditional-unit-with-role (unit-with-role)
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
         (phrase-type-or-lex-class (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                     `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                     `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name))))))
    (if  (string= (role-type (car unit-with-role)) "V")
      ;;a FEE unit also has the feature lemma
      `(,unit-name
        --
        (lemma ,(cadr (find 'lemma (unit-body unit) :key #'feature-name)))
        (parent ,parent)
        ,phrase-type-or-lex-class)
      ;;other units only have a parent feature and a phrase-type/lex-class feature
      `(,unit-name
        --
        (parent ,parent)
        ,phrase-type-or-lex-class))))


(defun make-propbank-conditional-units-without-role (units-with-role cxn-units-with-role unit-structure)
  "Makes conditional units that are needed in a propbank cxn to encode
the paths in the syntactic tree between units that function as slot
fillers (arg0, arg1) and the frame-evoking element unit."
  (remove-duplicates
   (loop with fee-unit = (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                                               units-with-role))
         for unit-with-role in (remove fee-unit units-with-role :test #'equal) ;;discard the frame-evoking element (FEE) unit
         for path = (find-path-in-syntactic-tree (cdr unit-with-role) fee-unit unit-structure) ;;find path between a unit in the transient structure and the FEE unit
         append (loop for unit-name in path
                      for unit = (find unit-name unit-structure :key #'unit-name)
                      unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name) ;;check that the unit is not a frame-element
                      collect `(,(variablify unit-name)
                                --
                                (parent ,(variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                                ,(if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                   `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                   `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                                                    :key #'unit-name))

(defun find-path-in-syntactic-tree (unit v-unit unit-structure)
  "A search process that finds a path between two units in a transient
structure. The path is returned as a list of unit names, ordered from
start to end(v-unit)"
  (let ((queue (list `((:unit-name . ,(unit-name unit))
                       (:parent . ,(cadr (find 'parent (unit-body unit) :key #'feature-name)))
                       (:constituents .  ,(cadr (find 'constituents (unit-body unit) :key #'feature-name)))
                       (:path . ((,(unit-name unit) . :initial)))))))

    (loop while queue
          for state = (pop queue)
          if (equal (cdr (assoc :unit-name state)) (unit-name v-unit)) ;;solution found
          return (mapcar #'car (cdr (assoc :path state)))
          else do (setf queue (append queue (make-constituent-and-parent-states state unit-structure))))))

(defun make-constituent-and-parent-states (state unit-structure)
  "Creates new states for parent and constituents of current state"
  (let ((new-states nil))
    ;;First create states based on the constituents of the current state
    (loop for constituent-name in (cdr (assoc :constituents state)) 
          for constituent-unit = (find constituent-name unit-structure :key #'unit-name)
          for new-state = `((:unit-name . ,constituent-name)
                            (:parent . ,(cadr (find 'parent (unit-body constituent-unit) :key #'feature-name)))
                            (:constituents . ,(cadr (find 'constituents (unit-body constituent-unit) :key #'feature-name)))
                            (:path . ,(append (cdr (assoc :path state)) (list (cons constituent-name :constituent)))))
          unless (find constituent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          do (push new-state new-states))

    ;;Now create a state for the parent of the current state
    (when (assoc :parent state)
      (let* ((parent-name (cdr (assoc :parent state)))
             (parent-unit (find parent-name unit-structure :key #'unit-name))
             (new-state `((:unit-name . ,parent-name)
                          (:parent . ,(cadr (find 'parent (unit-body parent-unit) :key #'feature-name)))
                          (:constituents . ,(cadr (find 'constituents (unit-body parent-unit) :key #'feature-name)))
                          (:path . ,(append (cdr (assoc :path state)) (list (cons parent-name :parent)))))))
        (unless (find parent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          (push new-state new-states))))
    new-states))




               
       
