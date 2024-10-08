(in-package :muhai-cookingbot)

(defun ontological-vector (ontological-class-name cxn-inventory)
  (gethash ontological-class-name (get-data (blackboard cxn-inventory) :ontology-hash-table)))

(defun all-subclasses (class)
  "returns a list of all subclasses of a given class, including the class itself."
  (if (not (closer-mop:class-direct-subclasses class))
    (list class)
    (loop for subclass in (closer-mop:class-direct-subclasses class)
          append (cons subclass (all-subclasses subclass)) into all-subclasses
          finally (return (remove-duplicates (cons class all-subclasses))))))


(defun all-superclasses (class &optional collected-superclasses &key until)
  "Returns a list of all superclasses of a given class, including the
class itself, optionally with an upper bound."
  (let ((direct-superclasses (closer-mop:class-direct-superclasses class)))
    (if direct-superclasses
      (remove-duplicates (loop for superclass in direct-superclasses
                               append (all-superclasses superclass (append collected-superclasses (list class)) :until until))
                         :test #'equalp)
      (if until
        (set-difference collected-superclasses (all-superclasses until))
        collected-superclasses))))

(defun immediate-subclasses (class)
  "Returns all immediate subclasses of a class"
  (if (not (closer-mop:class-direct-subclasses class))
    nil
    (loop for subclass in (closer-mop:class-direct-subclasses class)
          collect subclass)))

;(all-superclasses (find-class 'sugar) nil :until (find-class 'kitchen-entity))
;(immediate-subclasses (find-class 'bowl))

(defun make-ontology-vectors ()
  (let ((ontology-hash-table (make-hash-table))
        (ontological-classes (all-subclasses (find-class 'kitchen-entity))))
    (loop for class in ontological-classes
          for class-vector = (loop with super-classes = (all-superclasses class nil :until (find-class 'entity))
                                   for vector-class in ontological-classes
                                   if (find vector-class super-classes)
                                   collect 1
                                   else collect 0)
          do
          (setf (gethash (class-name class) ontology-hash-table) class-vector)
          finally (return ontology-hash-table))))

;(setf *vectors* (make-ontology-vectors))
;(cosine-similarity (ontological-vector 'broccoli *fcg-constructions*)
;                   (ontological-vector 'chopped-broccoli *fcg-constructions*))

    


