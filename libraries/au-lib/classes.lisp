(in-package :au-lib)

(export '(#|anti-unification-result
          pattern source generalisation
          pattern-bindings source-bindings
          pattern-delta source-delta cost|#
          predicate-alignment-processor
          queue
          predicate-alignment-state
          pattern-predicates source-predicates
          pattern-remaining source-remaining))

#|(defclass anti-unification-result ()
  ((pattern
    :accessor pattern :initarg :pattern :initform nil)
   (source
    :accessor source :initarg :source :initform nil)
   (generalisation
    :accessor generalisation :initarg :generalisation :type list :initform nil)
   (pattern-bindings
    :accessor pattern-bindings :initarg :pattern-bindings :type list :initform nil)
   (source-bindings
    :accessor source-bindings :initarg :source-bindings :type list :initform nil)
   (pattern-delta
    :accessor pattern-delta :initarg :pattern-delta :type list :initform nil)
   (source-delta
    :accessor source-delta :initarg :source-delta :type list :initform nil)
   (cost
    :accessor cost :initarg :cost :type number :initform 0))
  (:documentation "Result of anti-unification consists of a generalisation,
                   delta's, bindings lists, and a cost."))|#


(defclass predicate-alignment-processor ()
  ((queue
    :accessor queue :initarg :queue :initform nil)))


(defclass predicate-alignment-state ()
  ((pattern-predicates
    :accessor pattern-predicates :initarg :pattern-predicates :initform nil)
   (source-predicates
    :accessor source-predicates :initarg :source-predicates :initform nil)
   (pattern-delta
    :accessor pattern-delta :initarg :pattern-delta :initform nil)
   (source-delta
    :accessor source-delta :initarg :source-delta :initform nil)
   (pattern-remaining
    :accessor pattern-remaining :initarg :pattern-remaining :initform nil)
   (source-remaining
    :accessor source-remaining :initarg :source-remaining :initform nil)))

