(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Class definitions for the fcg-learn package ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Constructions ;;
;;;;;;;;;;;;;;;;;;;

(defclass fcg-learn-cxn (fcg-construction)
  ()
  (:documentation "Class that groups all cxns learnt throug the fcg-learn system."))


(defclass holophrastic-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that are direct mappings between form and meaning predicates (no args, no cat)."))


(defclass filler-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that map between meaning predicates and provide a category and arguments to integrate with other cxns."))


(defclass linking-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that bind and/or percolate arguments of filler-cxns."))



;; Problems ;;
;;;;;;;;;;;;;;

(defclass fcg-learn-problem (problem)
  ()
  (:documentation "Class that groups fcg-learn problems.
Avoids the need for repair methods to  ever specialise on t - which
causes them to apply twice in notify-learning."))


(defclass gold-standard-not-in-search-space (fcg-learn-problem)
  ()
  (:documentation "Problem to be created when the cip does not contain
a node that matches the gold standard."))


(defclass gold-standard-elsewhere-in-search-space (fcg-learn-problem)
  ()
  (:documentation "Problem to be created when the cip contains a node that matches the gold standard, but where this node
is not the highest ranked solution node (best-solution)."))



;; Diagnostics ;;
;;;;;;;;;;;;;;;;;

(defclass fcg-learn-diagnostic (diagnostic)
  ()
  (:documentation "Class that groups fcg-learn diagnostics"))


(defclass diagnose-cip-against-gold-standard (fcg-learn-diagnostic)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Diagnostic called on cip after routine processing, checking solutions
against the gold standard and diagnosing gold-standard-not-in-search-space and gold-standard-not-in-search-space problems."))



;; Repairs ;;
;;;;;;;;;;;;;

(defclass fcg-learn-repair (repair)
  ()
  (:documentation "Class that groups fcg-learn repairs"))


(defclass repair-learn-holophrastic-cxn (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates a holophrastic construction - always works."))


(defclass repair-through-anti-unification (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates filler and/or linking cxns through anti-unification."))


(defclass repair-add-categorial-link (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates categorial links only."))



;; Fixes ;;
;;;;;;;;;;;

(defclass cxn-inventory-fix (fix)
  ((fixed-cars
    :accessor fixed-cars
    :initform nil
    :initarg :fixed-cars
    :type list
    :documentation "Slot that holds a list of construction application results that lead to a solution.")
   (speech-act
    :accessor speech-act
    :initform nil
    :initarg :speech-act
    :documentation "The speech act based on which that fix was created.")
   (cip
    :accessor cip
    :initform nil
    :initarg :cip
    :documentation "The cip that was passed to repair and is being fixed."))
  (:documentation "When applying sequentially the fix-cxns to the initial
transient structure given the categorial links, a solution should be found"))


(defclass holophrastic-fix (cxn-inventory-fix)
  ((fix-constructions
    :accessor fix-constructions
    :initform nil
    :initarg :fix-constructions
    :type list
    :documentation "Slot that holds the holophrastic cxn that was created."))
  (:documentation "Class for fixes created by repair-learn-holophrastic-cxn."))


(defclass anti-unification-fix (cxn-inventory-fix)
  ((fix-constructions
    :accessor fix-constructions
    :initform nil
    :initarg :fix-constructions
    :type list
    :documentation "Slot that holds the cxns that were created.")
   (fix-categories
    :accessor fix-categories
    :initform nil
    :initarg :fix-categories
    :type list
    :documentation "Slot that holds the categories that were created.")
   (fix-categorial-links
    :accessor fix-categorial-links
    :initform nil
    :initarg :fix-categorial-links
    :type alist
    :documentation "Slot that holds the categorial links that were created.")
   (base-cxns
    :initarg :base-cxns
    :accessor base-cxns
    :type list
    :documentation "Constructions used in the anti-unification process.")
   (anti-unification-state
    :initarg :anti-unification-state
    :accessor anti-unification-state
    :documentation "The solution state in the anti-unification process that led to this fix."))
  (:documentation "Class for fixes created by repair-through-anti-unification."))


(defclass categorial-link-fix (cxn-inventory-fix)
  ((fix-categorial-links
    :accessor fix-categorial-links
    :initform nil
    :initarg :fix-categorial-links
    :type alist
    :documentation "Slot that holds the categorial links that were created."))
  (:documentation "Class for fixes created by repair-add-categorial-link."))


;; Speech act ;;
;;;;;;;;;;;;;;;;

(defclass speech-act (blackboard)
  ((form
    :type string
    :accessor form
    :initform nil
    :initarg :form)
   (meaning
    :type list
    :accessor meaning
    :initform nil
    :initarg :meaning)
   (situation
    :accessor situation
    :initform nil
    :initarg :situation))
  (:documentation "Class that represents a speech act during a communicative interaction."))

(defmethod copy-object ((speech-act speech-act))
  speech-act)



;; Anti-unification search process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass au-repair-processor ()
  ((top-state 
    :type (or null au-repair-state) :initform nil :accessor top-state :initarg :top-state
    :documentation "The top state of the search process.")
   (all-states
    :type list :initform nil :initarg :all-states :accessor all-states
    :documentation "All states created.")
   (queue 
    :type list :initform nil :accessor queue :initarg :queue
    :documentation "All nodes left to be explored.")
   (state-counter
    :type number :initform 1 :accessor state-counter
    :documentation "A counter for the number of states in the tree.")
   (succeeded-states 
    :type list :initform nil :accessor succeeded-states
    :documentation "All succeeded states of the search process"))
  (:documentation "The state of a anti-unification search process for repairing problems."))



(defclass au-repair-state ()
  ((au-repair-processor
    :accessor au-repair-processor
    :initarg :au-repair-processor
    :documentation "The AU repair processor to which the state belongs.")
   (all-cxns
    :accessor all-cxns
    :initarg :all-cxns
    :initform nil
    :type list
    :documentation "All constructions from the cxn-inventory")
   (remaining-applicable-cxns
    :accessor remaining-applicable-cxns
    :initarg :remaining-applicable-cxns
    :initform nil
    :type list
    :documentation "Remaining constructions that could apply to the initial cfs.")
   (remaining-form-speech-act
    :accessor remaining-form-speech-act
    :initarg :remaining-form-speech-act
    :initform nil
    :type list
    :documentation "Remaining form from the speech act.")
   (remaining-meaning-speech-act
    :accessor remaining-meaning-speech-act
    :initarg :remaining-meaning-speech-act
    :initform nil
    :type list
    :documentation "Remaining meaning from the speech act.")
   (integration-cat
    :accessor integration-cat
    :initarg :integration-cat
    :initform nil
    :type list
    :documentation "Category that is expected to be contributed by child.")
   (integration-form-args
    :accessor integration-form-args
    :initarg :integration-form-args
    :initform nil
    :type list
    :documentation "Form args that are expected to be contributed by child.")
   (integration-meaning-args
    :accessor integration-meaning-args
    :initarg :integration-meaning-args
    :initform nil
    :type list
    :documentation "Meaning args that are expected to be contributed by child.")
   (fix-cxn-inventory
    :accessor fix-cxn-inventory
    :initarg :fix-cxn-inventory
    :initform nil
    :documentation "The construction inventory of the fix.")
   (new-cxns
    :accessor new-cxns
    :initform nil
    :documentation "Constructions that are created in this state.")
   (base-cxn
    :accessor base-cxn
    :initarg :base-cxn
    :initform nil
    :documentation "The base cxn that was used for anti-unifying.")
   (children
    :accessor children
    :initform nil
    :documentation "Children of the state.")
   (all-parents
    :accessor all-parents
    :initform nil
    :documentation "All foreparents of the state.")
   (au-result-form
    :accessor au-result-form
    :initform nil
    :initarg :au-result-form
    :documentation "Anti-unification result on the form side.")
   (au-result-meaning
    :accessor au-result-meaning
    :initform nil
    :initarg :au-result-meaning
    :documentation "Anti-unification result on the meaning side.")
   (created-at
    :accessor created-at
    :initarg :created-at
    :initform nil
    :documentation "Time stamp indicating when the state was added to the queue.")))
