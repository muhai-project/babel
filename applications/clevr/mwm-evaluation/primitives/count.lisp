;;;; count.lisp

(in-package :mwm-evaluation)

;; -----------------
;; COUNT primtive ;;
;; -----------------
;; Count the number of objects in a given set

;(export '(count!))

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive count! ((target-num number)
                      (source-set mwm::mwm-object-set))
  ;; first case; given source-set, compute target
  ((source-set => target-num)
   (bind (target-num 1.0 (length (objects source-set)))))

  #|
  ;; second case; given source and target, check consistency
  ((source-set target-num =>)
   (= target-num (length (objects source-set))))
  |#
  :primitive-inventory *mwm-primitives*)

