;;;; nominal.lisp

(in-package :cgl)

;; ----------------------------------------------------- ;;
;; This file contains  grammatical constructions to      ;;
;; recursively construct nominals and to construct       ;;
;; determined noun phrases                               ;;
;; ----------------------------------------------------- ;;

;; base nominal will capture a noun (shape or thing)
;; and create a nominal. Pass on the ?number of the noun.
;; It will also introduce the 'filter' predicate.
(def-fcg-cxn base-nominal-cxn
             ((?nominal-unit
               (args ((sources ?source)
                      (target ?target)))
               (leftmost-unit ?noun-unit)
               (rightmost-unit ?noun-unit)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number ?number))
               (sem-cat (sem-class object))
               (subunits (?noun-unit))
               (superunits nil))
              (?noun-unit
               (superunits (?nominal-unit))
               (footprints (nominal)))
              <-
              (?nominal-unit
               (HASH meaning ((filter ?target ?source ?shape)))
               --
               )
              (?noun-unit
               (args ((target ?shape)))
               (sem-cat (sem-class shape))
               (footprints (NOT nominal))
               --
               (footprints (NOT nominal))
               (syn-cat (lex-class noun)
                        (syn-function nominal)
                        (number ?number))))
             :cxn-set (nom cxn)
             :cxn-inventory *CLEVR*)

;; nominal cxn will capture an existing nominal and
;; add an adjective in front.
;; this will also add a filter predicate for the adjective
;; and chain the filter inputs/outputs together
;; and keep passing on the ?number of the base nominal (shape)
(def-fcg-cxn nominal-cxn
             ((?super-nominal-unit
               (args ((sources ?source)
                      (target ?target)))
               (leftmost-unit ?adjective-unit)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?adjective-unit ?nominal-unit))
               (superunits nil)
               (syn-cat (lex-class np)
                        (syn-function nominal)
                        (number ?number))
               (sem-cat (sem-class object)))
              (?adjective-unit
               (footprints (nominal))
               (superunits (?super-nominal-unit)))
              (?nominal-unit
               (superunits (?super-nominal-unit))
               (footprints (nominal)))
              <-
              (?super-nominal-unit
               (HASH meaning ((filter ?target ?between ?category)))
               --
               )
              (?adjective-unit
               (args ((target ?category)))
               (sem-cat (sem-class ?class))
               (syn-cat (lex-class adjective))
               (footprints (NOT nominal))
               --
               (footprints (NOT nominal))
               (syn-cat (lex-class adjective)))
              (?nominal-unit
               (args ((sources ?source)
                      (target ?between)))
               ;(syn-cat (lex-class np)
               ;         (syn-function nominal)
               ;         (number ?number))
               (sem-cat (sem-class object))
               (footprints (NOT nominal))
               (superunits nil)
               --
               (superunits nil)
               (footprints (NOT nominal))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number ?number))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)
               (HASH form ((meets ?adjective-unit ?leftmost-nom-unit)))))
             :cxn-set (nom cxn)
             :cxn-inventory *CLEVR*)

;; unique-determined <- "the" + nominal
;; this takes the topmost nominal (no superunits) and a determiner
;; and creates a determined noun phrase. It adds the 'unique' predicate.
(def-fcg-cxn unique-determined-cxn
             ((?determined-noun-phrase-unit
               (subunits (?determiner-unit ?nominal-unit))
               (args ((sources ?source)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (definite +)
                        (number singular))
               (leftmost-unit ?determiner-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?nominal-unit
               (superunits (?determined-noun-phrase-unit)))
              <-
              (?determiner-unit
               (HASH meaning ((unique ?target-object ?target-set)))
               --
               (HASH form ((string ?determiner-unit "the")
                           (meets ?determiner-unit ?leftmost-nom-unit))))
              (?nominal-unit
               (args ((sources ?source)
                      (target ?target-set)))
               ;(syn-cat (syn-function nominal)
               ;         (lex-class np)
               ;         (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)))
             :cxn-set cxn
             :cxn-inventory *CLEVR*)

;; unique-declared <- "there is a" + nominal
;; this takes the topmost nominal (no superunits) and a determiner
;; and creates a determined noun phrase. It adds the 'unique' predicate.
(def-fcg-cxn unique-declared-cxn
             ((?determined-noun-phrase-unit
               (subunits (?declaration-unit ?nominal-unit))
               (args ((sources ?source)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (definite -)
                        (number singular))
               (leftmost-unit ?declaration-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?nominal-unit
               (superunits (?determined-noun-phrase-unit)))
              (root
               (footprints (there-is-a)))
              <-
              (root
               (footprints (NOT there-is-a))
               --
               (footprints (NOT there-is-a)))
              (?declaration-unit
               (HASH meaning ((unique ?target-object ?target-set)))
               --
               (HASH form ((string ?declaration-unit "there is a")
                           (meets ?declaration-unit ?leftmost-nom-unit))))
              (?nominal-unit
               (args ((sources ?source)
                      (target ?target-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)))
             :cxn-set cxn
             :cxn-inventory *CLEVR*)