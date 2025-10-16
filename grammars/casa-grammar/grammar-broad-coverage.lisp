(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?
;; Shall I ring the police?

;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)

;;(defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;;(activate-monitor trace-fcg)

(def-fcg-constructions casa-grammar
  :feature-types ((form set-of-predicates)
                  (span sequence)
                  (phrase-types set)
                  (constituents set)
                  (dependents set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (meaning-args sequence)
                  (footprints set)
                  (syntactic-function set))
  :hierarchy-features (dependents constituents )
  :fcg-configurations (
                       ;; --- (DE)RENDER ---
                       (:de-render-mode . :de-render-constituents-dependents)
                       (:render-mode . :render-sequences)

                       ;; --- HEURISTICS ---
                       ;; use dedicated cip
                       (:construction-inventory-processor-mode . :heuristic-search)
                       ;; always fully expands node immediately
                       (:node-expansion-mode . :full-expansion)
                       ;; returns all cxns at once
                       (:cxn-supplier-mode . :all-cxns)
                       ;; for using heuristics (alternatives: :depth-first, :breadth-first :random)
                       (:search-algorithm . :best-first)
                       ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched)
                       ;; how to use results of heuristic functions for scoring a node
                       (:heuristic-value-mode . :sum-heuristics-and-parent) 

                       ;; --- GOAL TESTS ---
                       ;; goal tests for comprehension
                       (:parse-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        ))) ;; succeeds if the semantic network is fully connected))) ;:connected-semantic-network


;;(extract-frames-from-utterance "it is here where I learned I used the wrong video drivers.")
;;(extract-frames-from-utterance "It is here where you decide what direction you want to go.")


#|
(def-fcg-cxn monotransitive-cxn
             ((?vp-parent
               (meaning-args (?event))
               (syntactic-function (pred-rest)))
              (?slot-2-predicate
               (footprints (arg-structure-cxn)))
              <-
              (?slot-1-argument
               (meaning-args (?agent))
               (syntactic-function (potential subject))
               --
               (dependency-label nsubj)
               (dependency-head ?slot-2-predicate))
              (?slot-2-predicate
               (sem-roles (arg0 ?agent)
                          (arg1 ?undergoer))
               (meaning ((:arg0 ?event ?agent)
                         (:arg1 ?event ?undergoer)))
               --
               (footprints (not arg-structure-cxn))
               (parent ?vp-parent)
               (meaning-args (?event))
               (syntactic-form verb))
              (?slot-3-argument
               (meaning-args (?undergoer))
               (syntactic-function (unit in predicate))
               --
               ;(phrase-types (np))
               (dependency-label dobj)
               (dependency-head ?slot-2-predicate)
               )
              :disable-automatic-footprints t))

(def-fcg-cxn here-cxn
             (<-
              (?here-unit
               (meaning ((here ?h)))
               (meaning-args (?h))
               --
               (lex-class rb)
               (string "here"))))

(def-fcg-cxn she-cxn
             (<-
              (?she-unit
               (meaning ((she ?s)))
               (syntactic-form np)
               (meaning-args (?s))
               (agreement (person 3)
                          (number sg)
                          (gender f)
                          (case nominative))
               --
               (lex-class prp)
               (string "she"))))      

(def-fcg-cxn has-done-lv-cxn
             (<-
              (?has-unit
               (syntactic-form verb)
               --
               (string "has")
               (dependency-label aux)
               (dependency-head ?done-unit))
              (?done-unit
               (syntactic-form verb)
               (meaning-args (?event))
               --
               (string "done")
               (lex-class vbn))
              (?nominalisation-unit
               --
               (lex-class nn)
               (dependency-head ?done-unit)
               (meaning-args (?event)))))

(def-fcg-cxn groundbreaking-neuroimaging-research-cxn
             (<-
              (?np-unit
               --
               (constituents (?groundbreaking-unit ?neuroimaging-unit ?research-unit))
               (string "groundbreaking neuroimaging research"))
              (?research-unit
               (meaning-args (?r))
               (frame-evoking +)
               (meaning ((research-01 ?r)
                         (:arg1 ?r ?n)
                         (neuroimaging ?n)
                         (:mod ?r ?g)
                         (groundbreaking ?g)))
               --
               (dependents (?groundbreaking-unit ?neuroimaging-unit)))))

(def-fcg-cxn intransitive-cxn
             ((?slot-2-predicate
               (footprints (arg-structure-cxn)))
              <-
              (?slot-1-argument
               (syntactic-function (potential subject))
               --
               (meaning-args (?agent))
               (dependency-label nsubj)
               (dependency-head ?slot-2-predicate)
               (syntactic-form np))
              (?slot-2-predicate
               (sem-roles (arg0 ?agent))
               (meaning ((:arg0 ?event ?agent)))
               --
               (footprints (not arg-structure-cxn))
               (meaning-args (?event))
               (syntactic-form verb)))
             :disable-automatic-footprints t)

|#
(def-fcg-cxn it-cleft-location-cxn
             ((?first-slot
               (syntactic-function (subject)))
              (?second-slot
               (syntactic-function (V))
               (frame-evoking +))
              (?third-slot
               (semantic-function focus)
               (syntactic-function (obj attr)))
              (?fourth-slot
                (syntactic-function (dependent-clause))
                            (meaning-args (?r)))
              <-
              (?first-slot
               --
               (lemma it))
              (?second-slot
               (HASH meaning ((be-located-at-91 ?b)
                              (:arg2-of ?b ?h)
                              (:arg1 ?b ?r)))
               --
               (lemma be)
               (dependents (?first-slot ?third-slot ?fourth-slot)))
              (?third-slot
               (meaning-args (?h))
               --
               (lex-class rb)
               (lemma here))
              (?fourth-slot
               (meaning-args (?r))
               --
               (dependency-label ccomp)))
             :description "Highlight focused / new information to hearer.")


;; It's here where she has done groundbreaking neuroimaging research (COCA-2014-SPOK)

#|'(h / here
    :arg2-of (b / be-located-at-91
                :arg1 (r / research.01
                         :arg1 (n / neuroimaging)
                         :mod (g / groundbreaking)
                         :arg0 (s / she))))|#

;;((FCG::HERE FCG::H) (FCG::BE-LOCATED-AT-91 UTILS:B) (FCG::RESEARCH.01 FCG::R) (FCG::NEUROIMAGING FCG::N) (FCG::GROUNDBREAKING FCG::G) (FCG::SHE FCG::S) (:ARG2-OF FCG::H UTILS:B) (:ARG1 UTILS:B FCG::R) (:ARG1 FCG::R FCG::N) (:MOD FCG::R FCG::G) (:ARG0 FCG::R FCG::S))


;;(comprehend-all "it's here where she has done groundbreaking neuroimaging research.")

;;(pprint (extract-frames-from-utterance "it is here where I learned I used the wrong video drivers."))



;; (ql:quickload :ofef-parser)
;; (ofef-parser:fcg->ofef *fcg-constructions*)

(defparameter *cleft-mini-corpus*
  '("Like most dumps, it's smelly and dirty, but it is here where the artists find a treasure trove of materials."
    "It is here where he introduced to the world the term, \" socialism with Chinese characteristics.\" "
    "It is here where Cal, thirty-one, is sipping beer in a backyard and gazing toward the crowd."
    "If there is a ground zero, it is here where Gray lived, Sandtown in West Baltimore, pockmarked by vacant buildings struggling with higher-than-average unemployment and poverty and a robust heroin market."
    "Known as Bo tes, it is here where humans have already begun their renaissance."
    "When detailing case studies, forest policies, and macroeconomic trends, it is here where Hyde's effort truly shines."
    "It is here where you decide what direction you want to go; to spend eternity with Jesus or to a burning hell."
    "It is here where I learned I used the wrong video drivers, so I had to uninstall the old ones and reinstall a different package."))

(loop for construct in *cleft-mini-corpus*
      do (pprint (extract-frames-from-utterance construct)))
      
