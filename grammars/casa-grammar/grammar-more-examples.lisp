(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?
;; Shall I ring the police?

;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)

;;(defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;;(activate-monitor trace-fcg)
*fcg-constructions*
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

;;(extract-frames-from-utterance "the two of them")
;;(comprehend-all "the four of us")

;;(extract-frames-from-utterance "shall I ring the police?")
;;(extract-frames-from-utterance "would you like tea or coffee")

;;(pprint (extract-frames-from-utterance "it's here where she has done groundbreaking neuroimaging research."))

;;(extract-frames-from-utterance "it is here where I learned I used the wrong video drivers.")
;;(extract-frames-from-utterance "It is here where you decide what direction you want to go.")


(def-fcg-cxn two-cxn
             (<-
              (?two-unit
               (meaning ((:quant ?x 2)))
               (lex-class cd)
               (meaning-args (?x))
               --
               (string "two"))))

(def-fcg-cxn four-cxn
             (<-
              (?four-unit
               (meaning ((:quant ?x 4)))
               (lex-class cd)
               (meaning-args (?x))
               --
               (string "four"))))

(def-fcg-cxn them-cxn
             (<-
              (?them-unit
               (meaning ((they ?t)))
               (lex-class prp)
               (meaning-args (?t))
               (agreement (person 3)
                          (number pl)
                          (case accusative))
               --
               (string "them"))))

(def-fcg-cxn us-cxn
             (<-
              (?us-unit
               (meaning ((we ?w)))
               (lex-class prp)
               (meaning-args (?w))
               (agreement (person 1)
                          (number pl)
                          (case accusative))
               --
               (string "us"))))
             
(def-fcg-cxn the-CARD-of-PRON-cxn
             (<-
              (?the-unit
               --
               (string "the"))
              (?cardinal-unit
               (meaning ((group ?g)))
               (meaning-args (?g))
               (agreement (number pl)
                          (person ?person))
               --
               (lex-class cd))
              (?of-unit
               --
               (string "of"))
              (?pers-pronoun-unit
               (meaning-args (?g))
               --
               (lex-class prp)
               (agreement (person ?person)
                          (number pl)
                          (case accusative)))))

(def-fcg-cxn ring-cxn
             (<-
              (?ring-unit
               (frame-evoking +)
               (meaning ((ring.04 ?r)))
               (meaning-args (?r))
               (sem-roles (arg0 ?caller)
                          (arg1 ?called))
               (lex-class vb)
               (syntactic-form verb)
               --
               (string "ring"))))

(def-fcg-cxn I-cxn
             (<-
              (?i-unit
               (meaning ((i ?i)))
               (lex-class prp)
               (syntactic-form np)
               (meaning-args (?i))
               (agreement (person 1)
                          (number sg)
                          (case nominative))
               --
               (string "I"))))

(def-fcg-cxn shall-cxn
             (<-
              (?shall-unit
               (meaning ((recommend.01 ?r)
                         (:arg1 ?r ?recommendation)))
               (meaning-args (?r))
               (sem-roles (arg1 ?recommendation))
               (lex-class md)
               (syntactic-function (pred-op))
               --
               (string "shall"))))

(def-fcg-cxn modal-cxn
             (<-
              (?modal-verb
               (meaning-args (?m))
               (sem-roles (arg1 ?event))
               --
               (lex-class md))
              (?base-verb
               (meaning-args (?event))
               --
               (lex-class vb))))

(def-fcg-cxn police-cxn
             (<-
              (?the-police-unit
               (meaning ((police ?p)))
               (meaning-args (?p))
               --
               (lex-class nn)
               (string "police"))))
         
(def-fcg-cxn yes-no-question-cxn
             (<-
              (?first-slot
               --
               (syntactic-function (pred-op)))
              (?second-slot
               --
               (phrase-types (np))
               (syntactic-function (subject)))
              (?third-slot
               --
               (meaning-args (?pred))
               (syntactic-function (pred-rest)))
              (?sentence
               (meaning ((:polarity ?pred ?a)
                         (amr-unknown ?a)))
               --
               (string "?")))
             :description "Asking a question that can be answered by yes or no.")
 

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

(def-fcg-cxn you-cxn
             (<-
              (?you-unit
               (meaning ((you ?y)))
               (syntactic-form np)
               (meaning-args (?y))
               (agreement (person 2)
                          (number ?numb)
                          (case nominative))
               --
               (lex-class prp)
               (string "you"))))

(def-fcg-cxn coffee-cxn
             (<-
              (?coffee-unit
               (meaning-args (?c))
               
               (meaning ((coffee ?c)))
               --
               (lex-class nn)
               (string "coffee"))))

(def-fcg-cxn tea-cxn
             (<-
              (?tea-unit
               (meaning-args (?t))
               (meaning ((tea ?t)))
               --
               (lex-class nn)
               (string "tea"))))


(def-fcg-cxn or-coordination-cxn
             (
              <-
              (?first-slot
               (meaning-args (?a))
               (meaning ((amr-choice ?a)
                         (:op1 ?a ?first-slot)
                         (:op2 ?a ?second-slot)))
               --
               (lex-class nn)
               (dependents (?or-unit ?second-slot)))
              (?or-unit
               --
               (string "or"))
              (?second-slot
               --
               (lex-class nn))))

(def-fcg-cxn like-cxn
             (<-
              (?like-unit
               (frame-evoking +)
               (meaning ((like.02 ?l)))
               (meaning-args (?l))
               (sem-roles (arg0 ?subject)
                          (arg1 ?phrasal-complement))
               (syntactic-form verb)
               --
               (lex-class vb)
               (lemma like))))

(def-fcg-cxn would-cxn
             (<-
              (?would-unit
               (syntactic-function (pred-op))
               --
               (lex-class md)
               (string "would"))))


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
#|
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
               (dependents (?groundbreaking-unit ?neuroimaging-unit)))))|#

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


(def-fcg-cxn it-location-cleft-cxn
             (<-
              (?first-slot ;;it
                           (syntactic-function (subject))
                           --
                           (lemma it))
              (?second-slot ;;is
                            (syntactic-function (V))
                            (frame-evoking +)
                            (meaning ((be-located-at-91 ?b)
                                      (:arg2-of ?b ?h)
                                      (:arg1 ?b ?r)))
                            --
                            (lemma be)
                            (dependents (?first-slot ?third-slot ?fourth-slot)))
              (?third-slot ;;here
                           (syntactic-function (obj attr))
                           (meaning-args (?h))
                           --
                           (lex-class rb)
                           (lemma here))
              (?fourth-slot ;;where she has done X
                            (syntactic-function (dependent-clause))
                            (meaning-args (?r))
                            --
                            (dependency-label ccomp)
                            ))
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

;;(comprehend "it is here where I learned I used the wrong video drivers.")



;; (ql:quickload :ofef-parser)
;; (ofef-parser:fcg->ofef *fcg-constructions*)