(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?
;; Shall I ring the police?

;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)


;;(activate-monitor trace-fcg)

(def-fcg-constructions casa-grammar
  :feature-types ((form set-of-predicates)
                  (span sequence)
                  (syn-class set)
                  (constituents set)
                  (dependents set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (form-args sequence)
                  (meaning-args sequence)
                  (footprints set)
                  (syntactic-function set))
  :hierarchy-features (constituents dependents)

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
                        :connected-semantic-network ;; succeeds if the semantic network is fully connected
                        :only-empty-sequences-in-root
                        :connected-structure)
                       ;; goal tests for formulation
                       (:production-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :no-meaning-in-root ;; succeeds if no meaning predicates remain in root
                        )))


;;(comprehend-all "the two of them")
;;(comprehend-all "the four of us")

;;(comprehend "shall I ring the police?")
;;(comprehend "would you like tea or coffee?")


(def-fcg-cxn two-cxn
             (<-
              (?two-unit
               (meaning ((:quant ?x 2)))
               (lex-class cardinal)
               (meaning-args (?x))
               --
               (string "two"))))

(def-fcg-cxn four-cxn
             (<-
              (?four-unit
               (meaning ((:quant ?x 4)))
               (lex-class cardinal)
               (meaning-args (?x))
               --
               (string "four"))))

(def-fcg-cxn them-cxn
             (<-
              (?them-unit
               (meaning ((they ?t)))
               (lex-class pers-pronoun)
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
               (lex-class pers-pronoun)
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
               (lex-class cardinal))
              (?of-unit
               --
               (string "of"))
              (?pers-pronoun-unit
               (meaning-args (?g))
               --
               (lex-class pers-pronoun)
               (agreement (person ?person)
                          (number pl)
                          (case accusative)))))

(def-fcg-cxn ring-cxn
             (<-
              (?ring-unit
               (meaning ((ring.04 ?r)))
               (meaning-args (?r))
               (sem-roles (arg0 ?caller)
                          (arg1 ?called))
               (lex-class base-verb)
               (syntactic-form verb)
               --
               (string "ring"))))

(def-fcg-cxn I-cxn
             (<-
              (?i-unit
               (meaning ((i ?i)))
               (lex-class pers-pronoun)
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
               (lex-class modal-verb)
               (syntactic-function (pred-op))
               --
               (string "shall"))))

(def-fcg-cxn modal-cxn
             (<-
              (?modal-verb
               (meaning-args (?m))
               (sem-roles (arg1 ?event))
               --
               (lex-class modal-verb))
              (?base-verb
               (meaning-args (?event))
               --
               (lex-class base-verb))))

(def-fcg-cxn the-police-cxn
             (<-
              (?the-police-unit
               (meaning ((police ?p)))
               (syntactic-form np)
               (meaning-args (?p))
               --
               (string "the police"))))
         
(def-fcg-cxn yes-no-question-cxn
             (<-
              (?sentence
               (meaning ((:polarity ?pred amr-unknown)))
               --
               (string "?"))
              (?first-slot
               --
               (syntactic-function (pred-op)))
              (?second-slot
               --
               (syntactic-form np)
               (syntactic-function (subject)))
              (?third-slot
               --
               (meaning-args (?pred))
               (syntactic-function (pred-rest))))
             :description "Asking a question that can be answered by yes or no.")
 

(def-fcg-cxn monotransitive-cxn
             ((?vp-parent
               (meaning-args (?event))
               (syntactic-function (pred-rest)))
              <-
              (?slot-1-argument
               (meaning-args (?agent))
               (syntactic-function (potential subject))
               --
               (syntactic-form np))
              (?slot-2-predicate
               (sem-roles (arg0 ?agent)
                          (arg1 ?undergoer))
               (meaning ((:arg0 ?event ?agent)
                         (:arg1 ?event ?undergoer)))
               --
               (parent ?vp-parent)
               (meaning-args (?event))
               (syntactic-form verb))
              (?slot-3-argument
               (meaning-args (?undergoer))
               (syntactic-function (unit in predicate))
               --
               (syntactic-form np))))

(def-fcg-cxn you-cxn
             (<-
              (?you-unit
               (meaning ((you ?y)))
               (lex-class pers-pronoun)
               (syntactic-form np)
               (meaning-args (?y))
               (agreement (person 2)
                          (number ?numb)
                          (case nominative))
               --
               (string "you"))))

(def-fcg-cxn coffee-cxn
             (<-
              (?coffee-unit
               (meaning-args (?c))
               (lex-class noun)
               (meaning ((coffee ?c)))
               --
               (string "coffee"))))

(def-fcg-cxn tea-cxn
             (<-
              (?tea-unit
               (meaning-args (?t))
               (lex-class noun)
               (meaning ((tea ?t)))
               --
               (string "tea"))))


(def-fcg-cxn or-coordination-cxn
             ((?or-unit
               (meaning-args (?a))
               (syntactic-form np)
               (subunits (?first-slot ?second-slot)))
              <-
              (?first-slot
               (meaning-args (?slot-1-ref))
               --
               (lex-class noun))
              (?or-unit
               (meaning ((amr-choice ?a)
                         (:op1 ?a ?slot-1-ref)
                         (:op2 ?a ?slot-2-ref)))
               --
               (string "or"))
              (?second-slot
               (meaning-args (?slot-2-ref))
               --
               (lex-class noun))))

(def-fcg-cxn like-cxn
             (<-
              (?like-unit
               (meaning ((like.02 ?l)))
               (meaning-args (?l))
               (sem-roles (arg0 ?subject)
                          (arg1 ?phrasal-complement))
               (lex-class base-verb)
               (syntactic-form verb)
               --
               (string "like"))))

(def-fcg-cxn would-cxn
             (<-
              (?would-unit
               (lex-class modal-verb)
               (syntactic-function (pred-op))
               --
               (string "would"))))


(def-fcg-cxn here-cxn
             (<-
              (?here-unit
               (meaning ((here ?h)))
               (meaning-args (?h))
               (syntactic-form adverb)
               --
               (string "here"))))

(def-fcg-cxn she-cxn
             (<-
              (?she-unit
               (meaning ((she ?s)))
               (lex-class pers-pronoun)
               (syntactic-form np)
               (meaning-args (?s))
               (agreement (person 3)
                          (number sg)
                          (gender f)
                          (case nominative))
               --
               (string "she"))))      

(def-fcg-cxn has-done-cxn
             (<-
              (?done-unit
               (meaning ((do.02 ?d)))
               --
               (lemma do)
               (string "done"))
              (?has-unit
               (meaning-args (?d))
               (syntactic-form verb)
               --
               (string "has")
               (dependency-head ?done-unit))))

(def-fcg-cxn groundbreaking-neuroimaging-research-cxn
             (<-
              (?np-unit
               (meaning ((research.01 ?r)
                         (:arg1 ?r ?n)
                         (neuroimaging ?n)
                         (:mod ?r ?g)
                         (groundbreaking ?g)))
               (meaning-args (?r))
               (syntactic-form np)
               --
               (string "groundbreaking neuroimaging research"))))

(def-fcg-cxn it-cleft-cxn
             (
              <-
              (?first-slot
               (meaning-args (?t))
               (syntactic-function (subject))
               --
               (string "it"))
              (?second-slot
               (syntactic-function (V))
               (meaning ((be.01 ?b)
                         (:arg1 ?b ?t)
                         (:arg2 ?b ?c)))
               --
               (lemma be))
              (?third-slot
               (meaning ((:topic ?b ?c)))
               (syntactic-function (obj attr))
               --
               (meaning-args (?c))
               (syntactic-form adverb))
              (?where
               --
               (string "where")
               (parent ?fourth-slot))
              (?fourth-slot
               (syntactic-function (dependent-clause))
               --
               (syntactic-function (pred-rest))))
             :description "Highlight focused / new information to hearer.")


;It's here where she has done groundbreaking neuroimaging research (COCA-2014-SPOK)

;;(comprehend "it's here where she has done groundbreaking neuroimaging research")

