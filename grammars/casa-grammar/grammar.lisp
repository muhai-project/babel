(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?

#|(amr::penman->predicates '(l / like-01
                               :MOD (d/ deontic)
                               :ARG0 (y / you)
                               :ARG1 (a / amr-choice
                                        :op1 (t / tea)
                                        :op2 (c / coffee))))|#

;((FCG::LIKE-01 FCG::L) (:MOD L D) (DEONTIC D) (FCG::YOU UTILS:Y) (FCG::AMR-CHOICE UTILS:A) (FCG::TEA T) (FCG::COFFEE FCG::C) (:ARG0 FCG::L UTILS:Y) (:ARG1 FCG::L UTILS:A) (:OP1 UTILS:A T) (:OP2 UTILS:A FCG::C))


;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)


;;(activate-monitor trace-fcg)

(def-fcg-constructions casa-grammar
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (subunits set)
                  (form-args sequence)
                  (meaning-args sequence)
                  (footprints set)
                  (syntactic-function set))
  :fcg-configurations (
                       ;; --- (DE)RENDER ---
                       (:de-render-mode . :de-render-sequence-predicates)
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


(def-fcg-cxn two-cxn
             ((?two-unit
               (lex-class cardinal)
               (meaning-args (?x))
               (form-args (?two-start ?two-end)))
              <-
              (?two-unit
               (HASH meaning ((:quant ?x 2)))
               --
               (HASH form ((sequence "two" ?two-start ?two-end))))))


(def-fcg-cxn four-cxn
             ((?four-unit
               (lex-class cardinal)
               (meaning-args (?x))
               (form-args (?four-start ?four-end)))
              <-
              (?four-unit
               (HASH meaning ((:quant ?x 4)))
               --
               (HASH form ((sequence "four" ?four-start ?four-end))))))

(def-fcg-cxn them-cxn
             ((?them-unit
               (lex-class pers-pronoun)
               (meaning-args (?t))
               (form-args (?them-start ?them-end))
               (agreement (person 3)
                          (number pl)
                          (case accusative)))
              <-
              (?them-unit
               (HASH meaning ((they ?t)))
               --
               (HASH form ((sequence "them" ?them-start ?them-end))))))


(def-fcg-cxn us-cxn
             ((?us-unit
               (lex-class pers-pronoun)
               (meaning-args (?w))
               (form-args (?us-start ?us-end))
               (agreement (person 1)
                          (number pl)
                          (case accusative)))
              <-
              (?us-unit
               (HASH meaning ((we ?w)))
               --
               (HASH form ((sequence "us" ?us-start ?us-end))))))


(def-fcg-cxn the-CARD-of-PRON-cxn
             ((?the-two-of-them-unit
               (subunits (?the-unit ?cardinal-unit ?of-unit ?pers-pronoun-unit ))
               (agreement (number pl)
                          (person ?person)))
              <-
              (?the-unit
               --
               (HASH form ((sequence "the" ?the-start ?the-end))))
              (?cardinal-unit
               (meaning-args (?g))
               --
               (lex-class cardinal)
               (form-args (?cardinal-start ?cardinal-end)))
              (?of-unit
               --
               (HASH form ((sequence "of" ?of-start ?of-end))))
              (?pers-pronoun-unit
               (meaning-args (?g))
               --
               (lex-class pers-pronoun)
               (agreement (person ?person)
                          (number pl)
                          (case accusative))
               (form-args (?pronoun-start ?pronoun-end)))
              (?the-two-of-them-unit
               (HASH meaning ((group ?g))
               --
               (HASH form ((precedes ?the-end ?cardinal-start)
                           (precedes ?cardinal-end ?of-start)
                           (precedes ?of-end ?pronoun-start)))))))


(def-fcg-cxn the-two-of-them-cxn
             ((?the-two-of-them-unit
               (subunits (?the-unit ?cardinal-unit ?of-unit ?pers-pronoun-unit ))
               (agreement (number pl)
                          (person 3)))
              <-
              (?the-unit
               --
               (HASH form ((sequence "the" ?the-start ?the-end))))
              (?cardinal-unit
               --
               (HASH form ((sequence "two" ?two-start ?two-end))))
              (?of-unit
               --
               (HASH form ((sequence "of" ?of-start ?of-end))))
              (?pers-pronoun-unit
               --
               (HASH form ((sequence "them" ?them-start ?them-end))))
              (?the-two-of-them-unit
               (HASH meaning ((group ?g)
                              (:quant ?g 2))
               --
               (HASH form ((precedes ?the-end ?two-start)
                           (precedes ?two-end ?of-start)
                           (precedes ?of-end ?them-start)))))))


;;(comprehend-all "the two of them")
;; (comprehend-all "the four of us")


(def-fcg-cxn ring-cxn
             ((?ring-unit
               (meaning-args (?r))
               (form-args (?start ?end))
               (sem-roles (arg0 ?caller)
                          (arg1 ?called))
               (lex-class base-verb)
               (syntactic-form verb))
              <-
              (?ring-unit
               (HASH meaning ((ring.04 ?r)))
               --
               (HASH form ((sequence "ring" ?start ?end))))))

(def-fcg-cxn I-cxn
             ((?i-unit
               (lex-class pers-pronoun)
               (syntactic-form np)
               (meaning-args (?i))
               (form-args (?start ?end))
               (agreement (person 1)
                          (number sg)
                          (case nominative)))
              <-
              (?i-unit
               (HASH meaning ((i ?i)))
               --
               (HASH form ((sequence "I" ?start ?end))))))

(def-fcg-cxn shall-cxn
             ((?shall-unit
               (meaning-args (?r))
               (form-args (?start ?end))
               (sem-roles (arg1 ?recommendation))
               (lex-class modal-verb)
               (syntactic-function pred-op))
              <-
              (?shall-unit
               (HASH meaning ((recommend.01 ?r)
                              (:arg1 ?r ?recommendation)))
               --
               (HASH form ((sequence "shall" ?start ?end))))))

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
               (lex-class base-verb)
               (HASH form ((precedes ?modal-verb ?base-verb))))))

(def-fcg-cxn the-police-cxn
             ((?the-police-unit
               (syntactic-form np)
               (meaning-args (?p))
               (form-args (?start ?end)))
              <-
              (?the-police-unit
               (HASH meaning ((police ?p)))
               --
               (HASH form ((sequence "the police" ?start ?end))))))
         
(def-fcg-cxn yes-no-question-cxn
             ((?sentence
               (subunits (?slot-1 ?slot-2 ?slot-3)))
              <-
              (?sentence
               (HASH meaning ((:polarity ?pred amr-unknown)))
               --
               (HASH form ((sequence "?" ?question-mark-start ?question-mark-end)
                           (precedes ?end-1 ?start-2)
                           (precedes ?end-2 ?start-3)
                           (precedes ?end-3 ?question-mark-start))))
              (?slot-1
               --
               (syntactic-function pred-op)
               (form-args (?start-1 ?end-1)))
              (?slot-2
               --
               (syntactic-form np)
               (syntactic-function (subject))
               (form-args (?start-2 ?end-2)))
              (?slot-3
               --
               (meaning-args (?pred))
               (syntactic-function pred-rest)
               (form-args (?start-3 ?end-3))))
             :description "Asking a question that can be answered by yes or no.")
 

(def-fcg-cxn monotransitive-cxn
             ((?slot-2-predicate
               (subunits (?slot-3-argument))
               (syntactic-function pred-rest)
               (footprints (monotransitive-cxn)))
              <-
              (?slot-1-argument
               (meaning-args (?agent))
               (syntactic-function (potential subject))
               --
               (syntactic-form np))
              (?slot-2-predicate
               (meaning-args (?event))
               (sem-roles (arg0 ?agent)
                          (arg1 ?undergoer))
               (HASH meaning ((:arg0 ?event ?agent)
                              (:arg1 ?event ?undergoer)))
               --
               (syntactic-form verb))
              (?slot-3-argument
               (meaning-args (?undergoer))
               (syntactic-function (unit in predicate))
               --
               (syntactic-form np))))


;;(comprehend "shall I ring the police?")



(def-fcg-cxn you-cxn
             ((?you-unit
               (lex-class pers-pronoun)
               (syntactic-form np)
               (meaning-args (?y))
               (form-args (?start ?end))
               (agreement (person 2)
                          (number ?numb)
                          (case nominative)))
              <-
              (?you-unit
               (HASH meaning ((you ?y)))
               --
               (HASH form ((sequence "you" ?start ?end))))))

(def-fcg-cxn coffee-cxn
             ((?coffee-unit
               (meaning-args (?c))
               (lex-class noun)
               (form-args (?start ?end)))
              <-
              (?coffee-unit
               (HASH meaning ((coffee ?c)))
               --
               (HASH form ((sequence "coffee" ?start ?end))))))

(def-fcg-cxn tea-cxn
             ((?tea-unit
               (meaning-args (?t))
               (lex-class noun)
               (form-args (?start ?end)))
              <-
              (?tea-unit
               (HASH meaning ((tea ?t)))
               --
               (HASH form ((sequence "tea" ?start ?end))))))


(def-fcg-cxn or-coordination-cxn
             ((?or-unit
               (meaning-args (?a))
               (syntactic-form np)
               (form-args (?first-slot-start ?second-slot-end))
               (subunits (?first-slot-unit ?second-slot-unit)))
              <-
              (?first-slot-unit
               (meaning-args (?slot-1-ref))
               --
               (lex-class noun)
               (form-args (?first-slot-start ?first-slot-end)))
              (?or-unit
               (HASH meaning ((amr-choice ?a)
                              (:op1 ?a ?slot-1-ref)
                              (:op2 ?a ?slot-2-ref)))
               --
               (HASH form ((sequence "or" ?or-start ?or-end)
                           (precedes ?first-slot-end ?or-start)
                           (precedes ?or-end ?second-slot-start))))
              (?second-slot-unit
               (meaning-args (?slot-2-ref))
               --
               (lex-class noun)
               (form-args (?second-slot-start ?second-slot-end)))))

(def-fcg-cxn like-cxn
             ((?like-unit
               (meaning-args (?l))
               (form-args (?start ?end))
               (sem-roles (arg0 ?subject)
                          (arg1 ?phrasal-complement))
               (lex-class base-verb)
               (syntactic-form verb))
              <-
              (?like-unit
               (HASH meaning ((like.02 ?l)))
               --
               (HASH form ((sequence "like" ?start ?end))))))

(def-fcg-cxn would-cxn
             ((?would-unit
               (form-args (?start ?end))
               (lex-class modal-verb)
               (syntactic-function pred-op))
              <-
              (?would-unit
               --
               (HASH form ((sequence "would" ?start ?end))))))

;;(comprehend "would you like tea or coffee?")
;;(comprehend-and-formulate "would you like tea or coffee?")
