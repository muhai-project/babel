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

(def-fcg-constructions casa-grammar
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (subunits set)
                  (form-args sequence)
                  (meaning-args sequence)
                  (footprints set))
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
                        )
                       ;; goal tests for formulation
                       (:production-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :no-meaning-in-root ;; succeeds if no meaning predicates remain in root
                        ))


(def-fcg-cxn coffee-cxn
             ((?coffee-unit
               (referent ?c)
               (category noun)
               (form-args (?start ?end)))
              <-
              (?coffee-unit
               (HASH meaning ((coffee ?c)))
               --
               (HASH form ((sequence "coffee" ?start ?end))))))

(def-fcg-cxn tea-cxn
             ((?tea-unit
               (referent ?c)
               (category noun)
               (form-args (?start ?end)))
              <-
              (?tea-unit
               (HASH meaning ((tea ?c)))
               --
               (HASH form ((sequence "tea" ?start ?end))))))

(def-fcg-cxn or-coordination-cxn
             ((?or-unit
               (referent ?a)
               (phrase-type np)
               (form-args (?first-slot-start ?second-slot-end))
               (subunits (?first-slot-unit ?second-slot-unit)))
              <-
              (?first-slot-unit
               (referent ?slot-1-ref)
               --
               (category noun)
               (form-args (?first-slot-start ?first-slot-end)))
              (?or-unit
               (HASH meaning (
                              (:op1 ?a ?slot-1-ref)
                              (:op2 ?a ?slot-2-ref)))
               --
               (HASH form ((sequence " or " ?or-start ?or-end)
                           (precedes ?first-slot-end ?or-start)
                           (precedes ?or-end ?second-slot-start))))
              (?second-slot-unit
               (referent ?slot-2-ref)
               --
               (category noun)
               (form-args (?second-slot-start ?second-slot-end)))))

(def-fcg-cxn like-cxn
             ((?like-unit
               (referent ?l)
               (category verb)
               (finite -)
               (form-args (?start ?end)))
              <-
              (?like-unit
               (HASH meaning ((like ?l)))
               --
               (HASH form ((sequence "like" ?start ?end))))))

(def-fcg-cxn would-cxn
             ((?would-unit
               (referent ?d)
               (category aux)
               (modal +)
               (form-args (?start ?end)))
              <-
              (?would-unit
               (HASH meaning ((deontic ?d)))
               --
               (HASH form ((sequence "would" ?start ?end))))))

(def-fcg-cxn modal-cxn
             ((?base-verb-unit
               (footprints (modal)))
              <-
              (?modal-aux-unit
               (referent ?mod)
               (category aux)
               (modal +)
               --
               (referent ?mod)
               (category aux)
               (modal +)
               (form-args (?start-mod ?end-mod)))
              (?base-verb-unit
               (referent ?event)
               (category verb)
               (finite -)
               (HASH meaning ((:mod ?event ?mod)))
               (footprints (not modal))
               --
               (footprints (not modal))
               (referent ?event)
               (category verb)
               (finite -)
               (form-args (?start-base-v ?end-base-v))
               (HASH form ((precedes ?end-mod ?start-base-v)))))
             :disable-automatic-footprints t)

(def-fcg-cxn you-cxn
             ((?you-unit
               (referent ?y)
               (phrase-type np)
               (category pron)
               (form-args (?start ?end)))
              <-
              (?you-unit
               (HASH meaning ((you ?y)))
               --
               (HASH form ((sequence "you" ?start ?end))))))

(def-fcg-cxn monotransitive-cxn
             ((?predicate-unit
               (subunits (?verb-unit ?object-unit))
               (referent ?event)
               (meaning-args (?agent ?patient))
               (form-args (?verb-start ?object-end))
               (clause-type monotransitive))
              <-
              (?subject-unit
               (referent ?agent)
               (phrase-type np)
               --
               (phrase-type np)
               (form-args (?subject-start ?subject-end)))
              (?verb-unit
               (category verb)
               (referent ?event)
               (HASH meaning ((:arg0 ?event ?agent)
                              (:arg1 ?event ?patient)))
               --
               (form-args (?verb-start ?verb-end))
               (category verb)
               (finite -))
              (?predicate-unit
               
               --
               (HASH form ((sequence " " ?subject-end ?verb-start)
                           (sequence " " ?verb-end ?object-start))))
              (?object-unit
               (referent ?patient)
               (phrase-type np)
               --
               (phrase-type np)
               (form-args (?object-start ?object-end))))
             )
              
(def-fcg-cxn alternative-question-cxn
             ((?sentence
               (subunits (?operator-verb ?subject ?predicate-rest)))
              <-
              (?operator-verb
               (category aux)
               (form-args (?start-mod ?end-mod))
               --
               (category aux)
               (form-args (?start-mod ?end-mod)))
              (?subject
               (referent ?arg0)
               (phrase-type np)
               --
               (phrase-type np)
               (form-args (?start-subj ?end-subj))
               )
              (?predicate-rest
               (clause-type monotransitive)
               (meaning-args (?arg0 ?arg1))
               --
               
               (meaning-args (?arg0 ?arg1))
               (form-args (?start-pred ?end-pred)))
              (?sentence
               (HASH meaning ((amr-choice ?arg1)))
               --
               (HASH form ((sequence " " ?end-mod ?start-subj)
                           (sequence "?" ?end-pred ?end-question-mark))))))
              
)

;;(activate-monitor trace-fcg)
(comprehend "would you like tea or coffee?")
(comprehend-and-formulate "would you like tea or coffee?")

