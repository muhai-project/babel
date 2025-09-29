(in-package :fcg)
;;(ql:quickload :fcg)


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
                        )
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
               (HASH form ((sequence "them" ?them-start ?them-end)))))
              :feature-types ((lex-class default)))


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
               (HASH form ((sequence "us" ?us-start ?us-end)))))
             :attributes (:label cxn :score 1.0))


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
               (lex-class cardinal)
               (meaning-args (?g))
               --
               (lex-class cardinal)
               (form-args (?cardinal-start ?cardinal-end)))
              (?of-unit
               --
               (HASH form ((sequence "of" ?of-start ?of-end))))
              (?pers-pronoun-unit
               (meaning-args (?g))
               (lex-class pers-pronoun)
               --
               (lex-class pers-pronoun)
               (agreement (person ?person)
                          (number pl)
                          (case accusative))
               (form-args (?pronoun-start ?pronoun-end)))
              (?the-two-of-them-unit
               (HASH meaning ((group ?g)))
               --
               (HASH form ((precedes ?the-end ?cardinal-start)
                           (precedes ?cardinal-end ?of-start)
                           (precedes ?of-end ?pronoun-start)))))
             :description "Specifying the number of the members of a group.")



;;(activate-monitor trace-fcg)

;;(comprehend-and-formulate "the two of them")
;;(comprehend-all "the four of us")



#|(def-fcg-cxn the-two-of-them-cxn
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
                           (precedes ?of-end ?them-start)))))))|#