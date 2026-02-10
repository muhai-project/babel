(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?
;; Shall I ring the police?

;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)

;;(activate-monitor trace-fcg)

(def-fcg-constructions casa-grammar
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (span sequence)
                  (subunits set)
                  (meaning set-of-predicates)
                  (meaning-args sequence)
                  (footprints set)
                  (syntactic-function set))
  :hierarchy-features (subunits)
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
                       (:initial-categorial-link-weight . 1.0)
                       ;; --- GOAL TESTS ---
                       ;; goal tests for comprehension
                       (:parse-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :only-empty-sequences-in-root
                        :connected-structure
                        :connected-semantic-network))) ;; succeeds if the semantic network is fully connected)))

;;(comprehend "would you like tea or coffee?")
;;(add-element (make-html (categorial-network *fcg-constructions*)))


(def-fcg-cxn yes-no-question-cxn
             ((?sentence
               (subunits (?first-slot ?second-slot ?third-slot)))
              <-
              (?first-slot
               --
               (agreement (number ?nb)
                          (person ?pers))
               (span (?start-first-slot ?end-first-slot))
               (syntactic-function (pred-op)))
              (?second-slot
               --
               (syntactic-form np)
               (syntactic-function (subject))
               (span (?start-second-slot ?end-second-slot))
               (agreement (number ?nb)
                          (person ?pers)))
              (?third-slot
               --
               (meaning-args (?pred))
               (span (?start-third-slot ?end-third-slot))
               (syntactic-function (pred-rest)))
              (?sentence
               (HASH meaning ((:polarity ?pred ?a)
                              (amr-unknown ?a)))
               --
               (HASH form ((sequence "?" ?start-question-mark ?end-question-mark)
                           (precedes ?end-first-slot ?start-second-slot)
                           (precedes ?end-second-slot ?start-third-slot)
                           (precedes ?end-third-slot ?start-question-mark)))))
             :description "Asking a question that can be answered by yes or no."
             :attributes (:label sentence-type-cxn :score 1.0))
 

(def-fcg-cxn monotransitive-cxn
             ((?pred-rest-unit
               (meaning-args (?event))
               (subunits (?slot-2-predicate ?slot-3-argument))
               (span (?slot-2-start ?slot-3-end)))
              (?slot-1-argument
               (syntactic-function (potential subject)))
              (?slot-3-argument
               (syntactic-function (unit in predicate)))
              <-
              (?slot-1-argument
               (meaning-args (?aeffector))
               (category monotransitive-slot-1-cat)
               --
               (syntactic-form np)
               (category monotransitive-slot-1-cat)
               (span (?slot-1-start ?slot-1-end)))
              (?slot-2-predicate
               (meaning-args (?event))
               (sem-roles (arg0 ?aeffector)
                          (arg1 ?aeffected))
               (HASH meaning ((:arg0 ?event ?aeffector)
                              (:arg1 ?event ?aeffected)))
               --
               (lex-class vb)
               (category monotransitive-slot-2-cat)
               (span (?slot-2-start ?slot-2-end))
               )
              (?slot-3-argument
               (meaning-args (?aeffected))
               (category monotransitive-slot-3-cat)
               --
               (syntactic-form np)
               (category monotransitive-slot-3-cat)
               (span (?slot-3-start ?slot-3-end)))
              (?pred-rest-unit
               (syntactic-function (pred-rest))
               --
               (HASH form ((precedes ?slot-1-end ?slot-2-start)
                           (precedes ?slot-2-end ?slot-3-start)))))
             :description "An aeffector does something to an
aeffected. Examples: <i> She kissed him. </i>; <i> He opened the
window. </i>"
             :attributes (:label argument-structure-cxn :score 1.0))

(def-fcg-cxn you-cxn
             ((?you-unit
               (lex-class prp)
               (syntactic-form np)
               (category you-cxn-cat)
               (meaning-args (?y))
               (span (?start ?end))
               (agreement (person 2)
                          (number ?numb)
                          (case nominative)))
              <-
              (?you-unit
               (syntactic-form np)
               (HASH meaning ((you ?y)))
               --
               (HASH form ((sequence "you" ?start ?end)))))
             :attributes (:label np-cxn :score 1.0))

(def-fcg-cxn coffee-cxn
             ((?coffee-unit
               (meaning-args (?c))
               (span (?start ?end))
               (lex-class nn)
               (category coffee-cxn-cat))
              <-
              (?coffee-unit
               (HASH meaning ((coffee ?c)))
               --
               (HASH form ((sequence "coffee" ?start ?end))))))

(def-fcg-cxn mass-np-cxn
             ((?np-unit
               (meaning-args (?n))
               (syntactic-form np)
               (subunits (?noun-unit))
               (span (?start ?end))
               (category mass-np-cxn-cat))
              <-
              (?noun-unit
               (meaning-args (?n))
               (category mass-noun-slot-cat)
               --
               (span (?start ?end))
               (category mass-noun-slot-cat))))

(def-fcg-cxn tea-cxn
             ((?tea-unit
               (meaning-args (?t))
               (span (?start ?end))
               (lex-class nn)
               (category tea-cxn-cat))
              <-
              (?tea-unit
               (HASH meaning ((tea ?t)))
               --
               (HASH form ((sequence "tea" ?start ?end))))))

(def-fcg-cxn or-coordination-cxn
             ((?or-unit
               (meaning-args (?a))
               (syntactic-form np)
               (span (?start-1 ?end-2))
               (category or-coordination-cxn-cat)
               (subunits (?first-slot ?second-slot)))
              <-
              (?first-slot
               (meaning-args (?slot-1-ref))
               (category or-coordination-first-slot-cat)
               --
               (syntactic-form np)
               (category or-coordination-first-slot-cat)
               (span (?start-1 ?end-1)))
              (?or-unit
               (HASH meaning ((amr-choice ?a)
                              (:op1 ?a ?slot-1-ref)
                              (:op2 ?a ?slot-2-ref)))
               --
               (HASH form ((sequence " or " ?end-1 ?start-2))))
              (?second-slot
               (meaning-args (?slot-2-ref))
               (category or-coordination-second-slot-cat)
               --
               (syntactic-form np)
               (category or-coordination-second-slot-cat)
               (span (?start-2 ?end-2)))))

(def-fcg-cxn like-cxn
             ((?like-unit
               (meaning-args (?l))
               (category like-cxn-cat)
               (span (?start ?end))
               (lex-class vb)
               (sem-roles (arg0 ?liker)
                          (arg1 ?object-of-affection)))
              <-
              (?like-unit
               (HASH meaning ((like-01 ?l)))
               --
               (HASH form ((sequence "like" ?start ?end))))))

(def-fcg-cxn would-cxn
             ((?would-unit
               (lex-class md)
               (span (?start ?end)))
              <-
              (?would-unit
               (syntactic-function (pred-op))
               (agreement (person ?pers)
                          (number ?nb))
               --
               (HASH form ((sequence "would" ?start ?end)))))
             :attributes (:label modal-cxn))

(def-fcg-cxn modal-cxn
             (<-
              (?modal-verb-unit
               (lex-class md)
               --
               (lex-class md)
               (span (?start-md ?end-md)))
              (?base-verb-unit
               (meaning-args (?event))
               (lex-class vb)
               --
               (lex-class vb)
               (span (?start-vb ?end-vb))
               (HASH form ((precedes ?end-md ?start-vb))))))


(add-categories '(coffee-cxn-cat tea-cxn-cat mass-np-cxn-cat
                                 mass-noun-slot-cat you-cxn-cat
                                 like-cxn-cat or-coordination-first-slot-cat
                                 or-coordination-second-slot-cat monotransitive-slot-1-cat
                                 monotransitive-slot-2-cat monotransitive-slot-3-cat
                                 or-coordination-cxn-cat)
                *fcg-constructions*)

(add-link 'coffee-cxn-cat 'mass-noun-slot-cat *fcg-constructions*)
(add-link 'tea-cxn-cat 'mass-noun-slot-cat *fcg-constructions*)
(add-link 'mass-np-cxn-cat 'or-coordination-first-slot-cat *fcg-constructions*)
(add-link 'mass-np-cxn-cat 'or-coordination-second-slot-cat *fcg-constructions*)
(add-link 'like-cxn-cat 'monotransitive-slot-2-cat *fcg-constructions*)
(add-link 'or-coordination-cxn-cat 'monotransitive-slot-3-cat *fcg-constructions*)
(add-link 'you-cxn-cat 'monotransitive-slot-1-cat *fcg-constructions*)
(add-link 'mass-np-cxn-cat 'monotransitive-slot-3-cat *fcg-constructions*)


#|(formulate  '((you y) (tea t) (amr-choice a)
              (like-01 l) (coffee c) (amr-unknown a2)
              (:ARG0 l y) (:ARG1 l a)
              (:OP1 a t) (:OP2 a c)
              (:POLARITY l a2)))|#



(def-fcg-cxn two-cxn
             ((?two-unit
               (span (?start ?end))
               (lex-class cd)
               (meaning-args (?x)))
              <-
              (?two-unit
               (HASH meaning ((:quant ?x 2)))
               (category two-cxn-cat)
               --
               (HASH form ((sequence "two" ?start ?end))))))

(def-fcg-cxn four-cxn
             ((?four-unit
               (span (?start ?end))
               (lex-class cd)
               (meaning-args (?x))
               (category four-cxn-cat))
              <-
              (?four-unit
               (HASH meaning ((:quant ?x 4)))
               --
               (HASH form ((sequence "four" ?start ?end))))))

(def-fcg-cxn them-cxn
             ((?them-unit
               (span (?start ?end))
               (lex-class prp)
               (meaning-args (?t))
               (category them-cxn-cat)
               (agreement (person 3)
                          (number pl)
                          (case accusative)))
              <-
              (?them-unit
               (HASH meaning ((they ?t)))
               
               --
               (HASH form ((sequence "them" ?start ?end))))))

(def-fcg-cxn us-cxn
             ((?us-unit
               (span (?start ?end))
               (lex-class prp)
               (meaning-args (?w))
               (category us-cxn-cat)
               (agreement (person 1)
                          (number pl)
                          (case accusative)))
              <-
              (?us-unit
               (HASH meaning ((we ?w)))
               --
               (HASH form ((sequence "us" ?start ?end))))))
             
(def-fcg-cxn the-CARD-of-PRON-cxn
             ((?group-unit
               (meaning-args (?g))
               (syntactic-form np)
               (category the-CARD-of-PRON-cxn-cat)
               (span (?start-the ?end-pronoun))
               (subunits (?the-unit ?cardinal-unit ?of-unit ?pers-pronoun-unit)))
              <-
              (?the-unit
               --
               (HASH form ((sequence "the" ?start-the ?end-the))))
              (?cardinal-unit
               (meaning-args (?g))
               (agreement (number pl)
                          (person ?person))
               (category the-CARD-of-PRON-cxn-cardinal-slot-cat)
               --
               (span (?start-cardinal ?end-cardinal))
               (category the-CARD-of-PRON-cxn-cardinal-slot-cat)
               (lex-class cd))
              (?of-unit
               --
               (HASH form ((sequence "of" ?start-of ?end-of))))
              (?pers-pronoun-unit
               (meaning-args (?g))
               (category the-CARD-of-PRON-cxn-pronoun-slot-cat)
               --
               (category the-CARD-of-PRON-cxn-pronoun-slot-cat)
               (lex-class prp)
               (agreement (person ?person)
                          (number pl)
                          (case accusative))
               (span (?start-pronoun ?end-pronoun))
               )
              (?group-unit
               (HASH meaning ((group ?g)))
               --
               (HASH form ((precedes ?start-the ?start-cardinal)
                           (precedes ?end-cardinal ?start-of)
                           (precedes ?end-of ?start-pronoun))))))


(add-categories '(two-cxn-cat four-cxn-cat us-cxn-cat
                              them-cxn-cat
                              the-CARD-of-PRON-cxn-cardinal-slot-cat
                              the-CARD-of-PRON-cxn-pronoun-slot-cat
                              the-CARD-of-PRON-cxn-cat)
                *fcg-constructions*)
(add-link 'two-cxn-cat 'the-CARD-of-PRON-cxn-cardinal-slot-cat *fcg-constructions*)
(add-link 'four-cxn-cat 'the-CARD-of-PRON-cxn-cardinal-slot-cat *fcg-constructions*)
(add-link 'us-cxn-cat 'the-CARD-of-PRON-cxn-pronoun-slot-cat *fcg-constructions*)
(add-link 'them-cxn-cat 'the-CARD-of-PRON-cxn-pronoun-slot-cat *fcg-constructions*)
(add-link 'the-CARD-of-PRON-cxn-cat 'monotransitive-slot-1-cat *fcg-constructions*)


;;(comprehend "the two of them would like coffee")