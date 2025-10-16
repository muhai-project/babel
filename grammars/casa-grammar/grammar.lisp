(in-package :fcg)
;(ql:quickload :casa)

;; Would you like tea or coffee?
;; Shall I ring the police?

;; Constructions: YES-NO-QUESTION, MODAL-CXN, MONOTRANSITIVE-CXN, OR-COORDINATION, NP CXNS (3)

;;(defparameter nlp-tools::*penelope-host* "http://127.0.0.1:5000")
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

;;(comprehend-and-formulate "would you like tea or coffee?")
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
               (sem-roles (liker ?liker)
                          (object-of-affection ?object-of-affection)))
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



#|
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
               (meaning-args (?event))
               (syntactic-form verb)
               --
               (string "has")
               (dependency-head ?done-unit))
              (?done-unit
               (syntactic-form verb)
               --
               (lemma do)
               (string "done")
               (parent ?vp-unit))
              (?vp-unit
               --
               (word-order ((adjacent ?done-unit ?nominalisation-unit)))
               (constituents (?done-unit ?nominalisation-unit)))
              (?nominalisation-unit
               --
               (syntactic-form np)
               (meaning-args (?event)))))

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

(def-fcg-cxn intransitive-cxn
             ((?slot-2-predicate
               (footprints (arg-structure-cxn)))
              <-
              (?slot-1-argument
               (syntactic-function (potential subject))
               --
               (meaning-args (?agent))
               (lex-class pers-pronoun)
               (syntactic-form np))
              (?slot-2-predicate
               (sem-roles (arg0 ?agent))
               (meaning ((:arg0 ?event ?agent)))
               --
               (footprints (not arg-structure-cxn))
               (parent ?vp-parent)
               (meaning-args (?event))
               (syntactic-form verb))
              (?vp-parent
               (meaning-args (?event))
               (syntactic-function (pred-rest))
               --
               (parent ?sentence))
              (?sentence
               (meaning-args (?event))
               --
               (constituents (?slot-1-argument ?vp-parent))))
             :disable-automatic-footprints t)


(def-fcg-cxn it-cleft-where-cxn
             (<-
              (?first-slot
               (syntactic-function (subject))
               --
               (string "it"))
              (?second-slot
               (syntactic-function (V))
               (meaning ((be-located-at-91 ?b)
                         (:arg2-of ?b ?h)
                         (:arg1 ?b ?r)))
               --
               (lemma be))
              (?third-slot
               (syntactic-function (obj attr))
               --
               (meaning-args (?h))
               (syntactic-form adverb)) ;;here
              (?fourth-slot
               (syntactic-function (dependent-clause))
               --
               (constituents (?where ?dependent-clause)))
              (?dependent-clause
               (syntactic-function (dependent-clause))
               --
               (meaning-args (?r)))
              (?where
               --
               (string "where")
               (parent ?fourth-slot)))
             :description "Highlight focused / new information to hearer.")

|#
;; It's here where she has done groundbreaking neuroimaging research (COCA-2014-SPOK)

#|'(h / here
    :arg2-of (b / be-located-at-91
                :arg1 (r / research.01
                         :arg1 (n / neuroimaging)
                         :mod (g / groundbreaking)
                         :arg0 (s / she))))|#

;;((FCG::HERE FCG::H) (FCG::BE-LOCATED-AT-91 UTILS:B) (FCG::RESEARCH.01 FCG::R) (FCG::NEUROIMAGING FCG::N) (FCG::GROUNDBREAKING FCG::G) (FCG::SHE FCG::S) (:ARG2-OF FCG::H UTILS:B) (:ARG1 UTILS:B FCG::R) (:ARG1 FCG::R FCG::N) (:MOD FCG::R FCG::G) (:ARG0 FCG::R FCG::S))


;;(comprehend-all "it's here where she has done groundbreaking neuroimaging research")





;; (ql:quickload :ofef-parser)
;; (ofef-parser:fcg->ofef *fcg-constructions*)






#|
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



(def-fcg-cxn the-police-cxn
             (<-
              (?the-police-unit
               (meaning ((police ?p)))
               ;;(syntactic-form np)
               (phrase-types (np))
               (meaning-args (?p))
               --
               (string "the police")))) |#