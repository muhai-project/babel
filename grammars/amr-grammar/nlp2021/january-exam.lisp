(in-package :fcg)
;(ql:quickload :amr)
;(ql:quickload :irl)

(defun equivalent-amr-predicate-networks (fcg-amr-network amr-predicates)
  (irl::equivalent-irl-programs? fcg-amr-network
                               (mapcar #'(lambda (predicate)
                                           (cons (first predicate)
                                                 (mapcar #'(lambda (symbol)
                                                             (cond ((stringp symbol)
                                                                    symbol)
                                                                   ((numberp symbol)
                                                                    symbol)
                                                                   ((or (equal symbol '-)
                                                                        (equal symbol '+))
                                                                    symbol)
                                                                   (t
                                                                    (utils::variablify symbol))
                                                                   ))
                                                         (rest predicate))))
                                       amr-predicates)))

;; Sentence 1:
;;-------------------
;" ' My life is very monotonous , ' the fox said . "

#|
(amr:penman->predicates '(s / say-01
                            :ARG0 (f / fox)
                            :ARG1 (m / monotonous
                                     :domain (l / life
                                                :poss (i / i))
                                     :degree (v / very))))
|#

;((FCG::SAY-01 FCG::S) (FCG::FOX FCG::F) (FCG::MONOTONOUS FCG::M) (FCG::LIFE FCG::L) (FCG::I FCG::I) (FCG::VERY FCG::V) (:ARG0 FCG::S FCG::F) (:ARG1 FCG::S FCG::M) (:DOMAIN FCG::M FCG::L) (:DEGREE FCG::M FCG::V) (:POSS FCG::L FCG::I))


(def-fcg-constructions little-prince-exam
  ;; These are the feature-types that we declare. All other features are treated as feature-value pairs.
  :feature-types ((meaning set-of-predicates)
                  (form set-of-predicates)
                  (subunits set)
                  (lex-class set)
                  (footprints set))
  ;; We specify the goal tests here.
  :fcg-configurations ((:production-goal-tests  :no-applicable-cxns)
                       (:parse-goal-tests :no-strings-in-root :connected-structure :no-applicable-cxns)
                       (:max-search-depth . 1000))

  (def-fcg-cxn fox-cxn
               ((?fox-unit
                 (referent ?f)
                 (lex-class (noun))
                 (sem-class physical-entity)
                 )
                <-
                (?fox-unit
                 (HASH meaning ((fox ?f)))
                 --
                 (HASH form ((string ?fox-unit "fox"))))))

  (def-fcg-cxn prince-cxn
               ((?prince-unit
                 (referent ?p)
                 (lex-class (noun))
                 (sem-class physical-entity)
                 )
                <-
                (?prince-unit
                 (HASH meaning ((prince ?p)))
                 --
                 (HASH form ((string ?prince-unit "prince"))))))

  (def-fcg-cxn life-cxn
               ((?life-unit
                 (referent ?l)
                 (lex-class (noun))
                 (sem-class physical-entity))
                <-
                (?life-unit
                 (HASH meaning ((life ?l)))
                 --
                 (HASH form ((string ?life-unit "life"))))))
                 

  (def-fcg-cxn he-cxn
               ((?he-unit
                 (referent ?h)
                 (lex-class (pronoun))
                 (phrase-type noun-phrase)
                 (sem-class physical-entity)
                 (boundaries (leftmost-unit ?he-unit)
                             (rightmost-unit ?he-unit)))
                <-
                (?he-unit
                 
                 --
                 (HASH form ((string ?he-unit "he"))))))

  (def-fcg-cxn i-cxn
               ((?i-unit
                 (referent ?i)
                 (lex-class (pronoun))
                 (phrase-type noun-phrase)
                 (sem-class physical-entity)
                 (boundaries (leftmost-unit ?i-unit)
                             (rightmost-unit ?i-unit)))
                <-
                (?i-unit
                 (HASH meaning ((i ?i)))
                 --
                 (HASH form ((string ?i-unit "I"))))))

  (def-fcg-cxn nothing-cxn
               ((?nothing-unit
                 (referent ?n)
                 (lex-class (pronoun))
                 (phrase-type noun-phrase)
                 (sem-class physical-entity);;nothing?
                 (boundaries (leftmost-unit ?nothing-unit)
                             (rightmost-unit ?nothing-unit))) 
                <-
                (?nothing-unit
                 (HASH meaning ((nothing ?n)))
                 --
                 (HASH form ((string ?nothing-unit "nothing"))))))

  (def-fcg-cxn little-cxn
               ((?little-unit
                 (referent ?l)
                 (lex-class (adjective))
                 (sem-class property))
                <-
                (?little-unit
                 (HASH meaning ((little ?l)))
                 --
                 (HASH form ((string ?little-unit "little"))))))

  (def-fcg-cxn monotonous-cxn
               ((?monotonous-unit
                 (referent ?m)
                 (lex-class (adjective))
                 (sem-class property))
                <-
                (?monotonous-unit
                 (HASH meaning ((monotonous ?m)))
                 --
                 (HASH form ((string ?monotonous-unit "monotonous"))))))

  (def-fcg-cxn busy-cxn
               ((?busy-unit
                 (referent ?b)
                 (lex-class (adjective))
                 (sem-class property))
                <-
                (?busy-unit
                 (HASH meaning ((busy-01 ?b)))
                 --
                 (HASH form ((string ?busy-unit "busy"))))))
  
  (def-fcg-cxn noun-adjective-cxn
               ((?nominal-unit
                 (subunits (?noun-unit ?adjective-unit))
                 (referent ?n)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?adjective-unit)
                             (rightmost-unit ?noun-unit)))
                (?noun-unit
                 (footprints (nominal-cxn)))
                <-
                (?adjective-unit
                 --
                 (referent ?a)
                 (lex-class (adjective)))
                (?noun-unit
                 --
                 (footprints (not nominal-cxn))
                 (referent ?n)
                 (lex-class (noun)))
                (?nominal-unit
                 (HASH meaning ((:mod ?n ?a)))
                 --
                 (HASH form ((meets ?adjective-unit ?noun-unit)))))
               :disable-automatic-footprints t)
  
  (def-fcg-cxn noun-nominal-cxn
               ((?nominal-unit
                 (subunits (?noun-unit))
                 (referent ?ref)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?noun-unit)
                             (rightmost-unit ?noun-unit)))
                (?noun-unit
                 (footprints (nominal-cxn)))
                <-
                (?noun-unit
                 --
                 (footprints (not nominal-cxn))
                 (referent ?ref)
                 (sem-class ?sem-class)
                 (lex-class (noun))))
               :disable-automatic-footprints t)
                 
  (def-fcg-cxn the-nominal-cxn
               ((?noun-phrase-unit
                 (referent ?ref)
                 (phrase-type noun-phrase)
                 (sem-class ?sem-class)
                 (sem-function referring-expression)
                 (subunits (?the-unit ?nominal-unit))
                 (boundaries (leftmost-unit ?the-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                <-
                (?the-unit
                 --
                 (HASH form ((string ?the-unit "the"))))
                (?nominal-unit
                 --
                 (referent ?ref)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?leftmost-nominal-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                (?noun-phrase-unit
                 --
                 (HASH form ((meets ?the-unit ?leftmost-nominal-unit))))))

  (def-fcg-cxn my-nominal-cxn
               ((?noun-phrase-unit
                 (referent ?ref)
                 (phrase-type noun-phrase)
                 (sem-class ?sem-class)
                 (sem-function referring-expression)
                 (subunits (?my-unit ?nominal-unit))
                 (boundaries (leftmost-unit ?my-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                <-
                (?my-unit
                 (HASH meaning ((i ?i)
                                (:poss ?ref ?i)))
                 --
                 (HASH form ((string ?my-unit "my"))))
                (?nominal-unit
                 --
                 (referent ?ref)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?leftmost-nominal-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                (?noun-phrase-unit
                 --
                 (HASH form ((meets ?my-unit ?leftmost-nominal-unit))))))

  (def-fcg-cxn appear-lex-cxn
               ((?appear-unit
                 (referent ?a)
                 (lex-class (ergative-verb verb))
                 (sem-class activity))
                <-
                (?appear-unit
                 (HASH meaning ((appear-01 ?a)))
                 --
                 (lemma appear)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn appear-appeared-morph
           (<-
            (?appeared-unit
             (lemma appear)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?appeared-unit "appeared"))))))

  (def-fcg-cxn turn-lex-cxn
               ((?turn-unit
                 (referent ?t)
                 (lex-class (ergative-verb verb))
                 (sem-class activity)
                 (sem-frame motion))
                <-
                (?turn-unit
                 (HASH meaning ((turn-01 ?t)))
                 --
                 (lemma turn)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?tense))))

  (def-fcg-cxn turn-turned-morph
           (<-
            (?turned-unit
             (lemma turn)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?turned-unit "turned"))))))

    (def-fcg-cxn say-lex-cxn
               ((?say-unit
                 (referent ?s)
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame statement))
                <-
                (?say-unit
                 (HASH meaning ((say-01 ?s)))
                 --
                 (lemma say)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn say-said-morph
           (<-
            (?said-unit
             (lemma say)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?said-unit "said"))))))

  (def-fcg-cxn respond-lex-cxn
               ((?respond-unit
                 (referent ?r)
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame response))
                <-
                (?respond-unit
                 (HASH meaning ((respond-01 ?r)))
                 --
                 (lemma respond)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn respond-responded-morph
           (<-
            (?responded-unit
             (lemma respond)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?responded-unit "responded"))))))

  (def-fcg-cxn see-lex-cxn
               ((?see-unit
                 (referent ?s)
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame perception))
                <-
                (?see-unit
                 (HASH meaning ((see-01 ?s)))
                 --
                 (lemma see)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn see-saw-morph
           (<-
            (?saw-unit
             (lemma see)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?saw-unit "saw"))))))
  

  (def-fcg-cxn intransitive-ergative-cxn
               ((?ergative-clause-unit
                 (subunits (?arg1-unit ?ergative-verb-unit))
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?ergative-verb-unit))
                 (syn-valence (subject ?arg1-unit))
                 (phrase-type clause)
                 (referent ?event))
                (?ergative-verb-unit
                 (footprints (argument-structure-cxn)))
                <-
                (?arg1-unit
                 --
                 (referent ?arg1)
                 (sem-function referring-expression)
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?rightmost-arg1-unit)))
                (?ergative-verb-unit
                 (HASH meaning ((:arg1 ?event ?arg1)))
                 --
                 (footprints (not argument-structure-cxn))
                 (referent ?event)
                 (lex-class (ergative-verb))))
               :disable-automatic-footprints t)

  (def-fcg-cxn it-was-then-that-X-cxn
               (<-
                (?then-unit
                 (referent ?t)
                 (lex-class holophrase)
                 --
                 (HASH form ((string ?it "it")
                             (string ?was "was")
                             (string ?then-unit "then")
                             (string ?that "that")
                             (meets ?it ?was)
                             (meets ?was ?then-unit)
                             (meets ?then-unit ?that))))
                (?clause-unit
                 (HASH meaning ((:time ?event ?t)
                                (then ?t)))
                 --
                 (referent ?event)
                 (phrase-type clause)
                 (boundaries (leftmost-unit ?leftmost-event-unit)
                             (rightmost-unit ?rightmost-event-unit))
                 (HASH form ((meets ?that ?leftmost-event-unit))))))

  (def-fcg-cxn good-morning-cxn
               ((?good-unit
                 (referent ?g))
                (?morning-unit
                 (referent ?m))
                (?expression-unit
                 (referent ?m)
                 (sem-class greeting)
                 (lex-class fixed-expression)
                 (phrase-type clause)
                 (subunits (?good-unit ?morning-unit))
                 (boundaries (leftmost-unit ?good-unit)
                             (rightmost-unit ?morning-unit)))
                <-
                (?good-unit
                 (HASH meaning ((good-02 ?g)))
                 --
                 (HASH form ((string ?good-unit "Good"))))
                (?morning-unit
                 (HASH meaning ((morning ?m)))
                 --
                 (HASH form ((string ?morning-unit "morning"))))
               
                (?expression-unit
                 (HASH meaning ((:arg1-of ?m ?g)))
                 --
                 (HASH form ((meets ?good-unit ?morning-unit))))))

  (def-fcg-cxn X-is-Y-cxn ;;X-is-Y-cxn
               ((?statement-unit
                 (referent ?y)
                 (phrase-type clause)
                 (subunits (?x-unit ?is-unit ?y-unit))
                 (boundaries (leftmost-unit ?x-leftmost-unit)
                             (rightmost-unit ?y-rightmost-unit)))
                <-
                (?x-unit
                 (referent ?x)
                 (sem-function referring-expression)
                 --
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?x-leftmost-unit)
                             (rightmost-unit ?x-rightmost-unit)))
                (?is-unit
                 (HASH meaning ((:domain ?y ?x)))
                 --
                 (HASH form ((string ?is-unit "is")
                             (meets ?x-rightmost-unit ?is-unit)
                             (meets ?is-unit ?y-leftmost-unit))))
                (?y-unit
                 (referent ?y)
                 --
                 (lex-class (adjective))
                 (boundaries (leftmost-unit ?y-leftmost-unit)
                             (rightmost-unit ?y-rightmost-unit)))))

  (def-fcg-cxn X-was-Y-cxn 
               ((?statement-unit
                 (referent ?x)
                 (phrase-type clause)
                 (subunits (?x-unit ?is-unit ?y-unit))
                 (boundaries (leftmost-unit ?x-leftmost-unit)
                             (rightmost-unit ?y-rightmost-unit)))
                <-
                (?x-unit
                 (referent ?x)
                 (sem-function referring-expression)
                 --
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?x-leftmost-unit)
                             (rightmost-unit ?x-rightmost-unit)))
                (?is-unit
                 (HASH meaning ((:arg1-of ?x ?y)))
                 --
                 (HASH form ((string ?is-unit "was")
                             (meets ?x-rightmost-unit ?is-unit)
                             (meets ?is-unit ?y-leftmost-unit))))
                (?y-unit
                 (referent ?y)
                 --
                 (lex-class (adjective))
                 (boundaries (leftmost-unit ?y-leftmost-unit)
                             (rightmost-unit ?y-rightmost-unit)))))

  (def-fcg-cxn very-adverb-cxn
               ((?very-x-unit
                 (referent ?ref)
                 (sem-class ?sc)
                 (lex-class ?lc)
                 (boundaries (leftmost-unit ?very-unit)
                             (rightmost-unit ?modified-unit))
                 (subunits (?very-unit ?modified-unit)))
                <-
                (?very-unit
                 (HASH meaning ((:degree ?ref ?v)
                                (very ?v)))
                 --
                 (HASH form ((string ?very-unit "very"))))
                (?modified-unit
                 (referent ?ref)
                 --
                 (sem-class ?sc)
                 (lex-class ?lc)
                 (HASH form ((meets ?very-unit ?modified-unit))))))

  (def-fcg-cxn direct-speech-cxn
               ((?direct-speech-unit
                 (referent ?s)
                 (phrase-type direct-speech)
                 (subunits (?statement-unit))
                 (boundaries (leftmost-unit ?quote-1)
                             (rightmost-unit ?quote-2)))
                <-
                (?statement-unit
                 --
                 (referent ?s)
                 (phrase-type clause)
                 (boundaries (leftmost-unit ?leftmost-statement-unit)
                             (rightmost-unit ?rightmost-statement-unit)))
                (?direct-speech-unit
                 --
                 (HASH form ((string ?quote-1 "'")
                             (string ?comma ",") ;;with comma!
                             (string ?quote-2 "'")
                             (meets ?comma ?quote-2)
                             (meets ?quote-1 ?leftmost-statement-unit)
                             (meets ?rightmost-statement-unit ?comma))))))
  
  (def-fcg-cxn topicalised-statement-frame-cxn
               ((?clause-unit
                 (subunits (?statement-unit ?statement-verb-unit ?arg0-unit))
                 (syn-valence (subject ?arg0-unit))
                 (boundaries (leftmost-unit ?quote-1)
                             (rightmost-unit ?righmost-arg0-unit))
                 (phrase-type clause)
                 (sem-function proposition)
                 (referent ?s2))
                (?statement-verb-unit
                 (footprints ( argument-structure-cxn)))
                 
                <-
                (?statement-unit
                 --
                 (referent ?s)
                 (phrase-type direct-speech))
                (?statement-verb-unit
                 (HASH meaning ((:arg0 ?s2 ?arg0)
                                (:arg1 ?s2 ?s)))
                 --
                 (sem-frame statement)
                 (lex-class (verb))
                 (referent ?s2)
                 (footprints (not argument-structure-cxn)))
                (?arg0-unit
                 --
                 (referent ?arg0)
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?leftmost-arg0-unit)
                             (rightmost-unit ?righmost-arg0-unit))))
               :disable-automatic-footprints t)

  (def-fcg-cxn topicalised-response-frame-cxn
               ((?clause-unit
                 (subunits (?statement-unit ?statement-verb-unit ?arg0-unit))
                 (syn-valence (subject ?arg0-unit))
                 (boundaries (leftmost-unit ?leftmost-statement-unit)
                             (rightmost-unit ?righmost-statement-verb-unit))
                 (phrase-type clause)
                 (sem-function proposition)
                 (referent ?s2))
                (?statement-verb-unit
                 (footprints ( argument-structure-cxn)))
                <-
                (?statement-unit
                 --
                 (referent ?s)
                 (phrase-type direct-speech)
                 (boundaries (leftmost-unit ?leftmost-statement-unit)
                             (rightmost-unit ?rightmost-statement-unit)))
                (?arg0-unit
                 --
                 (referent ?arg0)
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?leftmost-arg0-unit)
                             (rightmost-unit ?righmost-arg0-unit)))
                (?statement-verb-unit
                 (HASH meaning ((:arg0 ?s2 ?arg0)
                                (:arg2 ?s2 ?s)))
                 --
                 (sem-frame response)
                 (lex-class (verb))
                 (referent ?s2)
                 (footprints (not argument-structure-cxn))
                 (boundaries (leftmost-unit ?leftmost-statement-verb-unit)
                             (rightmost-unit ?rightmost-statement-verb-unit)))
                
                (?clause-unit
                 --
                 (HASH form ((meets ?rightmost-statement-unit ?leftmost-arg0-unit)
                             (meets ?righmost-arg0-unit ?leftmost-statement-verb-unit)))))
               :disable-automatic-footprints t)

  (def-fcg-cxn politely-adverb
               ((?politely-unit
                 (referent ?p))
                (?event-unit
                 (subunits (?politely-unit))
                 (boundaries (leftmost-unit ?event-unit)
                             (rightmost-unit ?politely-unit)))
                <-
                (?politely-unit
                 (HASH meaning ((polite-01 ?p)))
                 --
                 (HASH form ((string ?politely-unit "politely"))))
                (?event-unit
                 (referent ?e)
                 (lex-class (verb))
                 (HASH meaning ((:manner ?e ?p)))
                 --
                 (HASH form ((meets ?event-unit ?politely-unit))))))


  (def-fcg-cxn active-transitive-cxn
               ((?transitive-clause-unit
                 (syn-valence (subject ?arg0-unit)
                              (direct-object ?arg1-unit))
                 (referent ?event)
                 (phrase-type clause)
                 (sem-function proposition)
                 (boundaries (leftmost-unit ?leftmost-arg0-unit)
                             (rightmost-unit ?rightmost-arg1-unit))
                 (subunits (?arg0-unit ?event-unit ?arg1-unit)))
                (?event-unit
                 (footprints (argument-structure-cxn)))
                <-
                (?arg0-unit
                 --
                 (referent ?arg0)
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?leftmost-arg0-unit)
                             (rightmost-unit ?rightmost-arg0-unit)))
                (?event-unit
                 --
                 (referent ?event)
                 (lex-class (verb))
                 (footprints (not argument-structure-cxn)))
                (?arg1-unit
                 --
                 (referent ?arg1)
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?rightmost-arg1-unit)))
                (?transitive-clause-unit
                 (HASH meaning ((:arg0 ?event ?arg0)
                                (:arg1 ?event ?arg1)))
                 --
                 (HASH form ((meets ?rightmost-arg0-unit ?event-unit)
                             (meets ?event-unit ?leftmost-arg1-unit)))))
               :disable-automatic-footprints t)

  (def-fcg-cxn around-cxn
               ((?around-unit
                 (referent ?a)
                 (lex-class (adverb))
                 (sem-class direction)
                 (boundaries (leftmost-unit ?around-unit)
                             (rightmost-unit ?around-unit)))
                <-
                (?around-unit
                 (HASH meaning ((around ?a)))
                 --
                 (HASH form ((string ?around-unit "around"))))))
  
  (def-fcg-cxn ergative-motion-direction-cxn
               ((?ergative-clause-unit
                 (referent ?motion)
                 (phrase-type clause)
                 (sem-function proposition)
                 (syn-valence (subject ?arg1-unit))
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?rightmost-direction-unit))
                 (subunits (?arg1-unit ?motion-unit ?direction-unit)))
                (?motion-unit
                 (footprints (argument-structure-cxn)))
                 <-
                (?arg1-unit
                 --
                 (referent ?arg1)
                 (phrase-type noun-phrase)
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?rightmost-arg1-unit)))
                (?motion-unit
                 --
                 (referent ?motion)
                 (sem-frame motion)
                 (lex-class (ergative-verb))
                 (footprints (not argument-structure-cxn)))
                (?direction-unit
                 --
                 (referent ?direction)
                 (sem-class direction)
                 (boundaries (leftmost-unit ?leftmost-direction-unit)
                             (rightmost-unit ?rightmost-direction-unit)))
                (?ergative-clause-unit
                 (HASH meaning ((:arg1 ?motion ?arg1)
                                (:direction ?motion ?direction)))
                 --
                 (HASH form ((meets ?rightmost-arg1-unit ?motion-unit)
                             (meets ?motion-unit ?leftmost-direction-unit)))))
               :disable-automatic-footprints t)

  (def-fcg-cxn when-cxn
               ((?when-unit
                 (lex-class (adverb temporal-adverb)))
                <-
                (?when-unit
                 --
                 (HASH form ((string ?when-unit "when"))))))
  
  (def-fcg-cxn temporal-chained-propositions-cxn
               ((?temporal-subclause-unit
                 (phrase-type clause)
                 (referent ?proposition-2)
                 (syn-valence (subject ?second-subject-unit))
                 (subunits (?temporal-adverb-unit ?first-proposition-unit ?second-proposition-unit))
                 (boundaries (leftmost-unit ?temporal-adverb-unit)
                             (rightmost-unit ?rightmost-proposition-2-unit)))
                <-
                (?temporal-adverb-unit
                 --
                 (lex-class (temporal-adverb)))
                (?first-proposition-unit
                 --
                 (referent ?proposition-1)
                 (sem-function proposition)
                 (syn-valence (subject ?first-subject-unit))
                 (boundaries (leftmost-unit ?leftmost-proposition-1-unit)
                             (rightmost-unit ?rightmost-proposition-1-unit)))
                (?first-subject-unit
                 --
                 (referent ?same-ref))
                (?second-proposition-unit
                 --
                 (referent ?proposition-2)
                 (sem-function proposition)
                 (syn-valence (subject ?second-subject-unit))
                 (boundaries (leftmost-unit ?leftmost-proposition-2-unit)
                             (rightmost-unit ?rightmost-proposition-2-unit)))
                (?second-subject-unit
                 --
                 (referent ?same-ref))
                (?temporal-subclause-unit
                 (HASH meaning ((:time ?proposition-2 ?proposition-1)))
                 --
                 (HASH form ((meets ?temporal-adverb-unit ?leftmost-proposition-1-unit)
                             (meets ?rightmost-proposition-1-unit ?leftmost-proposition-2-unit))))))

  (def-fcg-cxn although-cxn
               ((?although-unit
                 (lex-class (conjunction concessive-conjunction)))
                <-
                (?although-unit
                 --
                 (HASH form ((string ?although-unit "although"))))))
               
  (def-fcg-cxn concessive-subclause-cxn
               ((?concessive-clause-unit
                 (subunits (?concessive-conjunction-unit ?clause-unit))
                 (phrase-type subclause)
                 (syn-valence (subject ?subject-unit))
                 (referent ?concession)
                 (args (?proposition ?concession))
                 (boundaries (leftmost-unit ?concessive-conjunction-unit)
                             (rightmost-unit ?rightmost-clause-unit)))
                <-
                (?concessive-conjunction-unit
                 --
                 (lex-class (concessive-conjunction)))
                (?clause-unit
                 --
                 (phrase-type clause)
                 (referent ?concession)
                 (syn-valence (subject ?subject-unit))
                 (boundaries (leftmost-unit ?leftmost-clause-unit)
                             (rightmost-unit ?rightmost-clause-unit)))
                (?concessive-clause-unit
                 (HASH meaning ((:concession ?proposition ?concession)))
                 --
                 (HASH form ((meets ?concessive-conjunction-unit ?leftmost-clause-unit))))))

  (def-fcg-cxn main+subclause-cxn
               ((?sentence-unit
                 (referent ?proposition)
                 (syn-valence (subject ?subject-unit))
                 (subunits (?main-clause-unit ?comma-unit ?sub-clause-unit))
                 (boundaries (leftmost-unit ?leftmost-main-clause-unit)
                             (rightmost-unit ?rightmost-sub-clause-unit)))
                <-
                (?main-clause-unit
                 --
                 (referent ?proposition)
                 (phrase-type clause)
                 (sem-function proposition)
                 (syn-valence (subject ?subject-main-clause-unit))
                 (boundaries (leftmost-unit ?leftmost-main-clause-unit)
                             (rightmost-unit ?rightmost-main-clause-unit)))
                (?subject-main-clause-unit
                 --
                 (referent ?same-ref))
                (?comma-unit
                 --
                 (HASH form ((string ?comma-unit ",")
                             (meets ?comma-unit ?leftmost-sub-clause-unit)
                             (meets ?rightmost-main-clause-unit ?comma-unit))))
                (?sub-clause-unit
                 --
                 (phrase-type subclause)
                 (referent ?subclause-ref)
                 (syn-valence (subject ?subject-sub-clause-unit))
                 (args (?proposition ?subclause-ref))
                 (boundaries (leftmost-unit ?leftmost-sub-clause-unit)
                             (rightmost-unit ?rightmost-sub-clause-unit)))
                (?subject-sub-clause-unit
                 --
                 (referent ?same-ref))))

  (def-fcg-cxn at-that-moment-cxn
               ((?at-that-moment-unit
                 (referent ?m)
                 (sem-function temporal-expression)
                 (phrase-type adverbial-phrase)
                 (subunits (?at-unit ?that-unit ?moment-unit))
                 (boundaries (leftmost-unit ?at-unit)
                             (rightmost-unit ?moment-unit)))
                <-
                (?at-unit
                 --
                 (HASH form ((string ?at-unit "at"))))
                (?that-unit
                 (HASH meaning ((that ?t)))
                 --
                 (HASH form ((string ?that-unit "that"))))
                (?moment-unit
                 (HASH meaning ((moment ?m)
                                (:mod ?m ?t)))
                 --
                 (HASH form ((string ?moment-unit "moment")
                             (meets ?at-unit ?that-unit)
                             (meets ?that-unit ?moment-unit))))))

  
                
(def-fcg-cxn TO-UNSCREW-A-BOLT-THAT-HAD-GOT-STUCK-IN-MY-ENGINE-CXN
             ((?PHRASE-UNIT
               (SUBUNITS (?TO-UNIT ?UNSCREW-UNIT ?A-UNIT ?BOLT-UNIT ?THAT-UNIT ?HAD-UNIT ?GOT-UNIT ?STUCK-UNIT ?IN-UNIT ?MY-UNIT ?ENGINE-UNIT))
               (phrase-type infinitival-clause)
               (BOUNDARIES (LEFTMOST-UNIT ?TO-UNIT)
                           (RIGHTMOST-UNIT ?ENGINE-UNIT))
               (referent ?u)
               (sem-valence (:arg0 ?i)
                            (:arg1 ?b)))
              <-
              
              (?TO-UNIT
               --
               (HASH FORM ((STRING ?TO-UNIT "to"))))
              (?UNSCREW-UNIT
               --
               (HASH FORM ((STRING ?UNSCREW-UNIT "unscrew"))))
              (?A-UNIT
               --
               (HASH FORM ((STRING ?A-UNIT "a"))))
              (?BOLT-UNIT
               --
               (HASH FORM ((STRING ?BOLT-UNIT "bolt"))))
              (?THAT-UNIT
               --
               (HASH FORM ((STRING ?THAT-UNIT "that"))))
              (?HAD-UNIT
               --
               (HASH FORM ((STRING ?HAD-UNIT "had"))))
              (?GOT-UNIT
               --
               (HASH FORM ((STRING ?GOT-UNIT "got"))))
              (?STUCK-UNIT
               --
               (HASH FORM ((STRING ?STUCK-UNIT "stuck"))))
              (?IN-UNIT
               --
               (HASH FORM ((STRING ?IN-UNIT "in"))))
              (?MY-UNIT
               --
               (HASH FORM ((STRING ?MY-UNIT "my"))))
              (?ENGINE-UNIT
               --
               (HASH FORM ((STRING ?ENGINE-UNIT "engine"))))
              (?PHRASE-UNIT
               (HASH meaning ((unscrew-01 ?u) (bolt ?b) (stick-01 ?s) (engine ?e) (:arg0 ?u ?i) (:arg1 ?u ?b) (:arg1-of ?b ?s) (:arg2 ?s ?e) (:poss ?e ?i)))
               --
               (HASH FORM ((MEETS ?TO-UNIT ?UNSCREW-UNIT)
                           (MEETS ?UNSCREW-UNIT ?A-UNIT)
                           (MEETS ?A-UNIT ?BOLT-UNIT)
                           (MEETS ?BOLT-UNIT ?THAT-UNIT)
                           (MEETS ?THAT-UNIT ?HAD-UNIT)
                           (MEETS ?HAD-UNIT ?GOT-UNIT)
                           (MEETS ?GOT-UNIT ?STUCK-UNIT)
                           (MEETS ?STUCK-UNIT ?IN-UNIT)
                           (MEETS ?IN-UNIT ?MY-UNIT)
                           (MEETS ?MY-UNIT ?ENGINE-UNIT))))))


(def-fcg-cxn transitive-try-cxn
             ((?clause-unit
               (clause-type transitive)
               (sem-function predication)
               (referent ?t)
               (subunits (?arg0-unit ?trying-unit ?arg1-unit)))
              <-
              (?arg0-unit
               (referent ?arg0)
               --
               (referent ?arg0)
               (phrase-type clause)
               (boundaries (leftmost-unit ?arg0-leftmost-unit)
                           (rightmost-unit ?arg0-rightmost-unit)))
              (?trying-unit
               (HASH meaning ((try-01 ?t)
                              (:arg0 ?t ?arg0)
                              (:arg1 ?t ?arg1)))
               --
               (HASH form ((string ?trying-unit "trying")
                           (meets ?arg0-rightmost-unit ?trying-unit)
                           (meets ?trying-unit ?arg1-leftmost-unit))))
              (?arg1-unit
               (referent ?arg1)
               --
               (referent ?arg1)
               (sem-valence (:arg0 ?arg0))
               (phrase-type infinitival-clause)
               (boundaries (leftmost-unit ?arg1-leftmost-unit)
                           (rightmost-unit ?arg1-rightmost-unit)))))

(def-fcg-cxn temporal-modification-of-event-cxn
             ((?event-unit
               (subunits (?time-unit)))
              <-
              (?event-unit
               (referent ?event)
               (sem-function predication)
               (HASH meaning ((:time ?event ?time)))
               --
               (sem-function predication)
               (clause-type ?clause))
              (?time-unit
               (referent ?time)
               (sem-function temporal-expression)
               --
               (phrase-type adverbial-phrase))))
)
                     
         
        
  

;(activate-monitor trace-fcg)


(equivalent-amr-predicate-networks
 (comprehend "' my life is very monotonous , ' the fox said .")
 (penman->predicates '(s / say-01
                              :ARG0 (f / fox)
                              :ARG1 (m / monotonous
                                       :domain (l / life
                                                  :poss (i / i))
                                       :degree (v / very)))))

(equivalent-amr-predicate-networks
 (comprehend "at that moment I was very busy trying to unscrew a bolt that had got stuck in my engine")
 (penman->predicates '(t / try-01
                         :ARG0 (i / i
                                  :ARG1-of (b2 / busy-01
                                               :degree (v / very)))
                         :ARG1 (u / unscrew-01
                                  :ARG0 i
                                  :ARG1 (b / bolt
                                           :ARG1-of (s / stick-01
                                                       :ARG2 (e / engine
                                                                :poss i))))
                         :time (m / moment
                                  :mod (t2 / that)))))

;((FCG::TRY-01 T) (FCG::I FCG::I) (FCG::BUSY-01 FCG::B2) (FCG::VERY FCG::V) (FCG::UNSCREW-01 FCG::U) (FCG::BOLT UTILS:B) (FCG::STICK-01 FCG::S) (FCG::ENGINE FCG::E) (FCG::MOMENT FCG::M) (FCG::THAT FCG::T2)
(:ARG0 T FCG::I) (:ARG1 T FCG::U) (:TIME T FCG::M)
(:ARG1-OF FCG::I FCG::B2) (:DEGREE FCG::B2 FCG::V) (:ARG0 FCG::U FCG::I) (:ARG1 FCG::U UTILS:B) (:ARG1-OF UTILS:B FCG::S) (:ARG2 FCG::S FCG::E) (:POSS FCG::E FCG::I) (:MOD FCG::M FCG::T2))