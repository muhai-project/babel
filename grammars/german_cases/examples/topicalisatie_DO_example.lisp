(ql:quickload :fcg)

(in-package :fcg)

(activate-monitor trace-fcg)

(configure-grammar *fcg-constructions*)

(def-fcg-constructions german-case-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set)
                  (case sequence))
   :visualization-configurations ((:with-search-debug-data . t)
                                 (:remove-empty-units . nil)
                                 (:show-constructional-dependencies . t)
                                 (:labeled-paths . nil)
                                 (:colored-paths . nil)
                                 (:hierarchy-features subunits)
                                 (:selected-hierarchy . subunits)
                                 ;(:hide-features footprints sem-cat form boundaries)  ;;choose elements to hide
                                 (:select-subfeatures . nil)
                                 (:latex-visualization . t)
                                 (:add-form-and-meaning-to-car . t)
                                 (:show-upper-menu . nil)
                                 (:subfeatures . nil)
                                 (:expand-nodes-in-search-tree . t)
                                 (:coupled-mode . nil))
  :fcg-configurations ((:max-nr-of-nodes . 40000)
          
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network :connected-structure)
                       ;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :cxn-sets) ;; returns all cxns at once
                       (:node-tests :mal-cxn-applied :restrict-search-depth :restrict-nr-of-nodes :check-duplicate)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-sets) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       ;; cxn sets
                       (:parse-order cxn  mal-cxn)
                       (:production-order cxn mal-cxn)
                       ;; goal tests
                       (:production-goal-tests
                        :no-applicable-cxns :connected-structure
                        :no-meaning-in-root)))


(defmethod cip-node-test ((node cip-node) (mode (eql :mal-cxn-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'mal-cxn)
    (and (push 'mal-cxn-applied (statuses node))
         t)
      t
      ))

(pushnew '((mal-cxn-applied) .  "#eb4034;") *status-colors*)

;;;;DETERMINERS


;;;;;no meaning - comprehension no need existing unit 

(def-fcg-cxn der-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nm ?nm - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (?gen - ?gf - ?gp)    ;genitive feminine
                               (?df - ?df - -)      ;sing, masc, fem, neut, plural
                               (?s ?nm ?f - ?gp))))   ;sing, masc, fem, neut, plural

               --
               (HASH form ((string ?the-word "der")))))
             :disable-automatic-footprints t)

(def-fcg-cxn dem-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)      ;sing, masc, fem, neut, plural
                               (+ ?dm - ?dn -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "dem")))))
             :disable-automatic-footprints t)


(def-fcg-cxn die-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nom - ?nf - ?np)    ;nom, acc, gen, dat  (nom masculine)
                               (?acc - ?af - ?ap)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (- - - - -)
                               (?s - ?f - ?p))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "die")))))
             :disable-automatic-footprints t)


(def-fcg-cxn den-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)        
                               (?am ?am - - -)        
                               (- - - - -)          
                               (?dp - - - ?dp)
                               (?am ?am - - ?dp))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "den")))))
             :disable-automatic-footprints t)


(def-fcg-cxn das-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nn - - ?nn -)    ;nom, acc, gen, dat  (nom masculine)
                               (?an - - ?an -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (- - - - -)
                               (+ - - + -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "das")))))
             :disable-automatic-footprints t)


(def-fcg-cxn Direktor-cxn
             ((?director-word                      
               (referent ?d)
               (syn-cat (lex-class noun)
                        (case ((?nom ?nm - - ?np)    
                               (?acc ?am - - -)     
                               (?pg - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?director-word                           
               (HASH meaning ((director ?d)))              
               --
               (HASH form ((string ?director-word "Direktor"))))))

(def-fcg-cxn Doktor-cxn
             ((?doctor-word
               (referent ?d)                  
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))      
              <-
              (?doctor-word
               (HASH meaning ((doctor ?d)))                     
               --
               (HASH form ((string ?doctor-word  "Doktor"))))))


(def-fcg-cxn Buch-cxn
             ((?book-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
                        (sem-cat (animacy inanimate)))
              <-
              (?book-word                            
               (HASH meaning ((book ?b)))                    
               --
               (HASH form ((string ?book-word  "Buch"))))))

(def-fcg-cxn Apfel-cxn
             ((?apple-word                        
               (referent ?a)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy inanimate)))
              <-
              (?apple-word                            
               (HASH meaning ((apple ?a)))                    
               --
               (HASH form ((string ?apple-word  "Apfel"))))))

(def-fcg-cxn Clown-cxn
             ((?clown-word                        
               (referent ?c)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?clown-word                            
               (HASH meaning ((clown ?c)))                    
               --
               (HASH form ((string ?clown-word  "Clown")))))) 

(def-fcg-cxn Mann-cxn
             ((?man-word
               (referent ?m)                  
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
                       
              <-
              (?man-word
               (HASH meaning ((man ?m)))                     
               --
               (HASH form ((string ?man-word  "Mann"))))))


(def-fcg-cxn Lehrerin-cxn
             ((?teacher-word                        
               (referent ?t)
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate)))
              <-
              (?teacher-word
               (HASH meaning ((teacher ?t)))                     
               --
               (HASH form ((string ?teacher-word  "Lehrerin"))))))


(def-fcg-cxn Frau-cxn
             ((?woman-word                        
               (referent ?w)
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate)))
              <-
              (?woman-word
               (HASH meaning ((woman ?w)))                     
               --
               (HASH form ((string ?woman-word  "Frau"))))))


(def-fcg-cxn Blumen-cxn
             ((?flowers-word
               (referent ?fl)                             ;set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
                        (case ((?np - - - ?np)     
                               (?ap - - - ?ap)      
                               (?gp - - - ?gp)       
                               (?dp - - - ?dp)
                               (- - - - +))))
              (sem-cat (animacy inanimate)))
              <-
              (?flowers-word
               (HASH meaning ((flowers ?fl)))                     
               --
               (HASH form ((string ?flowers-word  "Blumen"))))))

(def-fcg-cxn Blume-cxn
             ((?flower-word
               (referent ?fl)                             ;set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
              (sem-cat (animacy inanimate)))
              <-
              (?flower-word
               (HASH meaning ((flower ?fl)))                     
               --
               (HASH form ((string ?flower-word  "Blume"))))))


(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               (subunits (?article ?noun))
               (boundaries (leftmost-unit ?article)
                           (rightmost-unit ?noun)))
              (?article
               (referent ?x)
               (part-of-noun-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?article
               --
               (syn-cat (lex-class article)
                        (case ?case)))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))
              ))
             :disable-automatic-footprints t)

(comprehend "der Mann")

(def-fcg-cxn verkauft-cxn
             ((?sell-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?v))  
                        
              <-
              (?sell-word                           
               (HASH meaning ((verkaufen-01 ?v)))
               --
               (HASH form ((string ?sell-word  "verkauft"))))))


(def-fcg-cxn schenkt-cxn
             ((?gift-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?g))  
                        
              <-
              (?gift-word                           
               (HASH meaning ((schenken-01 ?g)))
               --
               (HASH form ((string ?gift-word  "schenkt"))))))


(def-fcg-cxn gibt-cxn
             ((?give-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?g))  
                        
              <-
              (?give-word                           
               (HASH meaning ((geben-01 ?g)))
               --
               (HASH form ((string ?give-word  "gibt"))))))


(def-fcg-cxn ditransitive-argument-structure-cxn
             ((?ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (sem-cat (animacy animate))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (sem-cat (animacy animate))
              (referent ?arg2))
              
              (?ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --
               )))

(def-fcg-cxn ditransitive-incorrect-double-dative-argument-structure-cxn
             ((?ditransitive-incorrect-double-dative-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (?dm ?dm - - -)
                               (?agm ?m - - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (?dm ?dm - - -)
                               (?agm ?m - - -))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (referent ?arg2))
              
              (?ditransitive-incorrect-double-dative-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --
               ))
             :cxn-set mal-rule)

(comprehend "dem Clown verkauft dem Doktor das Buch")

(def-fcg-cxn topic-arg0-arg1-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-receiver-unit)
                           (meets ?rightmost-receiver-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (referent ?arg0)
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              ))


(def-fcg-cxn arg0-arg1-topic-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg2 +)))  
                          
               --
               (HASH form ((meets ?rightmost-receiver-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-agent-unit)
                           (meets ?rightmost-agent-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (referent ?arg2)
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (referent ?arg2)
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))))


(def-fcg-cxn incorrect-ditransitive-argument-structure-cxn
             ((?incorrect-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject))
               (error-cat (error incorrect-case-choice)
                         (reason the-agent-should-be-in-nominative-another-accusative-already-exists-in-the-sentence-for-the-patient)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?am - - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?am - - -))))
                        (sem-cat (animacy animate))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ - - ?an -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ps - - ?an -)))
                        )
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                               (+ - - ?an -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ps - - ?an -)))
                        )
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (referent ?arg2))
              
              (?incorrect-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v missing-because-of-incorrect-case-choice)
                              (:arg1 ?v ?arg1)
                              (:arg1-error ?v ?arg0)
                              (:arg2 ?v ?arg2)))                  
               --
               ))
             :cxn-set mal-cxn)


(comprehend "dem Clown verkauft den Doktor das Buch")


(def-fcg-cxn incorrect-receiver-in-ditransitive-argument-structure-cxn
             ((?incorrect-receiver-in-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject))
               (error-cat (error incorrect-case-choice)
                         (reason the-receiver-should-be-in-dative-another-accusative-already-exists-in-the-sentence-for-the-patient)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
                        (sem-cat (animacy animate))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ - - - +)         
                               (- - - - -)        
                               (- - - - -)
                               (- - - - +)))
                        )
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                               (+ - - - +)         
                               (- - - - -)        
                               (- - - - -)
                               (- - - - +)))
                        )
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
              (referent ?arg2))
              
              (?incorrect-receiver-in-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg1-error ?v ?arg2)
                              (:arg2 ?v missing-because-of-incorrect-case-choice)))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "den Direktor schenkt die Lehrerin die Blumen")


(def-fcg-cxn incorrect-receiver-and-patient-in-ditransitive-argument-structure-cxn
             ((?incorrect-receiver-and-patient-in-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject))
               (error-cat (error incorrect-case-choice)
                         (reason the-receiver-should-be-in-dative-another-accusative-already-exists-in-the-sentence-for-the-patient)))
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (error-cat (error incorrect-number-choice)
                         (reason the-patient-should-be-plural-according-to-stimulus)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
                        (sem-cat (animacy animate))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ - + - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -)))
                        )
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                               (+ - + - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -)))
                        )
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
              (referent ?arg2))
              
              (?incorrect-receiver-and-patient-in-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg1-error ?v ?arg2)
                              (:arg2 ?v missing-because-of-incorrect-case-choice)))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "den Direktor schenkt die Lehrerin die Blume")

(def-fcg-cxn undetermined-patient-in-ditransitive-argument-structure-cxn
             ((?undetermined-patient-in-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject))
               (error-cat (error undetermined-noun)
                         (reason variation-from-received-stimulus-no-determiner-in-patient)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
                        (sem-cat (animacy animate))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun)
                        (case ((- - - - -) 
                               (+ - - - +)         
                               (- - - - -)        
                               (- - - - -)
                               (- - - - +)))
                        )
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun)
                       (case ((- - - - -) 
                               (+ - - - +)         
                               (- - - - -)        
                               (- - - - -)
                               (- - - - +)))
                        )
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (?dat ?dm - - -)
                      (+ + - - -))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (?dat ?dm - - -)
                      (+ + - - -))))
              (referent ?arg2))
              
              (?undetermined-patient-in-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)
                              ))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "dem Direktor schenkt die Lehrerin Blumen")

(def-fcg-cxn arg0-arg1-topic-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg2 +)))  
                          
               --
               (HASH form ((meets ?rightmost-receiver-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-agent-unit)
                           (meets ?rightmost-agent-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (referent ?arg2)
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (referent ?arg2)
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              )
             :cxn-set mal-cxn) 


(comprehend "dem Clown verkauft den Doktor das Buch")


(def-fcg-cxn double-acc-masc-in-ditransitive-argument-structure-cxn
             ((?double-acc-masc-in-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject))
               )
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object))
               (error-cat (error incorrect-case-choice)
                         (reason the-receiver-should-be-in-dative-another-accusative-already-exists-in-the-sentence-for-the-patient)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ - + - -) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ - + - -))))
                        (sem-cat (animacy animate))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ + - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ + - - ))))
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                               (+ + - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (+ + - - ))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
               (sem-cat (animacy animate))
               (referent ?arg2)
               
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (+ + - - -)         
                      (- - - - -)         
                      (- - - - -)
                      (+ + - - -))))
              (sem-cat (animacy animate))
              (referent ?arg2))
              
              
              (?double-acc-masc-in-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg1-error ?v ?arg2)
                              (:arg2 ?v missing-because-of-incorrect-case-choice)))                  
               --
               ))
             :cxn-set mal-cxn)

(def-fcg-cxn arg0-arg1-topic-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg1-error +)))  
                          
               --
               (HASH form ((meets ?rightmost-receiver-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-agent-unit)
                           (meets ?rightmost-agent-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (referent ?arg1-error)
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (referent ?arg1-error)
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))))
             :cxn-set mal-cxn)

(comprehend "den Mann gibt die Frau den Apfel")


;;;;FORMULATION

;;;dem Clown verkauft der Doktor das Buch
;(formulate '((verkaufen-01 s) (doctor d) (clown c) (book b) (arg0 s d) (arg1 s b) (arg2 s c) (topicalized c +)))

;;;dem Direktor schenkt die Lehrerin die Blumen
;(formulate '((teacher t) (flowers f) (director d) (schenken-01 g) (arg2 g d) (arg1 g f) (arg0 g t) (topicalized d +)))


;;;;;COMPREHENSION
(comprehend "dem Mann gibt die Frau den Apfel")
(comprehend "dem Clown verkauft der Doktor das Buch")
(comprehend "dem Direktor schenkt die Lehrerin die Blumen")


;;;;ERRORS

(comprehend "dem Clown verkauft den Doktor das Buch")  ;DOUBLE ACC NO SUBJ
(comprehend "dem Clown verkauft dem Doktor das Buch")  ;DOUBLE DATIVE
(comprehend "den Direktor schenkt die Lehrerin die Blumen")
(comprehend "den Mann gibt die Frau den Apfel")
(comprehend "den Direktor schenkt die Lehrerin die Blume")
(comprehend "dem Direktor schenkt die Lehrerin Blumen")


