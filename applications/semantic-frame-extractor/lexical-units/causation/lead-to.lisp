(in-package :frame-extractor)

(def-fcg-cxn lead-to-verb-lex
              ((?lead-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (syn-valence (subject ?subject-unit)
                            (object ?object-unit))
               (sem-valence (actor ?cause)
                            (theme ?effect))
               (meaning ((frame causation lead-to ?frame) 
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect))))
              <-
              (?lead-unit
               --
               (syn-cat (lex-class verb))
               (lex-id lead-to)
               (dependents (?to-unit)))
              )
              :cxn-set lex)

(def-fcg-cxn lead->leads-morph
             (
              <-
              (?leads-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id lead-to)
               --
               (form ((string ?leads-unit "leads")))))
             :cxn-set morph)


(def-fcg-cxn lead->leading-morph
             (
              <-
              (?leading-unit
               (syn-cat (lex-class verb)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id lead-to)
               --
               (form ((string ?leading-unit "leading")))))
            :cxn-set morph)

(def-fcg-cxn lead->lead-morph
             (
              <-
              (?lead-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id lead-to)
               --
               (form ((string ?lead-unit "lead")))))
             :cxn-set morph)

(def-fcg-cxn lead->led-morph
             (
              <-
              (?led-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id lead-to)
               --
               (form ((string ?led-unit "led")))))
            :cxn-set morph)