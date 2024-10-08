;;;; same-relate.lisp

(in-package :clevr-grammar-v2)

;; ----------------------------------------------------- ;;
;; This file contains  grammatical constructions for the ;;
;; same relate question family                           ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn the-same-T-as-relate-cxn
             ((?same-type-unit
               (args ((sources ?source ?segmented-scene)
                      (target ?target)))
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (property-type ?type)
               (subunits (?the-same ?type-unit ?as))
               (leftmost-unit ?the-same)
               (rightmost-unit ?as))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-type-unit
               (HASH meaning ((same ?target ?source ?segmented-scene ?scene ?attribute)))
               --
               )
              (?the-same
               --
               (HASH form ((string ?the-same "the same"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?the-same ?type-unit)
                           (meets ?type-unit ?as))))
              (?as
               --
               (HASH form ((string ?as "as")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

;; same-relate-exist <- exist-unit + same-type-unit + det-np-unit
(def-fcg-cxn same-relate-exist-cxn
             ((?same-exist-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?exist-unit ?same-type-unit ?determined-noun-phrase-unit)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-exist-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?exist-unit
               (args ((sources ?exist-set)
                      (target ?target)))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (superunits nil)
               --
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit)
               (HASH form ((meets ?rightmost-exist-unit ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?exist-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (sem-cat (sem-function equal-property))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-exist-material <- exist-unit + "made of" + same-type-unit + det-np-unit
(def-fcg-cxn same-relate-exist-material-cxn
             ((?same-exist-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?exist-unit ?made-of ?same-type-unit ?determined-noun-phrase-unit)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-exist-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?exist-unit
               (args ((sources ?exist-set)
                      (target ?target)))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (material-suffix +)
               --
               (material-suffix +)
               (qtype same-relate)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-exist-unit ?made-of)
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?exist-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (sem-cat (sem-function equal-property))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-count <- count-unit + same-type-unit + det-np-unit
(def-fcg-cxn same-relate-count-cxn
             ((?same-count-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?count-unit ?same-type-unit ?determined-noun-phrase-unit)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-count-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?count-unit
               (args ((sources ?count-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (superunits nil)
               --
               (superunits nil)
               (qtype same-relate)
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (leftmost-unit ?leftmost-count-unit)
               (rightmost-unit ?rightmost-count-unit)
               (HASH form ((meets ?rightmost-count-unit ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?count-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?type)
               --
               (property-type ?type)
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; EXCEPTIONS
(def-fcg-cxn same-relate-count-material-cxn
             ((?same-count-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?count-unit ?made-of ?same-type-unit ?determined-noun-phrase-unit)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-count-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?count-unit
               (args ((sources ?count-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (material-suffix +)
               (superunits nil)
               --
               (superunits nil)
               (material-suffix +)
               (qtype same-relate)
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (leftmost-unit ?leftmost-count-unit)
               (rightmost-unit ?rightmost-count-unit))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-count-unit ?made-of)
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?count-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-query <- query-unit + det-np + "that is" + same-type-unit + det-np
(def-fcg-cxn same-relate-query-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-np-unit-1 ?that-is ?same-type-unit ?determined-np-unit-2)))
              (?determined-np-unit-1
               (footprints (query)))
              (?determined-np-unit-2
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
             (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)
               (HASH form ((meets ?rightmost-qt-unit ?leftmost-np-unit-1))))
             (?determined-np-unit-1
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
             (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
             (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit-2))))
             (?determined-np-unit-2
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
             
;; same-relate-query-other <- query-unit + "the" + "other" + singular nominal + "that is" + same-type-unit + det-np
(def-fcg-cxn same-relate-query-other-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-other-np-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit)))
              (?determined-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?the ?other ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit))
              (?the
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?the "the")
                           (meets ?rightmost-qt-unit ?the)
                           (meets ?the ?other))))
              (?other 
               --
               (HASH form ((string ?other "other")
                           (meets ?other ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
;; same-relate-query-reverse <- det-np + "that is" + same-type-unit + det-np + query-unit
(def-fcg-cxn same-relate-query-reverse-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?determined-np-unit-1 ?that-is ?same-type-unit ?determined-np-unit-2 ?query-type-unit)))
              (?determined-np-unit-1
               (footprints (query)))
              (?determined-np-unit-2
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?determined-np-unit-1
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit-2))))
              (?determined-np-unit-2
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)
               (HASH form ((meets ?rightmost-np-unit-2 ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position rear))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
;; same-relate-query-reverse-other <- "the" + "other" + singular nominal + "that is" + same-type-unit + det-np + query-unit
(def-fcg-cxn same-relate-query-reverse-other-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?determined-other-np-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit ?query-type-unit)))
              (?determined-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?the ?other ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?the
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?the "the")
                           (meets ?the ?other))))
              (?other
               --
               (HASH form ((string ?other "other")
                           (meets ?other ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)
               (HASH form ((meets ?rightmost-np-unit ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position rear))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-query-anaphoric <- declared-np + "that is" + same-type-unit + det-np + ";" + query-unit
(def-fcg-cxn same-relate-query-anaphoric-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit ?semicolon ?query-type-unit)))
              (?declared-noun-phrase-unit
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric +))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-query-anaphoric <- "there is another" + singular nominal + "that is" + same-type-unit + det-np + ";" + query-unit
(def-fcg-cxn same-relate-query-anaphoric-another-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-other-np-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit ?semicolon ?query-type-unit)))
              (?declared-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?there-is-unit ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?there-is-unit
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?there-is-unit "there is another")
                           (meets ?there-is-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric +))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; EXCEPTIONS
;; declared-np + "that is" + same-type-unit + det-np + ";" + "what is it made of"
(def-fcg-cxn same-relate-query-anaphoric-made-of-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit ?semicolon ?made-of-unit)))
              (?declared-noun-phrase-unit
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?made-of-unit))))
              (?made-of-unit
               (HASH meaning ((query ?target ?target-object ?scene ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               (HASH form ((string ?made-of-unit "what is it made of")))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; same-relate-query-anaphoric-anpther-made-of <- "there is another" + singular nominal + "that is" + same-type-unit + det-np + ";" + "what is it made of"
(def-fcg-cxn same-relate-query-anaphoric-another-made-of-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-other-np-unit ?that-is ?same-type-unit ?determined-noun-phrase-unit ?semicolon ?made-of-unit)))
              (?declared-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?there-is-unit ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?there-is-unit
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?there-is-unit "there is another")
                           (meets ?there-is-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type ?compare-type)
               --
               (property-type ?compare-type)
               (sem-cat (sem-function equal-property))
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?made-of-unit))))
              (?made-of-unit
               (HASH meaning ((query ?target ?target-object ?scene ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               (HASH form ((string ?made-of-unit "what is it made of")))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; query-unit + det-np + "that is" + "made of" + same-type-unit + det-np
(def-fcg-cxn same-relate-query-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-np-unit-1 ?that-is ?made-of ?same-type-unit ?determined-np-unit-2)))
              (?determined-np-unit-1
               (footprints (query)))
              (?determined-np-unit-2
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)
               (HASH form ((meets ?rightmost-qt-unit ?leftmost-np-unit-1))))
              (?determined-np-unit-1
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit-2))))
              (?determined-np-unit-2
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
;; query-unit + "the" + "other" + singular-nominal + "that is" + "made of" + same-type-unit + det-np
(def-fcg-cxn same-relate-query-other-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-other-np-unit ?that-is ?made-of ?same-type-unit ?determined-noun-phrase-unit)))
              (?determined-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?the ?other ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit))
              (?the
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?the "the")
                           (meets ?rightmost-qt-unit ?the)
                           (meets ?the ?other))))
              (?other 
               --
               (HASH form ((string ?other "other")
                           (meets ?other ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; det-np + "that is" + "made of" + same-type-unit + det-np + query-unit
(def-fcg-cxn same-relate-query-reverse-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?determined-np-unit-1 ?that-is ?made-of ?same-type-unit ?determined-np-unit-2 ?query-type-unit)))
              (?determined-np-unit-1
               (footprints (query)))
              (?determined-np-unit-2
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?determined-np-unit-1
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit-2))))
              (?determined-np-unit-2
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)
               (HASH form ((meets ?rightmost-np-unit-2 ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position rear))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
;; "the" + "other" + singular-nominal + "that is" + "made of" + same-type-unit + det-np + query-unit
(def-fcg-cxn same-relate-query-reverse-other-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?determined-other-np-unit ?that-is ?made-of ?same-type-unit ?determined-noun-phrase-unit ?query-type-unit)))
              (?determined-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?the ?other ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?the
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?the "the")
                           (meets ?the ?other))))
              (?other
               --
               (HASH form ((string ?other "other")
                           (meets ?other ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)
               (HASH form ((meets ?rightmost-np-unit ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position rear))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; declared-np + "that is" + "made of" + same-type-unit + det-np + ";" + query-unit
(def-fcg-cxn same-relate-query-anaphoric-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-np-unit ?that-is ?made-of ?same-type-unit ?determined-np-unit ?semicolon ?query-type-unit)))
              (?declared-np-unit
               (footprints (query)))
              (?determined-np-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?declared-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit-1 ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit-2))))
              (?determined-np-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit-2 ?semicolon)
                           (meets ?semicolon ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric +))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              

;; "there is another" + singular nominal + "that is" + "made of" + same-type-unit + det-np + ";" + query-unit
(def-fcg-cxn same-relate-query-anaphoric-another-material-cxn
             ((?same-query-unit
               (args ((sources ?segmented-scene)
                      (target ?target)))
               (subunits (?declared-other-np-unit ?that-is ?made-of ?same-type-unit ?determined-noun-phrase-unit ?semicolon ?query-type-unit)))
              (?declared-other-np-unit
               (args ((sources ?same-set)
                      (target ?target-object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?the)
               (rightmost-unit ?rightmost-nom-unit)
               (subunits (?there-is-unit ?singular-nominal-unit))
               (footprints (query)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (scene-unit
               --
               (scene ?scene))
              (?same-query-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               )
              (?there-is-unit
               (HASH meaning ((unique ?target-object ?nominal-set)))
               --
               (HASH form ((string ?there-is-unit "there is another")
                           (meets ?there-is-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?same-set)
                      (target ?nominal-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)
                           (meets ?that-is ?made-of))))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?made-of ?leftmost-type-unit))))
              (?same-type-unit
               (args ((sources ?object ?segmented-scene)
                      (target ?same-set)))
               (sem-cat (sem-function equal-property))
               (property-type material)
               --
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-type-unit)
               (rightmost-unit ?rightmost-type-unit)
               (HASH form ((meets ?rightmost-type-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?segmented-scene)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?target-object)
                      (target ?target)))
               (property-type ?query-type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric +))
               --
               (property-type ?query-type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
