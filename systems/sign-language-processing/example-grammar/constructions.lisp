(in-package :slp)

;;---------------------------;;
;; single-sign constructions ;;
;;---------------------------;;

(def-fcg-cxn fs_mississippi-cxn-1
             ((?fs_mississippi-unit
               (sign-cat fs_mississippi-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               (args ((target ?f)
                      (source ?g)))
               (location neutral))
              <-
              (?fs_mississippi-unit
               (HASH meaning ((mississippi ?g)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?fs_mississippi-unit ""))))))

(def-fcg-cxn fs_arkansas-cxn-1
             ((?fs_arkansas-unit
               (sign-cat fs_arkansas-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               (args ((target ?e)
                      (source ?f)))
               (location neutral))
              <-
              (?fs_arkansas-unit
               (HASH meaning ((arkansas ?f)
                              (stateid ?e ?f)))
               --
               (HASH form ((right-hand-articulation ?fs_arkansas-unit ""))))))

(def-fcg-cxn fs_virginie-cxn-1
             ((?fs_virginie-unit
               (sign-cat fs_virginie-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               (args ((target ?e)
                      (source ?f)))
               (location neutral))
              <-
              (?fs_virginie-unit
               (HASH meaning ((virginia ?f)
                              (stateid ?e ?f)))
               --
               (HASH form ((right-hand-articulation ?fs_virginie-unit ""))))))

(def-fcg-cxn ds_bent5_etat+-cxn-1
             ((?ds_bent5_etat+-unit
               (sign-cat ds_bent5_etat+-1-cat)
               (sem-class state-entity-cat)
               (args ((target ?b)
                      (scope ?e)))
               (number pl)
               (location neutral))
              <-
              (?ds_bent5_etat+-unit
               (HASH meaning ((state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?ds_bent5_etat+-unit ""))))))

(def-fcg-cxn riviere-cxn-1
             ((?riviere-unit
               (sign-cat riviere-1-cat)
               (sem-class river-entity-cat)
               (args ((target ?a)
                      (scope ?d)))
               (number sg)
               (location neutral))
              <-
              (?riviere-unit
               (HASH meaning ((river ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?riviere-unit ""))))))

(def-fcg-cxn ville-cxn-1
             ((?ville-unit
               (sign-cat ville-1-cat)
               (sem-class city-entity-cat)
               (args ((target ?a)
                      (scope ?d)))
               (number sg)
               (location neutral))
              <-
              (?ville-unit
               (HASH meaning ((city ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?ville-unit ""))))))

(def-fcg-cxn haut-cxn
             ((?haut-unit
               (sign-cat haut-1-cat)
               (sem-class property)
               (args ((source ?b)
                      (target ?a)
                      (scope ?e)))
               (number sg)
               (location right))
              <-
              (?haut-unit
               (HASH meaning ((high_point ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?haut-unit ""))))))

(def-fcg-cxn ds_bent5_etat-cxn-1
             (<-
              (?ds_bent5_etat-unit
               (sign-cat ds_bent5_etat-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               (location left)
               --
               (HASH form ((left-hand-articulation ?ds_bent5_etat-unit ""))))))

(def-fcg-cxn pt_det_loc_5+-ds-carte-cxn
             ((?pt-unit
               (location neutral))
              <-
              (?pt-unit
               (sign-cat pt_det_loc_5+-ds-carte-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               --
               (HASH form ((right-hand-articulation ?pt-unit  "")
                           (left-hand-articulation ?ds_carte-unit "")
                           (start-coincides ?pt-unit ?ds_carte-unit)
                           (end-coincides ?pt-unit ?ds_carte-unit))))))


;;------------------------------;;
;; sentence-level constructions ;;
;;------------------------------;;


(def-fcg-cxn figure-adjacent-to-ground-cxn-1
             ((?figure-ground-unit
               (subunits (?figure-unit ?ground-unit ?palm-up-unit ?dans-unit ?il-y-a-unit))
               (args ((target ?b)
                      (scope ?e)))
               (sentence-type declarative)
               (boundaries ((rh-left-boundary ?palm-up-unit)
                            (rh-right-boundary ?figure-unit))))
              (?ground-unit
               (subunits (?ground-entity-cat-unit ?ground-classifier-unit))
               (sem-class entity-cat))
              <-
              (?palm-up-unit
               --
               (HASH form ((two-hand-articulation ?palm-up-unit ""))))
              (?dans-unit
               --
               (HASH form ((two-hand-articulation ?dans-unit ""))))
              (?il-y-a-unit
               --
               (HASH form ((right-hand-articulation ?il-y-a-unit ""))))
              (?figure-ground-unit
               (HASH meaning ((next_to ?e ?b ?c)
                              (const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?palm-up-unit ?dans-unit)
                           (adjacent ?dans-unit ?ground-entity-cat-unit)
                           (adjacent ?ground-entity-cat-unit ?il-y-a-unit)
                           (adjacent ?il-y-a-unit ?ground-classifier-unit)
                           (adjacent ?il-y-a-unit ?figure-unit)
                           (start-coincides ?figure-unit ?ground-classifier-unit))))
             (?ground-entity-cat-unit
               (sem-class ?sem-class)
               (args ((target ?f)
                      (source ?g)))
               (sign-cat ground-entity-cat)
               --
               (sign-cat ground-entity-cat)
               (number ?number))
             (?ground-classifier-unit
              --
              (sem-class ?sem-class)
              (sign-cat ground-pronominal-classifier-cat)
              (location left)
              (number ?number))
             (?figure-unit
              (args ((target ?b)
                     (scope ?e)))
              (sem-class entity-cat)
              (sign-cat figure-pronominal-classifier-cat)
              --
              (location neutral)
              (sign-cat figure-pronominal-classifier-cat))))

(def-fcg-cxn pt_loc_1_+-anaphor-cxn
             ((?pt_loc_1_+-unit
               (location neutral)
               (number pl))
              <-
              (?antecedent-unit
               (args ((target ?b)
                      (scope ?e)))
               (sem-class entity-cat)
               --
               (sign-cat antecedent-cat)
               (location neutral)
               (number pl))
              (?pt_loc_1_+-unit
               (args ((target ?b)
                      (scope ?e)))
               (sign-cat pronoun-cat)
               (sem-class entity-cat)
               --
               (HASH form ((right-hand-articulation ?pt_loc_1_+-unit ""))))))

(def-fcg-cxn pronoun-property-question-cxn-1
             ((?question-unit
               (args ((target ?b)
                      (scope ?e)))
               (subunits (?pronoun-unit ?property-unit ?quoi-unit))
               (sentence-type interrogative)
               (boundaries ((rh-left-boundary ?pronoun-unit)
                            (rh-right-boundary ?quoi-unit))))
              (?pronoun-unit
               (args ((target ?b)
                      (scope ?e)))
               (sem-class entity-cat))
              <-
              (?quoi-unit
               --
               (HASH form ((right-hand-articulation ?quoi-unit ""))))
              (?question-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((adjacent ?pronoun-unit ?property-unit)
                           (adjacent ?property-unit ?quoi-unit))))
              (?pronoun-unit
               --
               (sign-cat pronoun-cat))
              (?property-unit
               (args ((source ?b)
                      (target ?a)
                      (scope ?e)))
               --
               (sign-cat property-cat))))

(def-fcg-cxn figure-within-ground-cxn-1
             ((?figure-ground-unit
               (subunits (?figure-unit ?ground-unit ?dans-unit-1 ?pt-unit-1 ?il-y-a-unit ?dans-unit-2 ?pt-unit-2))
               (args ((target ?a)
                      (scope ?d)))
               (sentence-type declarative)
               (boundaries ((rh-left-boundary ?dans-unit-1)
                            (rh-right-boundary ?pt-unit-2))))
              <-
              (?dans-unit-1
               --
               (HASH form ((two-hand-articulation ?dans-unit-1 ""))))
              (?pt-unit-1
               --
               (HASH form ((right-hand-articulation ?pt-unit-1 ""))))
              (?il-y-a-unit
               --
               (HASH form ((right-hand-articulation ?il-y-a-unit ""))))
              (?dans-unit-2
               --
               (HASH form ((two-hand-articulation ?dans-unit-2 ""))))
              (?pt-unit-2
               --
               (HASH form ((right-hand-articulation ?pt-unit-2 ""))))
              (?figure-ground-unit
               (HASH meaning ((loc ?d ?a ?b)
                              (const ?d ?b ?e)))
               --
               (HASH form ((adjacent ?dans-unit-1 ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?ground-unit)
                           (adjacent ?ground-unit ?il-y-a-unit)
                           (adjacent ?il-y-a-unit ?dans-unit-2)
                           (adjacent ?dans-unit-2 ?figure-unit)
                           (adjacent ?figure-unit ?pt-unit-2))))
              (?ground-unit
               (sem-class entity-cat)
               (args ((target ?e)))
               --
               (sign-cat ground-entity-cat))
              (?figure-unit
               (sem-class entity-cat)
               (args ((target ?a)
                      (scope ?d)))
               --
               (sign-cat figure-entity-cat))))

(def-fcg-cxn figure-within-ground-cxn-2
             ((?figure-ground-unit
               (subunits (?figure-unit ?ground-unit ?dans-unit-1 ?dans-unit-2 ?il-y-a-unit))
               (args ((target ?a)
                      (scope ?d)))
               (sentence-type declarative)
               (boundaries ((rh-left-boundary ?dans-unit-1)
                            (rh-right-boundary ?figure-unit))))
              (?ground-unit
               (subunits (?ground-classifier-unit ?ground-entity-unit)))
              (?ground-classifier-unit
               (sem-class entity-cat))
              <-
              (?dans-unit-1
               --
               (HASH form ((two-hand-articulation ?dans-unit-1 ""))))
              (?dans-unit-2
               --
               (HASH form ((two-hand-articulation ?dans-unit-2 ""))))
              (?il-y-a-unit
               --
               (HASH form ((right-hand-articulation ?il-y-a-unit ""))))
              (?figure-ground-unit
               (HASH meaning ((loc ?d ?a ?b)
                              (const ?d ?b ?e)))
               --
               (HASH form ((adjacent ?dans-unit-1 ?ground-classifier-unit)
                           (adjacent ?ground-classifier-unit ?ground-entity-unit)
                           (adjacent ?ground-entity-unit ?dans-unit-2)
                           (adjacent ?dans-unit-2 ?il-y-a-unit)
                           (adjacent ?il-y-a-unit ?figure-unit))))
              (?ground-classifier-unit
               --
               (sign-cat ground-specifying-classifier-cat)
               (number ?number))
              (?ground-entity-unit
               (sem-class ?sem-class)
               (args ((target ?e)))
               --
               (sign-cat ground-entity-cat)
               (number ?number))
              (?figure-unit
               (sem-class entity-cat)
               (args ((target ?a)
                      (scope ?d)))
               --
               (sign-cat figure-entity-cat))))

(def-fcg-cxn question-cxn-1
             ((?question-unit
               (args ((target ?a)
                      (scope ?e)))
               (subunits (?quoi-unit-1 ?me-dire-unit ?nom-unit ?quoi-unit-2))
               (sentence-type interrogative)
               (boundaries ((rh-left-boundary ?quoi-unit-1)
                            (rh-right-boundary ?quoi-unit-2))))
              <-
              (?quoi-unit-1
               --
               (HASH form ((right-hand-articulation ?quoi-unit-1 ""))))
              (?me-dire-unit
               --
               (HASH form ((right-hand-articulation ?me-dire-unit ""))))
              (?nom-unit
               --
               (HASH form ((two-hand-articulation ?nom-unit ""))))
              (?quoi-unit-2
               --
               (HASH form ((right-hand-articulation ?quoi-unit-2 ""))))
              (?question-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((adjacent ?quoi-unit-1 ?me-dire-unit)
                           (adjacent ?me-dire-unit ?nom-unit)
                           (adjacent ?nom-unit ?quoi-unit-2))))))

(def-fcg-cxn question-cxn-2
             ((?question-unit
               (args ((target ?a)
                      (scope ?e)))
               (sentence-type interrogative)
               (subunits (?quoi-unit-1 ?palm-up-unit ?me-dire-unit))
               (boundaries ((rh-left-boundary ?quoi-unit-1)
                            (rh-right-boundary ?me-dire-unit))))
              <-
              (?quoi-unit-1
               --
               (HASH form ((right-hand-articulation ?quoi-unit-1 ""))))
              (?palm-up-unit
               --
               (HASH form ((right-hand-articulation ?palm-up-unit ""))))
              (?me-dire-unit
               --
               (HASH form ((two-hand-articulation ?me-dire-unit ""))))
              (?question-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((adjacent ?quoi-unit-1 ?palm-up-unit)
                           (adjacent ?palm-up-unit ?me-dire-unit))))))

;;--------------------------------;;
;; complex-sentence constructions ;;
;;--------------------------------;;

(def-fcg-cxn theme-question-cxn-1
             ((?theme-question-unit
               (subunits (?theme-unit ?question-unit)))
              <-
              (?theme-question-unit
               --
               (HASH form ((adjacent ?theme-rightmost-unit ?question-leftmost-unit))))
              (?theme-unit
               (args ((target ?a)
                      (scope ?e)))
               (sentence-type declarative)
               --
               (sentence-type declarative)
               (boundaries ((rh-right-boundary ?theme-rightmost-unit))))
              (?question-unit
               (args ((target ?a)
                      (scope ?e)))
               (sentence-type interrogative)
               --
               (sentence-type interrogative)
               (boundaries ((rh-left-boundary ?question-leftmost-unit))))))

;;--------------------;;
;; Categorial network ;;
;;--------------------;;


(let ((categorial-network (categorial-network *fcg-constructions*)))
  (add-categories
   '(entity-cat
     state-entity-cat
     river-entity-cat
     city-entity-cat
     fs_mississippi-1-cat
     fs_arkansas-1-cat
     fs_virginie-1-cat
     ds_bent5_etat+-1-cat
     riviere-1-cat
     ville-1-cat
     haut-1-cat
     ds_bent5_etat-1-cat
     pt_det_loc_5+-ds-carte-1-cat
     ground-entity-cat
     ground-pronominal-classifier-cat
     figure-pronominal-classifier-cat
     antecedent-cat
     pronoun-cat
     property-cat
     figure-entity-cat
     ground-specifying-classifier-cat) categorial-network)
  (add-link 'entity-cat 'state-entity-cat categorial-network)
  (add-link 'entity-cat 'river-entity-cat categorial-network)
  (add-link 'entity-cat 'city-entity-cat categorial-network)
  (add-link 'fs_mississippi-1-cat 'ground-entity-cat categorial-network)
  (add-link 'fs_arkansas-1-cat 'ground-entity-cat categorial-network)
  (add-link 'fs_virginie-1-cat 'ground-entity-cat categorial-network)
  (add-link 'ds_bent5_etat+-1-cat 'figure-pronominal-classifier-cat categorial-network)
  (add-link 'riviere-1-cat 'figure-entity-cat categorial-network)
  (add-link 'ville-1-cat 'figure-entity-cat categorial-network)
  (add-link 'haut-1-cat 'property-cat categorial-network)
  (add-link 'ds_bent5_etat-1-cat 'ground-pronominal-classifier-cat categorial-network)
  (add-link 'pt_det_loc_5+-ds-carte-1-cat ' pronoun-cat categorial-network)
  (add-link 'antecedent-cat 'ds_bent5_etat+-1-cat categorial-network)
  (add-link 'ground-specifying-classifier-cat 'pt_det_loc_5+-ds-carte-1-cat categorial-network))
               
                     

               
               



