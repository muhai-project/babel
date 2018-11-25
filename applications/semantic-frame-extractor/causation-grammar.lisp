;;;;; Grammatical constructions for the CAUSATION-frames of The Guardian Climate Change Corpus
;;;;; Revised version after switch to hybrid approach, June 2018
;;;;; Katrien Beuls (katrien@ai.vub.ac.be)
;;;;; ----------------------------------------------------------------------------------------

(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General argument structure constructions
;;-----------------------------------------------------------------


(def-fcg-cxn partial-active-actor-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (actor-arg-structure)))
              <-
              (?subject
               (referent ?x)
               --
               (dependency (edge nsubj))
               (head ?vp))
              (?vp
               --
               (referent ?ev)
               (sem-valence (actor ?x))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject))
               (footprints (not actor-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn partial-active-transitive-theme-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (theme-arg-structure)))
              <-
              (?object
               (referent ?y)
               --
               (dependency (edge dobj))
               (head ?vp))
              (?vp
               --
               (referent ?ev)
               (sem-valence (actor ?x)
                            (theme ?y))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject)
                            (object ?object))
               (footprints (not theme-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn active-transitive-actor-theme-cxn-subject-parataxis
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?conjunctive-unit
               --
               (dependency (pos-tag cc)
                           (edge cc)))
              (?vp
               --
               (head ?same-head)
               (referent ?ev)
               (sem-valence (actor ?x)
                            (theme ?y))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject)
                            (object ?object))
               (footprints (not arg-structure)))
              (?subject
               (referent ?x)
               --
               (dependency (edge nsubj))
               (head ?same-head))
              (?object
               (referent ?y)
               --
               (dependency (edge dobj))
               (head ?vp))
              )
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn partial-passive-transitive-actor-cxn
             ((?vp-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (actor-arg-structure)))
              <-
              (?subject-unit
               (referent ?x)
               --
               (dependency (edge nsubjpass))
               (head ?vp-unit))
              (?vp-unit
               --
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (dependency (pos-tag vbn))
               (syn-valence (subject ?subject-unit))
               (footprints (not actor-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X is caused by Y")


(def-fcg-cxn partial-passive-transitive-theme-cxn
             ((?vp-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (theme-arg-structure)))
              <-
              (?vp-unit
               --
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (dependency (pos-tag vbn))
               (footprints (not theme-arg-structure)))
              (?by
               --
               (dependency (edge agent))
               (head ?vp-unit))
              (?oblique-unit
               (referent ?y)
               --
               (dependency (edge pobj))
               (head ?by)))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X is caused by Y")


(def-fcg-cxn causative-to-cxn
             ((?vp-unit
               (syn-cat (voice active)
                        (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?subject-unit
               (referent ?y)
               --
               (dependency (edge nsubj))
               (head ?vp-unit))
              (?vp-unit
               --
               (sem-cat (frame causation))
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (syn-cat (lex-class verb))
               (syn-valence (subject ?subject-unit))
               (footprints (not arg-structure)))
              (?effect-unit
               (referent ?x)
                --
               (dependency (pos-tag vb)
                           (edge ccomp))
               (head ?vp-unit)))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X causes [Y to Zverb]")


(def-fcg-cxn perfect-infinitive-passive-cxn
             ((?caused-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (arg-structure)))
               <-
               (?to-unit
                --
                (head ?caused-unit)
                (form ((string ?to-unit "to"))))
               (?have-unit
                --
                (head ?caused-unit)
                (form ((string ?have-unit "have"))))
               (?been-unit
                --
                (head ?caused-unit)
                (form ((string ?been-unit "been"))))
               (?effect-unit
                (referent ?y)
                --
                (dependency (pos-tag nn))
                (head ?unknown-2))
               (?unknown-1
                --
                (head ?unknown-2))
               (?caused-unit
                --
                (head ?unknown-1)
                (referent ?ev)
                (sem-valence (actor ?x)
                             (theme ?y))
                (syn-cat (verb-form participle))
                (dependency (pos-tag vbn)
                            (edge xcomp))
                (syn-valence (subject ?effect-unit))
                (footprints (not arg-structure)))
               (?by-unit
                --
                (head ?caused-unit)
                (dependency (edge agent)))
               (?cause-unit
                (referent ?x)
                --
                (dependency (edge pobj))
                (head ?by-unit)))
             :cxn-set unhashed
             :description "Example sentence: X is likely to have been caused by Y")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame specific constructions for linking frame slots to units
;;-----------------------------------------------------------------


(def-fcg-cxn X-caused-by-Y
             (
              <-
              (?caused-unit
               (referent ?frame)
               --
               (sem-cat (frame causation))
               (meaning ((slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               (dependency (pos-tag vbn)
                           (edge acl)) 
               (head ?effect-unit)
               (dependents (?by-unit)))
              (?by-unit
               --
               (head ?caused-unit)
               (dependents (?cause-unit))
               (form ((string ?by-unit "by"))))
              (?cause-unit
               (referent ?cause)
               --
               (head ?by-unit)
               (dependency (edge pobj)))
              (?effect-unit
               (referent ?effect)
               --
               (dependents (?caused-unit)))
              )
             :cxn-set unhashed)

