(ql:quickload :ont-alignment)
(in-package :ont-alignment)

(defparameter *experiment*
  (make-instance 'ont-alignment-experiment))

(run-interaction *experiment*)

;(disconnect-toplevel)
;(connect-toplevel "db2_actors_films_simple_table.db" "postgres" "postgres" "localhost")

;(execute-postmodern-query '(:select (:count film) :from actorsfilms :where (:= actor "Gerard Depardieu")))
;(execute-postmodern-query '(:select (:count actor) (:avg year) :from actorsfilms))
;(execute-postmodern-query '(:select film :from actorsfilms :where (:in actor (:set "G�rard Depardieu" "Fred Astaire"))))
;(execute-postmodern-query '(:select film :from actorsfilms :where (:not (:in actor (:set "G�rard Depardieu" "Fred Astaire")))))
;(execute-postmodern-query '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100)))
;(execute-postmodern-query '(:select actor film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 2.5))))
;(execute-postmodern-query '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101)))
;(execute-postmodern-query '(:select actor_id :from actors :where (:in actor (:set "Gerard Depardieu" "Fred Astaire" "Brigitte Bardot"))))

;; '(:select actor :from actorsfilms)
'((bind ?select-clause actor)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms))

;; '(:select actor film :from actorsfilms)
(defparameter *first-test*
  '((comma ?select-clause ?column-1 ?column-2)
    (bind column ?column-1 actor)
    (bind column ?column-2 film)
    (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
    (bind table ?from-clause actorsfilms)))

(execute-postmodern-query '(:select (:- year rating) :from actorsfilms))
(execute-postmodern-query '(:select rating :from actorsfilms :group-by rating :order-by-size t))
(execute-postmodern-query '(:select film :from actorsfilms :where (:< (:/ year rating) 3000)))

;(ql:quickload :irl)
;(irl:draw-irl-program *first-test* :open t)

;; '(:select actor film year :from actorsfilms)
(defparameter *second-test*
  '((comma ?comma-clause-1 ?column-1 ?column-2)
    (comma ?select-clause ?comma-clause-1 ?column-3)
    (bind column ?column-1 actor)
    (bind column ?column-2 film)
    (bind column ?column-3 year)
    (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
    (bind table ?from-clause actorsfilms)))

;(irl:draw-irl-program *second-test* :open t)

;; '(:select (:count actor) :from actorsfilms)
'((count ?select-clause ?column-1)
  (bind column ?column-1 actor)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms))

;; '(:select (:count actor) (:avg year) :from actorsfilms)
(defparameter *fourth-test*
              '((comma ?select-clause ?aggregate-clause-1 ?aggregate-clause-2)
                (count ?aggregate-clause-1 ?column-1)
                (average ?aggregate-clause-2 ?column-2)
                (bind column ?column-1 actor)
                (bind column ?column-2 year)
                (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
                (bind table ?from-clause actorsfilms)))

;(irl:draw-irl-program *fourth-test* :open t)
;;max
;;min

;; (execute-postmodern-query '(:select film :from actorsfilms :where (:= actor "Gerard Depardieu")))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-2 ?comparator-1)
  (bind column ?column-2 actor)
  (bind concept ?comparator-1 "Gerard Depardieu"))

;; '(:select film :from actorsfilms :where (:in actor (:set "Gerard Depardieu" "Fred Astaire")))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (where ?where-clause ?filter-1)
  (in ?filter-1 ?column-2 ?set-clause)
  (bind column ?column-2 actor)
  (set ?set-clause ?set-values)
  (bind set ?set-values ("G�rard Depardieu" "Fred Astaire")))

;; '(:select film :from actorsfilms :where (:not (:= actor "Gerard Depardieu")))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (where ?where-clause ?filter-1)
  (not ?filter-1 ?filter-2)
  (equals ?filter-2 ?column-1 ?comparator-1)
  (bind column ?column-1 actor)
  (bind concept ?comparator-1 "Gerard Depardieu"))

;; '(:select film :from actorsfilms :where (:not (:in actor (:set "Gerard Depardieu" "Fred Astaire")))))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (where ?where-clause ?filter-1)
  (not ?filter-1 ?filter-2)
  (in ?filter-2 ?column-1 ?set-clause)
  (bind column ?column-1 actor)
  (set ?set-clause ?set-values)
  (bind set ?set-values ("G�rard Depardieu" "Fred Astaire")))

;; '(:select actor (:count film) :from actorsfilms :group-by actor :having (:> (:count film) 100))
'((count ?select-clause ?column-1)
  (bind column ?column-1 actor)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (group-by ?group-by-clause ?column-2 ?having-clause)
  (bind column ?column-2 film)
  (having ?having-clause ?filter-1)
  (bigger-than ?filter-1 ?aggregate-clause-1 ?comparator-1)
  (count ?aggregate-clause-1 ?column-2)
  (bind concept ?comparator-1 100))

;; '(:select actor (:count film) :from actorsfilms :group-by actor :having (:between (:count film) 100 101))
'((count ?select-clause ?column-1)
  (bind column ?column-1 actor)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (group-by ?group-by-clause ?column-2 ?having-clause)
  (bing column ?column-2 actor)
  (having ?having-clause ?filter-1)
  (between ?filter-1 ?aggregate-clause-1 ?lower-bound ?higher-bound)
  (count ?aggregate-clause-1 ?column-2)
  (bind concept ?lower-bound 100)
  (bind concept ?higher-bound 101))

;; '(:select film :from actorsfilms :where (:and (:= actor "Gerard Depardieu") (:<= rating 5)))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause actorsfilms)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?condition-1 ?condition-2)
  (equals ?condition-1 ?column-1 ?comparator-1)
  (lower-than ?condition-2 ?column-2 ?comparator-2)
  (bind column ?column-1 actor)
  (bind column ?column-2 rating)
  (bind concept ?comparator-1 "Gerard Depardieu")
  (bind concept ?comparator-2 5))


;(disconnect-toplevel)
;(connect-toplevel "db1_films_years.db" "postgres" "postgres" "localhost")

;; (execute-postmodern-query '(:select film :from films :inner-join years :on (:= films.film_id years.film_id) :where (:= years.year 2021)))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (inner-join ?join-clause-1 ?table-1 ?column-1 ?column-2 ?inner-join-clause-2)
  (bind table ?table-1 films)
  (dot ?column-1 ?table-1 ?column-3)
  (bind column ?column-3 film_id)
  (dot ?column-2 ?table-2 ?column-4)
  (bind table ?table-2 years)
  (bind column ?column-4 film_id)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-5 ?comparator-1)
  (dot ?column-5 ?table-2 ?column-6)
  (bind column ?column-6 year)
  (bind concept ?comparator-1 2021))

;; (execute-postmodern-query '(:select film :from films :where (:in film_id (:select film_id :from years :where (:= year 2021)))))
'((bind column ?select-clause film)
  (select ?result-1 ?select-clause-1 ?from-clause-1 ?join-clause-1 ?where-clause-1 ?group-by-clause-1)
  (bind table ?from-clause-1 films)
  (select ?result-2 ?select-clause-2 ?from-clause-2 ?join-clause-2 ?where-clause-2 ?group-by-clause-2)
  (bind table ?from-clause-2 years)
  (bind column ?column-1 film_id)
  (bind column ?column-2 year)
  (where ?where-clause-1 ?filter-1)
  (in ?filter-1 ?column-1 ?result-2)
  (where ?where-clause-2 ?filter-2)
  (equals ?filter-2 ?column-2 ?comparator-1)
  (bind concept ?comparator-1 2021))

;(disconnect-toplevel)
;(connect-toplevel "geography.db" "postgres" "postgres" "localhost")

;; "SELECT CITYalias0.CITY_NAME FROM CITY AS CITYalias0 WHERE CITYalias0.POPULATION = ( SELECT MAX( CITYalias1.POPULATION ) FROM CITY AS CITYalias1 WHERE CITYalias1.STATE_NAME = \"state_name0\" ) AND CITYalias0.STATE_NAME = \"state_name0\" ;
;; (execute-postmodern-query '(:select CITYalias0.CITY_NAME :from (:as CITY CITYalias0) :where (:and (:= CITYalias0.POPULATION (:select (:max CITYalias1.POPULATION) :from (:as CITY CITYalias1) :where (:= CITYalias1.STATE_NAME "wyoming"))) (:= CITYalias0.STATE_NAME "wyoming"))))
'((dot ?column-1 ?alias-1 ?column-2)
  (bind concept ?alias-1 CITYalias0)
  (bind column ?column-2 CITY_NAME)
  (select ?result-1 ?select-clause-1 ?from-clause-1 ?join-clause-1 ?where-clause-1 ?group-by-clause-1)
  (as ?from-clause-1 ?table-1 ?alias-1)
  (where ?where-clause-1 ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (equals ?filter-2 ?column-3 ?result-2)
  (dot ?column-3 ?alias-1 ?column-4)
  (bind column ?column-4 POPULATION)
  (select ?result-2 ?select-clause-2 ?from-clause-2 ?join-clause-2 ?where-clause-2 ?group-by-clause-2)
  (max ?select-clause-2 ?column-5)
  (dot ?column-5 ?alias-2 ?column-4)
  (bind concept ?alias-2 CITYalias1)
  (as ?from-clause-2 ?table-1 ?alias-2)
  (where ?where-clause-2 ?filter-4)
  (equals ?filter-4 ?column-6 ?comparator-1)
  (dot ?column-6 ?alias-2 ?column-7)
  (dot column ?column-7 STATE_NAME)
  (bind concept ?comparator-1 "wyoming")
  (equals ?filter-4 ?column-8 ?comparator-1)
  (dot ?column-8 ?alias-1 ?column-7))

;need to change : select-clause, from-clause (need :as possible), dot predicate (can take an alias as a second argument)

;; (execute-postmodern-query '(:select (:distinct film) :from films :inner-join years :on (:= films.film_id years.film_id) :where (:= years.year 2021)))
'((distinct ?select-clause ?column-1)
  (bind column ?column-1 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 film_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind table ?table-2 years)
  (bind column ?column-5 film_id)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-6 ?comparator-1)
  (dot ?column-6 ?table-2 ?column-7)
  (bind column ?column-7 year)
  (bind concept ?comparator-1 2021))

;; (execute-postmodern-query '(:select (:count film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:between years.year 1915 1916)))
'((count ?select-clause ?column-1)
  (bind column ?column-1 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?inner-join-clause ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (bind table ?table-2 years)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 actor_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 actor_id)
  (where ?where-clause ?filter-1)
  (between ?filter-1 ?column-6 ?lower-bound ?higher-bound)
  (dot ?column-6 ?table-2 ?column-7)
  (bind column ?column-7 year)
  (bind concept ?lower-bound 1915)
  (bind concept ?higher-bound 1916))

;; (execute-postmodern-query '(:select (:count film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:between years.year 1915 1920) (:and (:not (:= years.year 1917)) (:not (:= years.year 1918))))))
'((count ?select-clause ?column-1)
  (bind column ?column-1 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (bind table ?table-2 years)
  (dot ?column-2 ?table-1 ?column-4)
  (bind table ?table-1 films)
  (bind column ?column-4 actor_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 actor_id)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (between ?filter-2 ?column-6 ?lower-bound ?higher-bound)
  (dot ?column-6 ?table-2 ?column-7)
  (bind column ?column-7 year)
  (bind concept ?lower-bound 1915)
  (bind concept ?higher-bound 1916)
  (and ?filter-3 ?filter-4 ?filter-5)
  (not ?filter-4 ?filter-6)
  (equals ?filter-6 ?column-6 ?comparator-1)
  (bind concept ?comparator-1 1917)
  (not ?filter-5 ?filter-7)
  (equals ?filter-7 ?column-6 ?comparator-2)
  (bind concept ?comparator-2 1918))
  

;; (execute-postmodern-query '(:select (:distinct films.film) years.year :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:= films.actor "Gerard Depardieu")))
'((comma ?select-clause ?distinct-clause-1 ?column-1)
  (distinct ?distinct-clause-1 ?column-2)
  (dot ?column-2 ?table-1 ?column-3)
  (dot ?column-1 ?table-2 ?column-4)
  (bind table ?table-1 films)
  (bind column ?column-3 film)
  (bind table ?table-2 years)
  (bind column ?column-4 year)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (inner-join ?join-clause-1 ?column-5 ?column-6 ?inner-join-clause-2)
  (dot ?column-5 ?table-1 ?column-7)
  (dot ?column-6 ?table-2 ?column-8)
  (bind column ?column-7 actor_id)
  (bind column ?column-8 actor_id)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-9 ?comparator-1)
  (dot ?column-9 ?table-1 ?column-10)
  (bind column ?column-10 actor)
  (bind concept ?comparator-1 "Gerard Depardieu"))

;; (execute-postmodern-query '(:select (:count (:distinct films.film)) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:= films.actor "Gerard Depardieu")))
'((count ?select-clause ?distinct-clause-1)
  (distinct ?distinct-clause-1 ?column-1)
  (dot ?column-1 ?table-1 ?column-2)
  (bind table ?table-1 films)
  (bind column ?column-2 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (inner-join ?join-clause-1 ?column-3 ?column-4 ?inner-join-clause-2)
  (dot ?column-3 ?table-1 ?column-5)
  (dot ?column-4 ?table-2 ?column-6)
  (bind column ?column-5 actor_id)
  (bind column ?column-6 actor_id)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-8 ?comparator-1)
  (dot ?column-8 ?table-1 ?column-9)
  (bind column ?column-9 actor)
  (bind concept ?comparator-1 "Gerard Depardieu"))

  
;; SELECT film FROM films INNER JOIN films.actor_id = years.actor_id WHERE actor = "Gerard Depardieu" OR actor "Brigitte Bardot" AND year = 1999
;;(execute-postmodern-query '(:select film :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:or (:= films.actor "Gerard Depardieu") (:= films.actor "Brigitte Bardot")) (:= years.year "1999"))))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?column-1 ?column-2 ?inner-join-clause-2)
  (dot ?column-1 ?table-1 ?column-3)
  (dot ?column-2 ?table-2 ?column-4)
  (bind table ?table-2 years)
  (bind column ?column-3 actor_id)
  (bind column ?column-4 actor_id)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (or ?filter-2 ?filter-4 ?filter-5)
  (equals ?filter-4 ?column-5 ?comparator-1)
  (dot ?column-5 ?table-1 ?column-6)
  (bind column ?column-6 actor)
  (bind concept ?comparator-1 "Gerard Depardieu")
  (equals ?filter-5 ?column-6 ?comparator-2)
  (bind concept ?comparator-2 "Brigitte Bardot")
  (equals ?filter-3 ?column-7 ?comparator-3)
  (dot ?column-7 ?table-2 ?column-8)
  (bind column ?column-8 year)
  (bind concept ?comparator-3 1999))


;;SELECT film FROM films INNER JOIN films.actor_id = years.actor_id WHERE actor = "Gerard Depardieu" AND year = 1999 AND film LIKE "Th%"
;;(execute-postmodern-query '(:select (:distinct film) :from films :inner-join years :on (:= films.actor_id years.actor_id) :where (:and (:and (:= films.actor "Gerard Depardieu") (:= years.year 1999)) (:like films.film "The%"))))

'((distinct ?select-clause ?column-1)
  (bind column ?column-1 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?column-2 ?column-3 ?inner-join-clause-2)
  (dot ?column-2 ?table-1 ?column-4)
  (dot ?column-3 ?table-2 ?column-5)
  (bind table ?table-2 years)
  (bind column ?column-4 actor_id)
  (bind column ?column-5 actor_id)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (or ?filter-2 ?filter-4 ?filter-5)
  (equals ?filter-4 ?column-6 ?comparator-1)
  (dot ?column-6 ?table-1 ?column-7)
  (bind column ?column-7 actor)
  (bind concept ?comparator-1 "Gerard Depardieu")
  (equals ?filter-5 ?column-8 ?comparator-2)
  (dot ?column-8 ?table-2 ?column-9)
  (bind column ?column-9 year)
  (bind concept ?comparator-2 1999)
  (like ?filter-3 ?column-10 ?comparator-3)
  (dot ?column-10 ?table-1 ?column-2)
  (bind concept ?comparator-3 "The%"))



;(disconnect-toplevel)
;(connect-toplevel "db3_actors_films_multiple_tables.db" "postgres" "postgres" "localhost")

;; (execute-postmodern-query '(:select (:count film) :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:= actors.actor "Gerard Depardieu")))
'((count ?select-clause ?column-2)
  (bind column ?column-2 film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-3 ?column-4 ?inner-join-clause-2)
  (bind table ?table-2 actorfilm_relations)
  (dot ?column-3 ?table-1 ?column-5)
  (bind column ?column-3 film_id)
  (dot ?column-4 ?table-2 ?column-6)
  (bind column ?column-6 film_id)
  (inner-join ?inner-join-clause-2 ?table-3 ?column-7 ?column-8 ?inner-join-clause-3)
  (bind table ?table-3 actors)
  (dot ?column-7 ?table-2 ?column-9)
  (dot ?column-8 ?table-3 ?column-10)
  (bind column ?column-9 actor_id)
  (bind column ?column-10 actor_id)
  (where ?where-clause ?filter-1)
  (equals ?filter-1 ?column-11 ?comparator-1)
  (dot ?column-11 ?table-3 ?column-12)
  (bind column ?column-12 actor)
  (bind concept ?comparator-1 "Gerard Depardieu"))
  

;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:= actors.actor "Gerard Depardieu") (:= films.year 1996))))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (bind table ?table-2 actorfilm_relations)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 film_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 film_id)
  (inner-join ?inner-join-clause-2 ?table-3 ?column-6 ?column-7 ?inner-join-clause-3)
  (bind table ?table-3 actors)
  (dot ?column-6 ?table-2 ?column-8)
  (dot ?column-7 ?table-3 ?column-9)
  (bind column ?column-8 actor_id)
  (bind column ?column-9 actor_id)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (equals ?filter-2 ?column-10 ?comparator-1)
  (dot ?column-10 ?table-3 ?column-11)
  (bind column ?column-11 actor)
  (bind concept ?comparator-1 "Gerard Depardieu")
  (equals ?filter-3 ?column-12 ?comparator-2)
  (dot ?column-12 ?table-1 ?column-13)
  (bind column ?column-13 year)
  (bind concept ?comparator-2 1996))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:and (:like actors.actor "Jean%") (:> films.year 2020))))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (bind table ?table-2 actorfilm_relations)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 film_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 film_id)
  (inner-join ?inner-join-clause-2 ?table-3 ?column-6 ?column-7 ?inner-join-clause-3)
  (bind table ?table-3 actors)
  (dot ?column-6 ?table-2 ?column-8)
  (dot ?column-7 ?table-3 ?column-9)
  (bind column ?column-8 actor_id)
  (bind column ?column-9 actor_id)
  (where ?where-clause ?filter-1)
  (and ?filter-1 ?filter-2 ?filter-3)
  (like ?filter-2 ?column-10 ?comparator-1)
  (dot ?column-10 ?table-3 ?column-11)
  (bind column ?column-11 actor)
  (bind concept ?comparator-1 "Jean%")
  (greater-than ?filter-3 ?column-12 ?comparator-2)
  (dot ?column-12 ?table-2 ?column-13)
  (bind column ?column-13 year)
  (bind concept ?comparator-2 2020))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) :group-by films.film :having (:> (:avg films.rating) 7.5)))
'((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?from-clause films)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?inner-join-clause-2)
  (bind table ?table-2 actorfilm_relations)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 film_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 film_id)
  (inner-join ?inner-join-clause-2 ?table-3 ?column-6 ?column-7 ?inner-join-clause-3)
  (bind table ?table-3 actors)
  (dot ?column-6 ?table-2 ?column-8)
  (dot ?column-7 ?table-3 ?column-9)
  (bind column ?column-8 actor_id)
  (bind column ?column-9 actor_id)
  (where ?where-clause ?filter-1)
  (in ?filter-1 ?column-10 ?set-clause-1)
  (dot ?column-10 ?table-3 ?column-11)
  (bind column ?column-11 actor)
  (set ?set-clause-1 ?set-values-1)
  (bind set ?set-values-1 ("Gerard Depardieu" "Fred Astaire"))
  (group-by ?group-by-clause ?column-12 ?having-clause)
  (dot ?column-12 ?table-1 ?column-1)
  (having ?having-clause ?filter-2)
  (bigger-than ?filter-2 ?aggregate-clause-1 ?comparator-1)
  (average ?aggregate-clause-1 ?column-13)
  (dot ?column-13 ?table-1 ?column-14)
  (bind column ?column-14 rating)
  (bind concept ?comparator-1 7.5))


;; (execute-postmodern-query '(:select film :from films :inner-join actorfilm_relations :on (:= films.film_id actorfilm_relations.film_id) :inner-join actors :on (:= actorfilm_relations.actor_id actors.actor_id) :where (:or (:in actors.actor (:set "Gerard Depardieu" "Fred Astaire")) (:not (:like actors.actor "Brigitte"))) :group-by films.film :having (:> (:avg films.rating) 9)))
(defparameter *test-network* '((bind column ?select-clause film)
  (select ?result ?select-clause ?from-clause ?join-clause-1 ?where-clause ?group-by-clause)
  (bind table ?table-1 films)
  (inner-join ?join-clause-1 ?table-2 ?column-2 ?column-3 ?join-clause-2)
  (bind table ?from-clause actorfilm_relations)
  (bind table ?table-2 actorfilm_relations)
  (dot ?column-2 ?table-1 ?column-4)
  (bind column ?column-4 film_id)
  (dot ?column-3 ?table-2 ?column-5)
  (bind column ?column-5 film_id)
  (inner-join ?join-clause-2 ?table-3 ?column-6 ?column-7 ?join-clause-3)
  (bind table ?table-3 actors)
  (dot ?column-6 ?table-2 ?column-8)
  (dot ?column-7 ?table-3 ?column-9)
  (bind column ?column-8 actor_id)
  (bind column ?column-9 actor_id)
  (where ?where-clause ?filter-1)
  (or ?filter-1 ?filter-2 ?filter-3)
  (in ?filter-2 ?column-10 ?set-clause-1)
  (dot ?column-10 ?table-3 ?column-11)
  (bind column ?column-11 actor)
  (set ?set-clause-1 ?set-values-1)
  (bind set ?set-values-1 ("Gerard Depardieu" "Fred Astaire"))
  (not ?filter-3 ?filter-4)
  (like ?filter-4 ?column-10 ?comparator-1)
  (bind concept ?comparator-1 "Brigitte")
  (group-by ?group-by-clause ?column-12 ?having-clause)
  (dot ?column-12 ?table-1 ?column-1)
  (having ?having-clause ?filter-5)
  (bigger-than ?filter-5 ?aggregate-clause-2 ?comparator-2)
  (average ?aggregate-clause-2 ?column-13)
  (dot ?column-13 ?table-1 ?column-14)
  (bind column ?column-14 rating)
  (bind concept ?comparator-2 9)))

;(ql:quickload :irl)
;(irl:draw-irl-program *test-network* :open t)




 