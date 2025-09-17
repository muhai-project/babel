(in-package :fcg)

;; (ql:quickload :fcg-learn)
;; (deactivate-all-monitors)
(activate-monitor trace-fcg-learning)


(def-fcg-constructions empty-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (integration-form-args sequence)
                  (meaning-args sequence)
                  (integration-meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :hashed)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:hash-mode . :filler-and-linking)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs  repair-add-categorial-link repair-through-anti-unification)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :punish-non-gold-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.5)
                       (:best-solution-mode . :highest-average-link-weight)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:fix-selection-mode . :deepest-branch-with-highest-cxn-entrenchment)
                       (:form-generalisation-mode :altschul-erickson
                        ((:match-cost . 0)
                         (:mismatch-cost . 1)
                         (:gap-cost . 1)
                         (:gap-opening-cost . 50)
                         (:n-optimal-alignments . nil)
                         (:max-nr-of-gaps . 1)))
                       (:meaning-generalisation-mode . :exhaustive)
                       (:max-nr-of-nodes . 5000)
                       (:k-swap-k . 1)
                       (:k-swap-w . 1)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence-predicates)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:expand-nodes-in-search-tree . t)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))



(defparameter *how-many-cubes-are-there*
  (make-instance 'speech-act
                 :form "how many cubes are there?"
                 :meaning '((get-context context-5)
                            (filter set-5 context-5 shape-5)
                            (bind shape-category shape-5 cube)
                            (count number-5 set-5))))

(defparameter *how-many-red-cubes-are-there*
  (make-instance 'speech-act
                 :form "how many red cubes are there?"
                 :meaning '((get-context context-6)
                            (filter set-6 context-6 color-6)
                            (bind color-category color-6 red)
                            (filter set-7 set-6 shape-6)
                            (bind shape-category shape-6 cube)
                            (count number-6 set-7))))


(comprehend *how-many-cubes-are-there* :cxn-inventory *fcg-constructions*)
(comprehend *how-many-red-cubes-are-there* :cxn-inventory *fcg-constructions*)




;;++++++++++++++++++++++++++++++++++++++++++++
;; Substitution
;;++++++++++++++++++++++++++++++++++++++++++++
 
(defparameter *what-color-is-the-cube*
    (make-instance 'speech-act
                   :form "what color is the cube?"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 cube)
                              (unique object-1 set-1)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 color))))

(setf *fcg-constructions* (make-empty-cxn-inventory-cxns))
(comprehend *what-color-is-the-cube* :cxn-inventory *fcg-constructions*)
(formulate-all (instantiate-variables (comprehend *what-color-is-the-cube* :cxn-inventory *fcg-constructions*)))
(comprehend *what-color-is-the-cube* :cxn-inventory *fcg-constructions* :learn nil :align nil)

(defparameter *color*
    (make-instance 'speech-act
                   :form "color"
                   :meaning '((bind attribute-category attribute-1 color))))

(comprehend *color* :cxn-inventory *fcg-constructions*)


(defparameter *large*
    (make-instance 'speech-act
                   :form "large"
                   :meaning '((bind size-category attribute-1 large))))

(comprehend *large* :cxn-inventory *fcg-constructions*)



(defparameter *cube*
    (make-instance 'speech-act
                   :form "cube"
                   :meaning '((bind shape-category attribute-1 cube))))

(comprehend *cube* :cxn-inventory *fcg-constructions*)

(defparameter *size*
    (make-instance 'speech-act
                   :form "size"
                   :meaning '((bind attribute-category attribute-1  size))))

(comprehend *size* :cxn-inventory *fcg-constructions*)


(defparameter *what-size-is-the-cube*
  (make-instance 'speech-act
                 :form "what size is the cube?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))

(comprehend *what-size-is-the-cube* :cxn-inventory *fcg-constructions*)

(defparameter *what-size-is-the-large-cube*
  (make-instance 'speech-act
                 :form "what size is the large cube?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (filter set-3 set-2 size-2)
                            (bind size-category size-2 large)
                            (unique object-2 set-3)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))

(set-configuration (construction-inventory *saved-cipn*) :cxn-supplier-mode :holophrase-cxns-only)
(comprehend *what-size-is-the-large-cube* :cxn-inventory *fcg-constructions*)


(loop for leaf in (remove-if #'(lambda (node) (find 'duplicate (statuses node))) (get-cip-leaves (cip *saved-cipn*)))
      do (setf (cxn-supplier leaf) nil)
         (setf (fully-expanded? leaf) nil)
         (cip-enqueue leaf (cip *saved-cipn*) :depth-first))


(setf *A* (next-cip-solution (cip *saved-cipn*)))

(add-element (make-html-fcg-light (cip *A*)))

(loop for ()
(cip-enqueue cip :

(add-element (make-html (next-cip-solution (cip *saved-cipn*))))

(add-element (make-html (cip *saved-cipn*)))

(defparameter *material*
    (make-instance 'speech-act
                   :form "material"
                   :meaning '((bind attribute-category attribute-2 material))))

(comprehend *material* :cxn-inventory *fcg-constructions*)

(defparameter *what-material-is-the-cube*
  (make-instance 'speech-act
                 :form "what material is the cube?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 material))))

(comprehend *what-material-is-the-cube* :cxn-inventory *fcg-constructions*)





(setf *fcg-constructions* (make-empty-cxn-inventory-cxns))


(defparameter *how-many-blocks-are-there*
  (make-instance 'speech-act
                 :form "how many blocks are there?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (count answer set-2))))

(defparameter *how-many-spheres-are-there*
  (make-instance 'speech-act
                 :form "how many spheres are there?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 sphere)
                            (count answer set-2))))


(comprehend *how-many-blocks-are-there* :cxn-inventory *fcg-constructions*)
(comprehend *how-many-spheres-are-there* :cxn-inventory *fcg-constructions*)





(defparameter *what-is-the-color-of-the-cylinder*
  (make-instance 'speech-act
                 :form "what is the color of the cylinder?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cylinder)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 color))))

(defparameter *what-is-the-color-of-the-block*
  (make-instance 'speech-act
                 :form "what is the color of the block?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 color))))

(defparameter *what-is-the-color-of-the-sphere*
  (make-instance 'speech-act
                 :form "what is the color of the sphere?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 sphere)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 color))))


(comprehend *what-is-the-color-of-the-cylinder* :cxn-inventory *fcg-constructions*)
(comprehend *what-is-the-color-of-the-block* :cxn-inventory *fcg-constructions*)
(comprehend *what-is-the-color-of-the-sphere* :cxn-inventory *fcg-constructions*)

(inspect *saved-cipn*)


(defparameter *how-many-cubes-are-there*
  (make-instance 'speech-act
                 :form "how many cubes are there?"
                 :meaning '((get-context context-5)
                            (filter set-5 context-5 shape-5)
                            (bind shape-category shape-5 cube)
                            (count number-5 set-5))))

(defparameter *how-many-red-cubes-are-there*
  (make-instance 'speech-act
                 :form "how many red cubes are there?"
                 :meaning '((get-context context-6)
                            (filter set-6 context-6 color-6)
                            (bind color-category color-6 red)
                            (filter set-7 set-6 shape-6)
                            (bind shape-category shape-6 cube)
                            (count number-6 set-7))))


(comprehend *how-many-cubes-are-there* :cxn-inventory *fcg-constructions*)
(comprehend *how-many-red-cubes-are-there* :cxn-inventory *fcg-constructions*)





(let ((*fcg-constructions* (make-empty-cxn-inventory-cxns))
      (holophrastic-what-color-is-the-cube (induce-cxns *what-color-is-the-cube* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *what-size-is-the-cube* holophrastic-what-color-is-the-cube :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *what-color-is-the-cube*))
  (comprehend-all (form-string *what-size-is-the-cube*))
  (formulate-all (instantiate-variables (meaning *what-color-is-the-cube*))))


(comprehend *what-color-is-the-cube* :cxn-inventory (make-empty-cxn-inventory-cxns))

(categorial-network (make-empty-cxn-inventory-cxns))
next-cip-solution

;;++++++++++++++++++++++++++++++++++++++++++++
;; Deletion + addition
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *how-many-cubes-are-there* '((:form . ((sequence "how many blocks are there?" ?l3 ?r3)))
                                           (:meaning . ((get-context ?context-5)
                                                        (filter ?set-5 ?context-5 ?shape-5)
                                                        (bind shape-category ?shape-5 cube)
                                                        (count ?number-5 ?set-5)))))

(defparameter *how-many-red-cubes-are-there* '((:form . ((sequence "how many red cubes are there?" ?l4 ?r4)))
                                               (:meaning . ((get-context ?context-6)
                                                            (filter ?set-6 ?context-6 ?color-6)
                                                            (bind color-category ?color-6 red)
                                                            (filter ?set-7 ?set-6 ?shape-6)
                                                            (bind shape-category ?shape-6 cube)
                                                            (count ?number-6 ?set-7)))))

;; Deletion
;;--------------

(let (;(*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*))
  (formulate-all (instantiate-variables (meaning *how-many-red-cubes-are-there*))))


;; Addition
;;--------------

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-how-many-cubes-are-there (induce-cxns *how-many-cubes-are-there* nil :cxn-inventory *fcg-constructions*)))

  (induce-cxns *how-many-red-cubes-are-there* holophrastic-how-many-cubes-are-there :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*)))


;;++++++++++++++++++++++++++++++++++++++++++++
;; Deletion + Existing slot-cxn -> filler-cxn
;;++++++++++++++++++++++++++++++++++++++++++++



(defparameter *how-many-blue-cubes-are-there* '((:form . ((sequence "how many blue cubes are there?" ?l40 ?r40)))
                                                (:meaning . ((get-context ?context-60)
                                                             (filter ?set-60 ?context-60 ?color-60)
                                                             (bind color-category ?color-60 blue)
                                                             (filter ?set-70 ?set-60 ?shape-60)
                                                             (bind shape-category ?shape-60 cube)
                                                             (count ?number-60 ?set-70)))))

(defparameter *how-many-blue-spheres-are-there* '((:form . ((sequence "how many blue spheres are there?" ?l42 ?r42)))
                                                  (:meaning . ((get-context ?context-60)
                                                             (filter ?set-60 ?context-60 ?color-60)
                                                             (bind color-category ?color-60 blue)
                                                             (filter ?set-70 ?set-60 ?shape-60)
                                                             (bind shape-category ?shape-60 sphere)
                                                             (count ?number-60 ?set-70)))))

(defparameter *is-there-a-blue-metal-sphere* '((:form . ((sequence "is there a blue metal sphere?" ?l42 ?r42)))
                                                  (:meaning . ((EXIST ?VAR-3220854 ?VAR-3220853)
                                                               (FILTER ?VAR-3220853 ?VAR-3220851 ?VAR-3220852)
                                                               (FILTER ?VAR-3220851 ?VAR-3220849 ?VAR-3220850)
                                                               (FILTER ?VAR-3220849 ?VAR-3220847 ?VAR-3220848)
                                                               (BIND SHAPE-CATEGORY ?VAR-3220852 SPHERE)
                                                               (BIND COLOR-CATEGORY ?VAR-3220850 BLUE)
                                                               (BIND MATERIAL-CATEGORY ?VAR-3220848 METAL)
                                                               (GET-CONTEXT ?VAR-3220847)))))


(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      holophrastic-how-many-red-cubes-are-there
      how-many-X-cubes-are-there-cxn)
  
  ;;Learn a holophrastic cxn first
  (setf holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil :cxn-inventory *fcg-constructions*))

  ;;Learn a first slot cxn and one filler cxn for red
  (setf how-many-X-cubes-are-there-cxn
        (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there :cxn-inventory *fcg-constructions*))

  (comprehend-all (form-string *how-many-cubes-are-there*))
  (comprehend-all (form-string *how-many-red-cubes-are-there*))
  
  ;;Learn a filler cxn for blue
  (induce-cxns *how-many-blue-cubes-are-there* how-many-X-cubes-are-there-cxn :cxn-inventory *fcg-constructions*)
  (comprehend-all (form-string *how-many-blue-cubes-are-there*))
  (formulate-all (instantiate-variables (meaning *how-many-blue-cubes-are-there*)))

  ;;Reuse filler for blue
  (setf blue-filler-cxn (find-cxn "blue " *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (assert blue-filler-cxn)

  ;;Learn blue slot cxn
  (induce-cxns *is-there-a-blue-metal-sphere* blue-filler-cxn :cxn-inventory *fcg-constructions*)
  ;(comprehend-all (form-string *is-there-a-blue-metal-sphere*))
  (formulate-all (instantiate-variables (meaning *is-there-a-blue-metal-sphere*)))
  
  )
  






;;----------------------------------------------------------------------------------------------------------
;;EXAMPLE 1 OF ANTI-UNIFYING CONSTRUCTIONS (THE CASE OF TWO FILLER CONSTRUCTIONS)
;;----------------------------------------------------------------------------------------------------------
(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      holophrastic-how-many-red-cubes-are-there
      how-many-X-cubes-are-there-cxn
      blue-filler-cxn
      blue-spher-filler-cxn)

  ;;Learn a holophrastic cxn first
  (setf holophrastic-how-many-red-cubes-are-there (induce-cxns *how-many-red-cubes-are-there* nil :cxn-inventory *fcg-constructions*))

  ;;Learn a first slot cxn and one filler cxn for red
  (setf how-many-X-cubes-are-there-cxn
        (induce-cxns *how-many-cubes-are-there* holophrastic-how-many-red-cubes-are-there :cxn-inventory *fcg-constructions*))

   ;;Learn a filler cxn for blue
  (induce-cxns *how-many-blue-cubes-are-there* how-many-X-cubes-are-there-cxn :cxn-inventory *fcg-constructions*)
  (comprehend-all (form-string *how-many-blue-cubes-are-there*))
  (formulate-all (instantiate-variables (meaning *how-many-blue-cubes-are-there*)))
  
  ;;Learn a slot cxn for how-many-Xes-are-there and two filler cxns for blue-spher and cub
  (induce-cxns *how-many-blue-spheres-are-there* how-many-X-cubes-are-there-cxn :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *how-many-blue-spheres-are-there*))
 ; (comprehend-all (form-string *how-many-cubes-are-there*)) ;;WERKT NOG NIET CORRECT (MEANING NIET VERBONDEN)

  ;;Learn a slot-and-filler cxn for blue and filler cxn for spher based on two filler cxns blue-spher-cxn and blue-cxn
  (setf blue-filler-cxn (find-cxn "blue " *fcg-constructions*
                                  :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (assert blue-filler-cxn)
  
  (setf blue-spher-filler-cxn (find-cxn "blue spher" *fcg-constructions*
                                        :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (assert blue-spher-filler-cxn)

  (induce-cxns blue-filler-cxn blue-spher-filler-cxn :cxn-inventory *fcg-constructions*)
  
  (comprehend-all (form-string *how-many-blue-spheres-are-there*))
  (formulate-all (instantiate-variables  (meaning *how-many-blue-spheres-are-there*)))
  )

#|
(setf *res* (anti-unify-sequences
             '((sequence "how many blue spheres are there?" ?l6 ?r6))
             '((sequence "how many " ?l7 ?r7)
               (sequence "_" ?r7 ?l8)
               (sequence "cubes are there?" ?l8 ?r8)
               )))|#


;;++++++++++++++++++++++++++++++++++++++++++++
;; Existing filler-cxn -> slot-cxn
;;++++++++++++++++++++++++++++++++++++++++++++


;;----------------------------------------------------------------------------------------------------------
;;EXAMPLE 2 OF ANTI-UNIFYING CONSTRUCTIONS (THE CASE OF TWO SLOT CXNS)
;;----------------------------------------------------------------------------------------------------------


(defparameter *what-size-is-the-sphere* '((:form . ((sequence "what size is the sphere?" ?l5 ?r5)))
                                        (:meaning . ((get-context ?context-3)
                                                     (filter ?set-3 ?context-3 ?shape-3)
                                                     (bind shape-category ?shape-3 sphere)
                                                     (unique ?object-3 ?set-3)
                                                     (query ?target-3 ?object-3 ?attribute-3)
                                                     (bind attribute-category ?attribute-3 size)))))

(defparameter *what-size-is-the-block* '((:form . ((sequence "what size is the block?" ?l6 ?r6)))
                                         (:meaning . ((get-context ?context-4)
                                                      (filter ?set-4 ?context-4 ?shape-4)
                                                      (bind shape-category ?shape-4 cube)
                                                      (unique ?object-4 ?set-4)
                                                      (query ?target-4 ?object-4 ?attribute-4)
                                                      (bind attribute-category ?attribute-4 size)))))

(defparameter *what-color-is-the-block* '((:form . ((sequence "what color is the block?" ?l7 ?r7)))
                                         (:meaning . ((get-context ?context-5)
                                                      (filter ?set-5 ?context-5 ?shape-5)
                                                      (bind shape-category ?shape-5 cube)
                                                      (unique ?object-5 ?set-5)
                                                      (query ?target-5 ?object-5 ?attribute-5)
                                                      (bind attribute-category ?attribute-5 color)))))

(defparameter *how-many-blocks-are-there* '((:form . ((sequence "how many blocks are there?" ?l41 ?r41)))
                                            (:meaning . ((get-context ?context-61)
                                                         (filter ?set-71 ?context-61 ?shape-61)
                                                         (bind shape-category ?shape-61 cube)
                                                         (count ?number-61 ?set-71)))))


(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      (holophrastic-what-size-is-the-sphere (induce-cxns *what-size-is-the-sphere* nil :cxn-inventory *fcg-constructions*))
      block-cxn what-color-is-the-X-cxn what-size-is-the-X-cxn how-many-Xs-are-there-cxn sphere-cxn)

  ;;leer op basis van holophrase:
  (setf what-size-is-the-X-cxn (induce-cxns *what-size-is-the-block* holophrastic-what-size-is-the-sphere :cxn-inventory *fcg-constructions*))
  ;(comprehend-all (form-string *what-size-is-the-block*))

  (setf block-cxn (find-cxn "block" *fcg-constructions*
                            :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  ;; leer op basis van filler cxn:
  (setf what-color-is-the-X-cxn (induce-cxns *what-color-is-the-block* block-cxn :cxn-inventory *fcg-constructions*))
  (comprehend-all (form-string *what-color-is-the-block*))

   ;; leer op basis van filler cxn:
  (induce-cxns *how-many-blocks-are-there* block-cxn :cxn-inventory *fcg-constructions*)
  (comprehend-all (form-string *how-many-blocks-are-there*))
  (formulate-all (instantiate-variables (meaning *how-many-blocks-are-there*)))


  (induce-cxns what-size-is-the-X-cxn what-color-is-the-X-cxn) 

  (comprehend-all (form-string *what-size-is-the-block*))

  )



;;----------------------------------------------------------------------------------------------------------
;; Experimenten met subwoorden - ANTI-UNIFYING TWO FILLER CONSTRUCTIES (CENTRAAL SLOT)
;;----------------------------------------------------------------------------------------------------------

(defparameter *are-there-any-balls* '((:form . ((sequence "are there any balls?" ?left ?right)))
                                        (:meaning . ((get-context ?source-1)
                                                     (filter ?target-1 ?source-1 ?shape-2)
                                                     (bind shape-category ?shape-2 sphere)
                                                     (exist ?target-2 ?target-1)))))

(defparameter *are-there-any-large-matte-cubes* '((:form . ((sequence "are there any large matte cubes?" ?l80 ?r80)))
                                                  (:meaning . ((get-context ?source-1)
                                                               (filter ?target-1300 ?target-2 ?size-2)
                                                               (bind material-category ?material-2 rubber)
                                                               (filter ?target-1 ?source-1 ?shape-2)
                                                               (bind shape-category ?shape-2 cube)
                                                               (filter ?target-2 ?target-1 ?material-2)
                                                               (bind size-category ?size-2 large)
                                                               (exist ?target-23 ?target-1300)))))

(defparameter *are-there-any-large-shiny-cubes* '((:form . ((sequence "are there any large shiny cubes?" ?l80 ?r80)))
                                                  (:meaning . ((get-context ?source-1)
                                                               (filter ?target-1300 ?target-2 ?size-2)
                                                               (bind material-category ?material-2 metal)
                                                               (filter ?target-1 ?source-1 ?shape-2)
                                                               (bind shape-category ?shape-2 cube)
                                                               (filter ?target-2 ?target-1 ?material-2)
                                                               (bind size-category ?size-2 large)
                                                               (exist ?target-23 ?target-1300)))))

(let ((*fcg-constructions* (make-sandbox-grammar-cxns))
      holophrastic-are-there-any-balls large-matte-cube-cxn large-shiny-cube-cxn)
  
  (setf holophrastic-are-there-any-balls (induce-cxns *are-there-any-balls* nil :cxn-inventory *fcg-constructions*))

  (setf are-there-any-Xes-cxn
        (induce-cxns *are-there-any-large-matte-cubes* holophrastic-are-there-any-balls :cxn-inventory *fcg-constructions*))

  (induce-cxns *are-there-any-large-shiny-cubes* are-there-any-Xes-cxn)

  (comprehend-all (form-string *are-there-any-balls*))
  (comprehend-all (form-string *are-there-any-large-matte-cubes*))
  (comprehend-all (form-string *are-there-any-large-shiny-cubes*))

  (setf large-matte-cube-cxn (find-cxn "rge matte cube" *fcg-constructions*
                            :key #'(lambda (cxn) (second (third (attr-val cxn :sequence)))) :test #'string=))

  (setf large-shiny-cube-cxn (find-cxn "rge shiny cube" *fcg-constructions*
                            :key #'(lambda (cxn) (second (third (attr-val cxn :sequence)))) :test #'string=))

  ;;WERKT NOG NIET!!!
  (when (and large-matte-cube-cxn large-shiny-cube-cxn)
    (induce-cxns large-matte-cube-cxn large-shiny-cube-cxn)
    (comprehend-all (form-string *are-there-any-large-matte-cubes*)))

  )

(add-element  `((p)  "dsqf"))
(add-element  `((p)  "kʌlɹ̩"))
(render-xml `((p)  "kʌlɹ̩"))

(make-empty-cxn-inventory-cxns)
*fcg-constructions*

(defparameter *color*
    (make-instance 'speech-act
                   :form "kʌlɹ̩"
                   :meaning '((bind attribute-category attribute-1 color))))

(comprehend *color* :cxn-inventory *fcg-constructions*)


(defparameter *large*
    (make-instance 'speech-act
                   :form "lɑɹd͡ʒ"
                   :meaning '((bind size-category attribute-1 large))))

(comprehend *large* :cxn-inventory *fcg-constructions*)



(defparameter *cube*
    (make-instance 'speech-act
                   :form "kjub"
                   :meaning '((bind shape-category attribute-1 cube))))

(comprehend *cube* :cxn-inventory *fcg-constructions*)

(defparameter *size*
    (make-instance 'speech-act
                   :form "sajz"
                   :meaning '((bind attribute-category attribute-1  size))))

(comprehend *size* :cxn-inventory *fcg-constructions*)

(defparameter *what-color-is-the-cube*
    (make-instance 'speech-act
                   :form "wʌtkʌlɹ̩ɪzðəkjub?"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 cube)
                              (unique object-1 set-1)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 color))))

(comprehend *what-color-is-the-cube* :cxn-inventory *fcg-constructions*)


(defparameter *what-size-is-the-cube*
  (make-instance 'speech-act
                 :form "wʌtsajzɪzðəkjub?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))

(comprehend *what-size-is-the-cube* :cxn-inventory *fcg-constructions*)

(defparameter *what-size-is-the-large-cube*
  (make-instance 'speech-act
                 :form "wʌtsajzɪzðəlɑɹd͡ʒkjub?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (filter set-3 set-2 size-2)
                            (bind size-category size-2 large)
                            (unique object-2 set-3)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))


(comprehend *what-size-is-the-large-cube* :cxn-inventory *fcg-constructions*)








;;++++++++++++++++++++++++++++++++++++++++++++
;; AMR examples
;;++++++++++++++++++++++++++++++++++++++++++++


(progn
(defparameter *amr-1* `((:form . ((sequence "Hotel rooms available as of this weekend" ?l8 ?r8)))
                        (:meaning . ,(amr:penman->predicates
                                      '(a / available-02
                                          :ARG2 (r / room
                                                   :mod (h / hotel))
                                          :time (a2 / as-of
                                                    :op1 (w / weekend
                                                            :mod (t / this))))
                                      :variablify? t))))

(defparameter *amr-2* `((:form . ((sequence "Hotel rooms available as of tomorrow" ?l9 ?r9)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(a3 / available-02
                                            :ARG2 (r2 / room
                                                      :mod (h2 / hotel))
                                            :time (a4 / as-of
                                                      :op1 (t2 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-3* `((:form . ((sequence "Gonna be winter again tomorrow and Sunday" ?l10 ?r10)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w / winter
                                           :time (a5 / and
                                                    :op1 (t3 / tomorrow)
                                                    :op2 (d / date-entity
                                                            :weekday (s / sunday)))
                                           :mod (a6 / again))
                                       :variablify? t))))

(defparameter *amr-4* `((:form . ((sequence "Gonna be winter again Saturday and Sunday" ?l11 ?r11)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(w2 / winter
                                           :time (a7 / and
                                                    :op1 (d2 / date-entity
                                                            :weekday (s2 / saturday))
                                                    :op2 (d3 / date-entity
                                                            :weekday (s3 / sunday)))
                                           :mod (a7 / again))
                                       :variablify? t))))

(defparameter *amr-5* `((:form . ((sequence "The man will be buried at a funeral service tomorrow" ?l12 ?r12)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b / bury-01
                                           :ARG1 (m / man)
                                           :time (s4 / service-06
                                                    :ARG1 (f / funeral)
                                                    :time (t4 / tomorrow)))
                                       :variablify? t))))

(defparameter *amr-6* `((:form . ((sequence "The man will be buried at a funeral service" ?l13 ?r13)))
                        (:meaning .  ,(amr:penman->predicates 
                                       '(b2 / bury-01
                                           :ARG1 (m2 / man)
                                           :time (s5 / service-06
                                                    :ARG1 (f2 / funeral)))
                                       :variablify? t))))
)

;; Learning 'omorrow-cxn'
(let ((*fcg-constructions*  (make-sandbox-grammar-cxns))
      (holophrastic-amr-cxn-1 (induce-cxns *amr-1* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-2 (induce-cxns *amr-2* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-3 (induce-cxns *amr-3* nil :cxn-inventory *fcg-constructions*))
      (holophrastic-amr-cxn-6 (induce-cxns *amr-6* nil :cxn-inventory *fcg-constructions*))
      (ommorrow-cxn nil))

  (induce-cxns *amr-2* holophrastic-amr-cxn-1 :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *amr-1*))
  (comprehend-all (form-string *amr-2*))

  (setf ommorrow-cxn (find-cxn "omorrow" *fcg-constructions*
                               :key #'(lambda (cxn) (second (first (attr-val cxn :sequence)))) :test #'string=))

  (induce-cxns *amr-3* ommorrow-cxn :cxn-inventory *fcg-constructions*)
  (induce-cxns *amr-4* holophrastic-amr-cxn-3 :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *amr-4*))
  (comprehend-all (form-string *amr-3*))
  
  (induce-cxns *amr-5* holophrastic-amr-cxn-6 :cxn-inventory *fcg-constructions*)

  (comprehend-all (form-string *amr-5*))
  (comprehend-all (form-string *amr-3*)))




;;++++++++++++++++++++++++++++++++++++++++++++
;; Simplified AMR example
;;++++++++++++++++++++++++++++++++++++++++++++

(defparameter *amr-simple-1* `((:form . " local ")
                               (:meaning . ((local-02 ?l-1)))))

(defparameter *amr-simple-2* `((:form . " global ")
                               (:meaning . ((global-02 ?g-1)))))


(progn
  (setf *fcg-constructions*  (make-sandbox-grammar-cxns))
  (defparameter *cxn-1* (induce-cxns *amr-simple-1* nil :cxn-inventory *fcg-constructions*))
  (add-element (make-html *cxn-1*))
  (induce-cxns *amr-simple-2* *cxn-1* :cxn-inventory *fcg-constructions*)
  (comprehend-all (form-string *amr-simple-1*)) ;; here construction doesn't apply
; (comprehend (form-string *amr-simple-2*))
  )





(make-empty-cxn-inventory-cxns)


(defparameter *color*
    (make-instance 'speech-act
                   :form "kʌlɹ̩"
                   :meaning '((bind attribute-category attribute-1 color))))

(comprehend *color* :cxn-inventory *fcg-constructions*)

(defparameter *large*
    (make-instance 'speech-act
                   :form "lɑɹd͡ʒ"
                   :meaning '((bind size-category attribute-1 large))))

(comprehend *large* :cxn-inventory *fcg-constructions*)



(defparameter *cube*
    (make-instance 'speech-act
                   :form "kjub"
                   :meaning '((bind shape-category attribute-1 cube))))

(comprehend *cube* :cxn-inventory *fcg-constructions*)

(defparameter *size*
    (make-instance 'speech-act
                   :form "sajz"
                   :meaning '((bind attribute-category attribute-1  size))))

(comprehend *size* :cxn-inventory *fcg-constructions*)

(defparameter *what-color-is-the-cube*
    (make-instance 'speech-act
                   :form "wʌtkʌlɹ̩ɪzðəkjub?"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 cube)
                              (unique object-1 set-1)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 color))))

(formulate-all (instantiate-variables (comprehend *what-color-is-the-cube* :cxn-inventory *fcg-constructions*)))


pipe-through


(defparameter *what-size-is-the-cube*
  (make-instance 'speech-act
                 :form "wʌtsajzɪzðəkjub?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (unique object-2 set-2)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))

(comprehend *what-size-is-the-cube* :cxn-inventory *fcg-constructions*)

(defparameter *what-size-is-the-large-cube*
  (make-instance 'speech-act
                 :form "wʌtsajzɪzðəlɑɹd͡ʒkjub?"
                 :meaning '((get-context context-2)
                            (filter set-2 context-2 shape-2)
                            (bind shape-category shape-2 cube)
                            (filter set-3 set-2 size-2)
                            (bind size-category size-2 large)
                            (unique object-2 set-3)
                            (query target-2 object-2 attribute-2)
                            (bind attribute-category attribute-2 size))))

(comprehend *what-size-is-the-large-cube* :cxn-inventory *fcg-constructions*)


#|
(def-fcg-constructions test :hashed t :cxn-inventory *A*)
(add-category 'a *A*)
(categories *A*)
(setf *B* (copy-object *A*))
(categories *B*)
(add-category 'b *B*)
(categories *B*)
;; => (UTILS:A UTILS:B)
(categories *A*)
;; => (UTILS:A)
|#


;;########################################################################
;; Stappenplan
;;------------------------------------------

;; Stap 1: 
; voorbeelden -> nadenken is dit de correcte generalisatie gegeven 2 voorbeelden
; -> beginnen van holophrases en meerdere voorbeelden
; exploreren wat er mogelijk is met pure generalisatie 
; zin + constructie



; -> idee strategies






(make-empty-cxn-inventory-cxns)


(defparameter *what-size-is-the-red-cube*
    (make-instance 'speech-act
                   :form "what size is the abc def?"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 cube)
                              (filter set-2 set-1 color-1)
                              (bind color-category color-1 red)
                              (unique object-1 set-2)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 size))))

(defparameter *what-size-is-the-blue-sphere*
    (make-instance 'speech-act
                   :form "what size is the ghi jkl"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 sphere)
                              (filter set-2 set-1 color-1)
                              (bind color-category color-1 blue)
                              (unique object-1 set-2)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 size))))


(defparameter *what-size-is-the-blue-cylinder*
    (make-instance 'speech-act
                   :form "what size is the blue cylinder"
                   :meaning '((get-context context-1)
                              (filter set-1 context-1 shape-1)
                              (bind shape-category shape-1 cylinder)
                              (filter set-2 set-1 color-1)
                              (bind color-category color-1 blue)
                              (unique object-1 set-2)
                              (query target-1 object-1 attribute-1)
                              (bind attribute-category attribute-1 size))))

(comprehend *what-size-is-the-red-cube*)
(comprehend *what-size-is-the-blue-sphere*)

