(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; minimal logging after 100 interactions with type hierarchy
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; minimal logging after 100 interactions
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (defparameter *test-setup*
    (make-instance 'clevr-grammar-learning-experiment
                   :entries '((:observation-sample-mode . :random) ;; random or sequential
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:remove-cxn-on-lower-bound . t)
                              (:learner-th-connected-mode . :neighbours)))))

(defparameter *grammar* (grammar (first (agents *test-setup*))))
;;(activate-monitor fcg::trace-fcg-search-process)


(defun test-repair-holophrase-single-addition-left-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  

    ;; add an initial holophrase repair for the cube

  (comprehend "red cubes" :gold-standard-meaning '((get-context ?source)
                                                   (filter ?cubes ?source ?cube)
                                                   (filter ?red-cubes ?cubes ?red)
                                                   (bind shape-category ?cube cube)
                                                   (bind color-category ?red red)) :cxn-inventory *grammar*)
    ;; test the repair for the red cube
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube)
                                                       (filter ?red-cubes ?cubes ?red)
                                                       (filter ?large-red-cubes ?red-cubes ?large)
                                                       (bind size-category ?large large)
                                                       (bind shape-category ?cube cube)
                                                       (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-double-addition-center-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  ;; add an initial holophrase repair for the cube

  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair, which should generate an item-based + lex cxn

  (comprehend "the large red cube" :gold-standard-meaning '((get-context ?source)
                                                            (filter ?cubes ?source ?cube) 
                                                            (filter ?red-cubes ?cubes ?red)
                                                            (filter ?large-red-cubes ?red-cubes ?large)
                                                            (unique ?the-red-cube ?large-red-cubes)
                                                            (bind size-category ?large large)
                                                            (bind shape-category ?cube cube) 
                                                            (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-single-addition-center-existing-lex-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube)
                                                         (filter ?red-cubes ?cubes ?red)
                                                         (filter ?large-red-cubes ?red-cubes ?large)
                                                         (bind size-category ?large large)
                                                         (bind shape-category ?cube cube)
                                                         (bind color-category ?red red)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "red cubes" :gold-standard-meaning '((get-context ?source)
                                                   (filter ?cubes ?source ?cube)
                                                   (filter ?red-cubes ?cubes ?red)
                                                   (bind shape-category ?cube cube)
                                                   (bind color-category ?red red)) :cxn-inventory *grammar*)

  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair, which should generate an item-based + lex cxn

  (comprehend "the large cube" :gold-standard-meaning '((get-context ?source)
                                                        (filter ?cubes ?source ?cube) 
                                                        (filter ?large-cubes ?cubes ?large)
                                                        (unique ?the-large-cube ?large-cubes)
                                                        (bind shape-category ?cube cube) 
                                                        (bind size-category ?large large)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-holophrase-single-addition-center-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair, which should generate an item-based + lex cxn

  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-holophrase-single-addition-right-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  ;; add an initial holophrase repair for the cube

  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair, which should generate an item-based + lex cxn

  (comprehend "the cube red" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-double-deletion-center-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube
  (comprehend "the large red cube" :gold-standard-meaning '((get-context ?source)
                                                            (filter ?cubes ?source ?cube) 
                                                            (filter ?red-cubes ?cubes ?red)
                                                            (filter ?large-red-cubes ?red-cubes ?large)
                                                            (unique ?the-red-cube ?large-red-cubes)
                                                            (bind size-category ?large large)
                                                            (bind shape-category ?cube cube) 
                                                            (bind color-category ?red red)) :cxn-inventory *grammar*)
                (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*))

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)
  (comprehend "the large red cube" :gold-standard-meaning '((get-context ?source)
                                                            (filter ?cubes ?source ?cube) 
                                                            (filter ?red-cubes ?cubes ?red)
                                                            (filter ?large-red-cubes ?red-cubes ?large)
                                                            (unique ?the-red-cube ?large-red-cubes)
                                                            (bind size-category ?large large)
                                                            (bind shape-category ?cube cube) 
                                                            (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-holophrase-single-deletion-center-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube
  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)))
  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-holophrase-single-deletion-center-existing-lex-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube)
                                                         (filter ?red-cubes ?cubes ?red)
                                                         (filter ?large-red-cubes ?red-cubes ?large)
                                                         (bind size-category ?large large)
                                                         (bind shape-category ?cube cube)
                                                         (bind color-category ?red red)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "red cubes" :gold-standard-meaning '((get-context ?source)
                                                   (filter ?cubes ?source ?cube)
                                                   (filter ?red-cubes ?cubes ?red)
                                                   (bind shape-category ?cube cube)
                                                   (bind color-category ?red red)) :cxn-inventory *grammar*)


  (comprehend "the large cube" :gold-standard-meaning '((get-context ?source)
                                                        (filter ?cubes ?source ?cube) 
                                                        (filter ?large-cubes ?cubes ?large)
                                                        (unique ?the-large-cube ?large-cubes)
                                                        (bind shape-category ?cube cube) 
                                                        (bind size-category ?large large)) :cxn-inventory *grammar*)
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-single-deletion-right-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube
  (comprehend "the cube red" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)))
  (comprehend "the cube red" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-single-deletion-left-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube)
                                                       (filter ?red-cubes ?cubes ?red)
                                                       (filter ?large-red-cubes ?red-cubes ?large)
                                                       (bind size-category ?large large)
                                                       (bind shape-category ?cube cube)
                                                       (bind color-category ?red red)) :cxn-inventory *grammar*)
                (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*))

  ;; try the new repair-holophrase-single-deletion repair.
  (comprehend "red cubes" :gold-standard-meaning '((get-context ?source)
                                                   (filter ?cubes ?source ?cube)
                                                   (filter ?red-cubes ?cubes ?red)
                                                   (bind shape-category ?cube cube)
                                                   (bind color-category ?red red)) :cxn-inventory *grammar*)
    ;; test the repair for the red cube
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube)
                                                       (filter ?red-cubes ?cubes ?red)
                                                       (filter ?large-red-cubes ?red-cubes ?large)
                                                       (bind size-category ?large large)
                                                       (bind shape-category ?cube cube)
                                                       (bind color-category ?red red)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-single-deletion-center-formulation ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube
  (formulate '((get-context ?source)
               (filter ?cubes ?source ?cube)
               (filter ?red-cubes ?cubes ?red)
               (unique ?the-red-cube ?red-cubes)
               (bind shape-category ?cube cube)
               (bind color-category ?red red))
             :gold-standard-utterance "the red cube" :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (formulate '((get-context ?source)
               (filter ?cubes ?source ?cube) 
               (unique ?the-cube ?cubes) 
               (bind shape-category ?cube cube))
             :gold-standard-utterance "the cube" :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-repair-holophrase-single-deletion-left-formulation ()
  (set-up-cxn-inventory-and-repairs)
  
  ;; add an initial holophrase repair for the red cube
  (formulate '((get-context ?source)
               (filter ?cubes ?source ?cube)
               (filter ?red-cubes ?cubes ?red)
               (count 2 ?red-cubes)
               (bind shape-category ?cube cube)
               (bind color-category ?red red))
             :gold-standard-utterance "two red cubes" :cxn-inventory *grammar*)

  ;; try the new repair-holophrase-single-deletion repair.
  (formulate '((get-context ?source)
               (filter ?cubes ?source ?cube)
               (filter ?red-cubes ?cubes ?red)
               (bind shape-category ?cube cube)
               (bind color-category ?red red)) :gold-standard-utterance "red cubes" :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-add-item-based-cxn-existing-lex ()
  (set-up-cxn-inventory-and-repairs)

  (comprehend "small red spheres" :gold-standard-meaning '((get-context ?source)
                                                           (filter ?spheres ?source ?sphere)
                                                           (filter ?red-spheres ?spheres ?red)
                                                           (filter ?small-red-spheres ?red-spheres ?small)
                                                           (bind size-category ?small small)
                                                           (bind shape-category ?sphere sphere)
                                                           (bind color-category ?red red)) :cxn-inventory *grammar*)
  (comprehend "large red cubes" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube)
                                                         (filter ?red-cubes ?cubes ?red)
                                                         (filter ?large-red-cubes ?red-cubes ?large)
                                                         (bind size-category ?large large)
                                                         (bind shape-category ?cube cube)
                                                         (bind color-category ?red red)) :cxn-inventory *grammar*)
  (comprehend "large blue cubes" :gold-standard-meaning '((get-context ?source)
                                                          (filter ?cubes ?source ?cube)
                                                          (filter ?blue-cubes ?cubes ?blue)
                                                          (filter ?large-blue-cubes ?blue-cubes ?large)
                                                          (bind size-category ?large large)
                                                          (bind shape-category ?cube cube)
                                                          (bind color-category ?blue blue)) :cxn-inventory *grammar*)
  (comprehend "small blue spheres" :gold-standard-meaning '((get-context ?source)
                                                            (filter ?spheres ?source ?sphere)
                                                            (filter ?blue-spheres ?spheres ?blue)
                                                            (filter ?small-blue-spheres ?blue-spheres ?small)
                                                            (bind size-category ?small small)
                                                            (bind shape-category ?sphere sphere)
                                                            (bind color-category ?blue blue)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))
  
(defun test-repair-add-lexical-cxn-center-comprehension ()
  (set-up-cxn-inventory-and-repairs)

  ;; add an initial holophrase repair for the red cube

  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
  ;; create an item-based + 2 lex for blue and red
  (comprehend "the blue cube" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube) 
                                                       (filter ?blue-cubes ?cubes ?blue)
                                                       (unique ?the-blue-cube ?blue-cubes)
                                                       (bind shape-category ?cube cube) 
                                                       (bind color-category ?blue blue)) :cxn-inventory *grammar*)

  ;; create a single lexical cxn for green
   (comprehend "the green cube" :gold-standard-meaning '((get-context ?source)
                                                        (filter ?cubes ?source ?cube) 
                                                        (filter ?green-cubes ?cubes ?green)
                                                        (unique ?the-green-cube ?green-cubes)
                                                        (bind shape-category ?cube cube) 
                                                        (bind color-category ?green green)) :cxn-inventory *grammar*)
     (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-repair-lexical->item-based-cxn-comprehension ()
  (set-up-cxn-inventory-and-repairs)
  
  (comprehend "the metal cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?metal-cubes ?cubes ?metal)
                                                      (unique ?the-metal-cube ?metal-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind attribute-category ?metal metal)) :cxn-inventory *grammar*)
  
  (comprehend "the metal sphere" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?spheres ?source ?sphere) 
                                                      (filter ?metal-spheres ?spheres ?metal)
                                                      (unique ?the-metal-sphere ?metal-spheres)
                                                      (bind shape-category ?sphere sphere) 
                                                      (bind attribute-category ?metal metal)) :cxn-inventory *grammar*)
  
  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                  (filter ?cubes ?source ?cube) 
                                                  (unique ?the-cube ?cubes)
                                                  (bind shape-category ?cube cube)) :cxn-inventory *grammar*)

    
  (comprehend "what is the color of the metal sphere"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute color)
                                     (bind shape-category ?sphere sphere)
                                     (bind attribute-category ?metal metal)
                                     (unique ?object ?metal-spheres)
                                     (filter ?spheres ?source ?sphere)
                                     (filter ?metal-spheres ?spheres ?metal)
                                     (query ?response ?object ?attribute)) :cxn-inventory *grammar*)
    (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))
  
(defun test-th-links-comprehension ()  
(set-up-cxn-inventory-and-repairs)
  ;; add an initial holophrase repair for the red cube

  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
  ;; create an item-based + 2 lex for blue and red
  (comprehend "the blue cube" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube) 
                                                       (filter ?blue-cubes ?cubes ?blue)
                                                       (unique ?the-blue-cube ?blue-cubes)
                                                       (bind shape-category ?cube cube) 
                                                       (bind color-category ?blue blue)) :cxn-inventory *grammar*)

  ;; create a single lexical cxn for green
  (comprehend "the yellow cube" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube) 
                                                         (filter ?yellow-cubes ?cubes ?yellow)
                                                         (unique ?the-yellow-cube ?yellow-cubes)
                                                         (bind shape-category ?cube cube) 
                                                         (bind color-category ?yellow yellow)) :cxn-inventory *grammar*)
  (comprehend "the green sphere" :gold-standard-meaning
              '((get-context ?source)
                (filter ?spheres ?source ?sphere)
                (filter ?green-spheres ?spheres ?green)
                (unique ?the-green-sphere ?green-spheres)
                (bind shape-category ?sphere sphere)
                (bind color-category ?green green)) :cxn-inventory *grammar*)

  (comprehend "the black sphere" :gold-standard-meaning
              '((get-context ?source)
                (filter ?spheres ?source ?sphere)
                (filter ?black-spheres ?spheres ?black)
                (unique ?the-black-sphere ?black-spheres)
                (bind shape-category ?sphere sphere)
                (bind color-category ?black black)) :cxn-inventory *grammar*)

  (comprehend "the black cube" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube) 
                                                         (filter ?black-cubes ?cubes ?black)
                                                         (unique ?the-black-cube ?black-cubes)
                                                         (bind shape-category ?cube cube) 
                                                         (bind color-category ?black black)) :cxn-inventory *grammar*)

  (comprehend "the yellow sphere" :gold-standard-meaning
              '((get-context ?source)
                (filter ?spheres ?source ?sphere)
                (filter ?yellow-spheres ?spheres ?yellow)
                (unique ?the-yellow-sphere ?yellow-spheres)
                (bind shape-category ?sphere sphere)
                (bind color-category ?yellow yellow)) :cxn-inventory *grammar*)
   
  (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))


(defun test-item-based+item-based->item-based-cxn ()  
  (set-up-cxn-inventory-and-repairs)
  ;; add an initial holophrase repair for the red cube

  (comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                      (filter ?cubes ?source ?cube) 
                                                      (filter ?red-cubes ?cubes ?red)
                                                      (unique ?the-red-cube ?red-cubes)
                                                      (bind shape-category ?cube cube) 
                                                      (bind color-category ?red red)) :cxn-inventory *grammar*)
  ;; create an item-based + 2 lex for blue and red
  (comprehend "the blue cube" :gold-standard-meaning '((get-context ?source)
                                                       (filter ?cubes ?source ?cube) 
                                                       (filter ?blue-cubes ?cubes ?blue)
                                                       (unique ?the-blue-cube ?blue-cubes)
                                                       (bind shape-category ?cube cube) 
                                                       (bind color-category ?blue blue)) :cxn-inventory *grammar*)

  ;; create a single lexical cxn for green
  (comprehend "the yellow cube" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube) 
                                                         (filter ?yellow-cubes ?cubes ?yellow)
                                                         (unique ?the-yellow-cube ?yellow-cubes)
                                                         (bind shape-category ?cube cube) 
                                                         (bind color-category ?yellow yellow)) :cxn-inventory *grammar*)
  (comprehend "the blue sphere" :gold-standard-meaning
              '((get-context ?source)
                (filter ?spheres ?source ?sphere)
                (filter ?blue-spheres ?spheres ?blue)
                (unique ?the-blue-sphere ?blue-spheres)
                (bind shape-category ?sphere sphere)
                (bind color-category ?blue blue)) :cxn-inventory *grammar*)
   
  (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))

(defun test-item-based+item-based->item-based-addition ()  
  (set-up-cxn-inventory-and-repairs)
  ;; add an initial holophrase repair for the red cube

  (comprehend "the cylinder" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cylinders ?source ?cylinder) 
                                                         (unique ?the-cylinder ?cylinders)
                                                         (bind shape-category ?cylinder cylinder)) :cxn-inventory *grammar*)

  (comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                         (filter ?cubes ?source ?cube) 
                                                         (unique ?the-cube ?cubes)
                                                         (bind shape-category ?cube cube)) :cxn-inventory *grammar*)
                                                         
  (comprehend "the blue cube" :gold-standard-meaning
              '((get-context ?source)
                (filter ?cubes ?source ?cube)
                (filter ?blue-cubes ?cubes ?blue)
                (unique ?the-blue-cube ?blue-cubes)
                (bind shape-category ?cube cube)
                (bind color-category ?blue blue)) :cxn-inventory *grammar*)
   
  (wi:add-element (make-html (get-type-hierarchy
                              *grammar*) :weights? t :colored-edges-0-1 t))
  (wi:add-element (make-html *grammar*)))




(progn
  (test-repair-holophrase-single-addition-left-comprehension))





;(activate-monitor trace-fcg)
;; (deactivate-monitor trace-fcg)
;;;;;;;;;;;;;;;;;;; ;; comprehension ;; ;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A. starting from scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; ;; 1. addition ;; ;;;;;;;;;;;;;;;;;

;; 1.1 0 -> holophrase (the red cube) ;; --> the red cube

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; B. starting from a holophrase cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;; ;; 1. deletion ;; ;;;;;;;;;;;;;;;;;

;; 1.1 holophrase (the red cube) --> holophrase (the cube) + item-based (the-x-cube) + lexical (red)
;; the red cube --> the cube
;;
;; (test-repair-holophrase-single-deletion-center-comprehension)

;; the large red cube --> the cube
;;(test-repair-holophrase-double-deletion-center-comprehension)

;; the large cube + large --> the cube
;; (test-repair-holophrase-single-deletion-center-existing-lex-comprehension)

;; large red cubes --> red cubes
;;(test-repair-holophrase-single-deletion-left-comprehension)

;; the cube red --> the cube
;;(test-repair-holophrase-single-deletion-right-comprehension)

;; 1.2 holophrase (the red cube) + lex (red) --> item-based (the-x-cube) + holophrase (the cube)
;; the red cube --> the cube

;;;;;;;;;;;;;;;;;
;; 2. addition ;;
;;;;;;;;;;;;;;;;;

;; 2.1 holophrase (the cube) --> item-based (the-x-cube) + lexical (red)

;; the cube --> the red cube
;; (test-repair-holophrase-single-addition-left-comprehension)

;; the cube + large --> the large cube
;;(test-repair-holophrase-single-addition-center-existing-lex-comprehension)


;; the cube -> the large red cube
;;(test-repair-holophrase-double-addition-center-comprehension)

;; red cubes --> large red cubes
;;(test-repair-holophrase-single-addition-center-comprehension)

;; the cube --> the cube red
;;(test-repair-holophrase-single-addition-right-comprehension)

;; 2.2 holophrase (the cube) + lex (red) --> item-based (the-x-cube)
;; the cube --> the red cube

;;;;;;;;;;;;;;;;;;;;; ;; 3. substitution ;; ;;;;;;;;;;;;;;;;;;;;;


;; 3.1 holophrase (the red cube) --> item-based (the-x-cube) + lexical
;; (red) + lexical (green)
;; large red cube --> large green cube
;; large red cube --> small red cube
;; large red cube --> large red square

;; 3.2 holophrase (small red sphere) + lexical (blue) --> item-based (small-x-sphere)

;; small red spheres (holophrase)
;; large red cubes (holophrase)
;; large blue cubes lexical (blue) + item-based (large-x-cube) > substitution repair 3.1
;; observation: small blue spheres --> small-x-sphere
;; (test-repair-add-item-based-cxn-existing-lex)

;; 3.3 item-based (the-x-cube) --> lexical (blue)
;; the red cube --> holophrase (the red cube)
;; the blue cube --> item-based (the-x-cube) + lex (red) + lex (blue)
;; the green cube --> lexical (green)

;; (test-repair-add-lexical-cxn-center-comprehension)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C. starting from a lexical cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; already learned: colour (lex) large (lex) metal (lex) sphere (lex)
;; what is the colour of the large metal sphere? -> what is the x of the y z a? (item-based)

;; (test-repair-lexical->item-based-cxn-comprehension)


;;;;;;;;;;;;;;;;;
;; DEBUG CASES ;;
;;;;;;;;;;;;;;;;;

;;(test-th-links-comprehension)
;;(test-item-based+item-based->item-based-cxn)
;;(test-item-based+item-based->item-based-addition)

