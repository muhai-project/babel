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

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'clevr-grammar-learning-experiment
                   :entries '((:observation-sample-mode . :random) ;; random or sequential
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:remove-cxn-on-lower-bound . nil)
                              (:learner-th-connected-mode . :neighbours))))) ;; :neighbours or :path-exists

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (get-type-hierarchy (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 50)


;

(formulate '((get-context ?source-1) (query ?target-51 ?target-object-1 ?attribute-15) (bind attribute-category ?attribute-15 material) (filter ?target-2 ?target-1 ?size-2) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-2 cube) (filter ?target-1 ?source-1 ?shape-2) (bind size-category ?size-2 small)) :gold-standard-utterance "What is the small cube made of?" :cxn-inventory (grammar (first (interacting-agents *experiment*))))


#|
NOTES
------
OVERAL ORIGINAL behalve in FCG apply!

meerdere runs over geshufflede data met behouden grammatica mag geen verschil geven!

 
ISSUES
------
-niets weggooien bij lateral inhibition
-en bij score berekenen de 0 scores niet meetellen
-bidirectional (formulation en comprehension)
met en zonder lateral inhibition
repair evo plot
cxn type evo plot
num type hierarchy links / comm success


TODO
----
- testcase per repair
- constructiesoortmonitor invoegen, zie code jens
- repair monitor (zie Jens): welke repair heeft toegepast in een interactie?

- item based based repairs updaten en terug invoegen
- logica in lexical to item-based nakijken, dubbels gewoon skippen uit veiligheid, zie diff-non-overlapping-meaning functie in utils
- constructiesoortmonitor invoegen: 
- check handle fix! fix cxns en th-links moeten doorgegeven worden
- series run duration
|#





#| ABSTRACT CXNS:
PISTE 1
-------
als er twee opeenvolgende slots zijn, maak er een slot van, bijvoorbeeld:
Is there a X? "is there a cube?"
Is there a X Y? "is there a large cube?"
--> Is there a X? + X--> Y Z cxn "large cube" (determined noun phrase)

PISTE 2
-------
laat ook langere chunks toe bij diffs, niet enkel single lex items!
bijv: how many large cubes are there? vs how many small spheres are there?
==> large cubes cxn
==> small spheres cxn
==> how many x are there?

dan als we large of small leren krijgen we X cubes, x spheres, en X Y.

==> zo zou je ook X or Y kunnen leren!
|#
