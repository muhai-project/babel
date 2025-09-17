
;;###########################################################################;;
;;                                                                           ;;
;; Web demo file for the Dutch VP paper (Constructions and Frames)           ;;
;;                                                                           ;;
;;###########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :casa)

(in-package :fcg)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Operationalising CASA with Fluid Construction Grammar"))
  (add-element '((h3) "Katrien Beuls"))
  (add-element '((p) "This web demo implements a selection of constructional analyses taken from the following book:"))
  (add-element '((p) "Herbst, T. and Hoffmann, T. (2024). A Construction Grammar of the English Language. CASA - A Constructionist Approach to Syntactic Analysis."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "1. Introduction")))
  (add-element '((p)  ((a :href "#WD-2") "2. The Construction Inventory")))
  (add-element '((p)  ((a :href "#WD-3") "3. An Example (Comprehension)")))
  (add-element '((p)  ((a :href "#WD-4") "4. An Example (Formulation)")))
  (add-element '((p)  ((a :href "#WD-5") "5. An Example (Robust Comprehension)")))
  (add-element '((hr))))

;; (header-page)

(defun introduction ()
  (add-element '((p) ((a :name "WD-1") "")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration demonstrates a possible implementation of a number of CASA analyses as described by Herbst and Hoffmann (2024). For more information about the FCG formalism, notation and processing mechanisms, we refer the reader to the paper <i>Fluid Construction Grammar: State of the Art and Future Outlook</i> (Beuls and Van Eecke, 2023)."))
  (add-element '((hr))))

;; (introduction)

(defun grammar ()
  (add-element '((p) ((a :name "WD-2") "")))
  (add-element '((h2) "2. The Construction Inventory"))
  (add-element '((p) "The construction inventory is shown below. You can click on the blue boxes and then on the encircled + sign to expand the constructions. The construction inventory contains the morphological and lexical constructions for 2 main verbs ('zingen' (to sing) and 'gaan' (to go)), the morphological and lexical constructions for the perfect and modal auxiliaries, the verb phrase construction, the modality constructions, the perfect-constructions and the tense constructions. Morphological and lexical constructions for other main verbs are not shown here."))
  (add-element (make-html *fcg-constructions* :expand-initially t))
  (add-element '((hr))))

;; (grammar)

(defun comprehension-example ()
  (add-element '((p) ((a :name "WD-3") "")))
  (add-element '((h2) "3. An Example (Comprehension)"))
  (add-element '((p) "The example for comprehension that is discussed in the paper is shown here in full detail. It consists of comprehending the utterance 'would you like tea or coffee?'. Click on the green boxes (twice for more detail) to zoom in on an individual step in the construction application process."))
  (comprehend  "would you like tea or coffee?")
  (add-element '((hr))))


(create-static-html-page "Operationalising CASA with Fluid Construction Grammar"
(progn
  (header-page)
  (introduction)
  (grammar)
  (comprehension-example)
 ; (formulation-example)
;  (robust-parsing))
))
