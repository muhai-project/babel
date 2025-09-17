(ql:quickload :slp)
(in-package :slp)

;;;;;;;;;;;;;;;;;;;;;
;; CSS definitions ;;
;;;;;;;;;;;;;;;;;;;;;

(define-css 'demo-header "
.demo-header {
    background-color: #002333;
    padding: 30px 20px 30px 20px;
}")

(define-css 'footer "
.footer {
    background-color: #002333;
    height: 60px;
    margin: 0px;
}")

(define-css 'demo-title "
.demo-title {
    font-size: 40px;
    color: white;
    margin: 0px;
    padding: 0px;
}")

(define-css 'sub-title "
.sub-title {
    font-size:20px;
    color: white;
    margin: 20px 0px 0px 0px;
}")

(define-css 'italic-text "
.italic-text {
    font-style: italic;
}")


(define-css 'bold-text "
.bold-text {
    font-weight: bold;
}")

(define-css 'main-text "
.main-text {
    margin:20px;
    font-size:20px;
}")

(define-css 'flex-div "
.flex-div {
    display:flex;
    justify-content: flex-start;
    margin: 20px;
}")

(define-css 'fcg-element "
.fcg-element {
    margin:20px;
}")

(define-css 'section-header "
.section-header {
    font-size: 25px;
    padding: 20px;
    margin: 0px;
    background-color: #d1d1d1
}")

(define-css 'sub-section-header "
.sub-section-header {
    font-size: 20px;
    margin: 20px;
}")


;;;;;;;;;;;;;;;
;; demo data ;;
;;;;;;;;;;;;;;;

(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

(defparameter *test-utterance-xml-1*
  (read-xml
  (merge-pathnames
   (make-pathname :directory '(:relative "elan-files")
                  :name "1_0_1" :type "eaf")
   *geoquery-lsfb-data*)))

(setf web-interface::*dispatch-table*
          (append web-interface::*dispatch-table*
                  (list (web-interface::create-static-file-dispatcher-and-handler   
                         "/1_0_1.mp4" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/videos/1_0_1.mp4"))))

(defparameter *test-utterance-1-predicates*
 (elan->predicates *test-utterance-xml-1*))

(defparameter *resulting-meaning* nil)
(defparameter *comprehension-cip*
  (multiple-value-bind (result cipn)
    (comprehend *test-utterance-1-predicates*)
    (setf *resulting-meaning* result)
    (cip cipn)))

(defparameter *comprehension-resulting-structure*
  (car-resulting-cfs (cipn-car (first (succeeded-nodes *comprehension-cip*)))))

(defparameter *haut-cxn*
  (loop for construction in (constructions *fcg-constructions*)
        when (eql (name construction) 'haut-cxn)
          return construction))

(defparameter *meaning-network*
  '((ANSWER ?D ?A ?E) (HIGH_POINT ?E ?B ?A) (STATE ?E ?B) (NEXT_TO ?E ?B ?C) (CONST ?E ?C ?F) (STATEID ?F ?G) (MISSISSIPPI ?G)))

(defparameter *resulting-form* nil)

(defparameter *production-cip*
  (multiple-value-bind (result cipn)
    (formulate *meaning-network*)
    (setf *resulting-form* result)
    (cip cipn)))

(defparameter *production-resulting-structure*
  (car-resulting-cfs (cipn-car (first (succeeded-nodes *production-cip*)))))

;;;;;;;;;;;;;;;;;;;
;; demo sections ;;
;;;;;;;;;;;;;;;;;;;

(defun header ()
  (add-element
   `((div :class "demo-header")
     ((h1 :class "demo-title")
     "A Computational Construction Grammar Framework for Modelling Signed Languages")
     ((p :class "sub-title")
      "This demonstration accompanies the paper"
      ((span :class "italic-text")
       "\"A Computational Construction Grammar Framework for Modelling Signed Languages\" ")
      "and demonstrates how multilinear signed expressions can be represented and processed bidirectionally using the Fluid Construction Grammar framework. It contains many interactive elements that can be expanded by clicking on them."))))

(defun intro ()
  (add-element
   `((p :class "main-text")
     "We focus on an example expression from French Belgian Sign Language (LSFB), translated as:"
     ((span :class "italic-text")
      "\"What are the high points of states surrounding Mississippi ?\"")":"))
  (add-element
   `((div :class "flex-div")
     ((video :controls "" :height "20%" :width "20%" :muted "")
      ((source :src "http://localhost:8000/1_0_1.mp4")))
      ,(make-html *test-utterance-1-predicates* :avatar nil))))
 

(defun construction-inventory-section ()
  (add-element
   `((h2 :class "section-header")
     "1. The construction inventory"))
  (add-element
   `((p :class "main-text" )
     ((span :class "bold-text") "Comprehending and producing signed expressions with FCG relies on an inventory of constructions.") "The diagram below shows an example construction for the LSFB sign HAUT (HIGH).  This construction maps the linguistic form of the right-handed sign HAUT (represented in HamNoSys as \"\") to a procedural meaning predicate that refers to a high point. When applied, the construction adds additional linguistic information to the intermediate analysis, such as the number and location of the sign."))
  (add-element
   `((div :class "fcg-element")
     ,(make-html *haut-cxn*
                 :expand-initially t
                 )))
  (add-element
   `((p :class "main-text")
     "For this demonstration, we provide a minimal collection of sign language constructions:"))
  (add-element
   `((div :class "fcg-element")
     ,(make-html *fcg-constructions*
                 :expand-initially t))))

;(construction-inventory-section)
(defun comprehension ()
  (add-element
   `((h2 :class "section-header")
     "2. Comprehension: mapping form to meaning"))
  (add-element
   `((h3 :class "sub-section-header")
     "Graphical representation of input form"))
  (add-element
   `((p :class "main-text" )
     "At the start of comprehension, each input form is represented graphically. To make the HamNoSys strings within this representation more accessible, users can animate signs by clicking on the play button located next to or below each sign's unique identifier: "))
  (add-element
   `((div :class "fcg-element")
     ,(make-html *test-utterance-1-predicates*)))
  (add-element
   `((h3 :class "sub-section-header")
     "Initial structure"))
  (add-element
   `((p :class "main-text" )
     "The graphical representation of the input form is transformed into the notation format discussed in the paper. Each sign is modeled using a predicate that describes its type, unique identifier, and HamNoSys structure. This predicate representation is added to the initial state of the construction application process, also known as the initial transient structure:"))
  (add-element
   `((div :class "fcg-element")
      ,(make-html-fcg-light 
        (initial-cfs *comprehension-cip*)
        :configuration
        (visualization-configuration
         (construction-inventory *comprehension-cip*))
        :feature-types
        (feature-types
         (original-cxn-set
          (construction-inventory *comprehension-cip*)))
        :expand-initially t)))
  (add-element
   `((h3 :class "sub-section-header")
     "Construction application process"))
  (add-element
   `((p :class "main-text" )
     "Constructions apply to this initial state, expanding it incrementally to reach a complete meaning analysis:"))
  (add-element
   `((div :class "fcg-element")
   ,(make-html-fcg-light
    (top-node *comprehension-cip*)
    :subtree-id (mkstr (make-id 'subtree-id))
    :struct-id (make-id 'info-struct)
    )))
  (add-element
   `((h3 :class "sub-section-header")
     "Resulting structure"))
  (add-element
   `((p :class "main-text" )
     "The dark green node represents the goal state of the comprehension process. Its transient structure is an expanded version of the initial one, containing a collection of linguistic units:"))
  (add-element
   `((div :class "fcg-element")
   ,(make-html-fcg-light
     *comprehension-resulting-structure*
        :configuration
        (visualization-configuration
         (construction-inventory *comprehension-cip*))
        :feature-types
        (feature-types
         (original-cxn-set
          (construction-inventory *comprehension-cip*))))))
  (add-element
   `((h3 :class "sub-section-header")
     "Resulting meaning network"))
  (add-element
   `((p :class "main-text" )
     "Finally, FCG extracts all meaning predicates from the resulting structure, retrieving the meaning of the observed form: "))
  (add-element
   `((div :class "fcg-element")
     ,(predicate-network->svg
       *resulting-meaning*
       :only-variables nil))))

(defun production ()
  (add-element
   `((h2 :class "section-header")
     "2. Production: mapping meaning to form"))
  (add-element
   `((h3 :class "sub-section-header")
     "Initial meaning representation"))
  (add-element
   `((p :class "main-text" )
     "In production, the input to the construction application process is a meaning network. For our example, the meaning is defined as a procedural semantic representation: "))
  (add-element
   `((div :class "fcg-element")
     ,(predicate-network->svg
       *meaning-network*
       :only-variables nil)))
  (add-element
   `((h3 :class "sub-section-header")
     "Initial structure"))
  (add-element
   `((p :class "main-text" )
     "The meaning network is represented as a set of predicates within the initial transient structure:"))
  (add-element
   `((div :class "fcg-element")
      ,(make-html-fcg-light 
        (initial-cfs *production-cip*)
        :configuration
        (visualization-configuration
         (construction-inventory *comprehension-cip*))
        :feature-types
        (feature-types
         (original-cxn-set
          (construction-inventory *comprehension-cip*)))
        :expand-initially t)))
  (add-element
   `((h3 :class "sub-section-header")
     "Construction application process"))
  (add-element
   `((p :class "main-text")
     "Constructions apply to this initial structure, expanding it incrementally to reach a complete form analysis:"))
  (add-element
   `((div :class "fcg-element")
   ,(make-html-fcg-light
    (top-node *production-cip*)
    :subtree-id (mkstr (make-id 'subtree-id))
    :struct-id (make-id 'info-struct))))
  (add-element
   `((h3 :class "sub-section-header")
     "Resulting structure"))
  (add-element
   `((p :class "main-text" )
     "Similarly to earlier, the dark green node represents the goal state, which contains a collection of linguistic units:"))
  (add-element
   `((div :class "fcg-element")
   ,(make-html-fcg-light
     *comprehension-resulting-structure*
        :configuration
        (visualization-configuration (construction-inventory *comprehension-cip*))
        :feature-types (feature-types (original-cxn-set (construction-inventory *comprehension-cip*))))))
  (add-element
   `((h3 :class "sub-section-header")
     "Resulting form"))
  (add-element
   `((p :class "main-text" )
     "Finally, FCG extracts all form predicates from the resulting structure, retrieving a form which expresses the input meaning: "))
  (add-element
   `((div :class "fcg-element")
     ,(make-html *resulting-form*)))
  )

(defun footer ()
   (add-element
    `((div :class "footer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating the full demo ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-demo ()
  (header)
  (intro)
  (construction-inventory-section)
  (comprehension)
  (production)
  (footer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making a static web demo ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-demo () 
       (web-interface:create-static-html-page "sign-language-processing" (full-demo)))



(make-demo)
;(full-demo)

