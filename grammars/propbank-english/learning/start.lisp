;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
(load-pb-data :store-data nil :ignore-stored-data nil)
;(length *pb-data*)

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data nil :ignore-stored-data nil)
;(length (train-split *propbank-annotations*))

;; Activating spacy-api locally
(setf nlp-tools::*penelope-host* "http://localhost:5000")

;; Activating trace-fcg
(activate-monitor trace-fcg)

;;Create an empty cxn inventory
(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation) ;;:de-render-constituents-dependents-without-tokenisation
                       (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
                       (:parse-goal-tests :gold-standard-meaning) ;:no-valid-children
                       (:max-nr-of-nodes . 100)
                       (:parse-order multi-argument-with-lemma multi-argument-without-lemma single-argument-with-lemma single-argument-without-lemma)
                       (:node-expansion-mode . :multiple-cxns)
                       (:priority-mode . :nr-of-applied-cxns)
                       (:queue-mode . :greedy-best-first)
                       (:hash-mode . :hash-lemma)
                       (:cxn-supplier-mode . :hashed-scored-labeled)
                       (:equivalent-cxn-fn . fcg::equivalent-propbank-construction)
                       (:equivalent-cxn-key . identity)
                       (:learning-mode :multi-argument-with-lemma :multi-argument-without-lemma
                       ; :single-argument-with-lemma
                        ))
  :visualization-configurations ((:show-constructional-dependencies . nil))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :cxn-inventory *propbank-learned-cxn-inventory*
  :hashed t)


;;;;;;;;;;;
;; Data  ;;
;;;;;;;;;;;

(defparameter *opinion-sentences* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *opinion-sentences-dev* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(defparameter *believe-sentences* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *believe-sentences-dev* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

(defparameter *difficult-sentence* (nth 6063 (train-split *propbank-annotations*))) ;;13 frames!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing and restoring grammars ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-english" "learning")
                                :name "learned-grammar-multi-core-argm-lemma-and-arg-pp"
                                :type "fcg"))

(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "learning")
                           :name "learned-grammar-multi-argument-allsentences"
                           :type "fcg")))

;;;;;;;;;;;;;;
;; Training ;;
;;;;;;;;;;;;;;

(defparameter *training-configuration*
  '((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation)
    (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
    (:parse-goal-tests :gold-standard-meaning) ;:no-valid-children
    (:max-nr-of-nodes . 100)
    (:node-expansion-mode . :multiple-cxns)
    (:priority-mode . :nr-of-applied-cxns)
    (:queue-mode . :greedy-best-first)
    (:hash-mode . :hash-lemma)
    (:parse-order
     multi-argument-core-only
     argm-pp-with-lemma
     argm-with-lemma)
    (:equivalent-cxn-fn . fcg::equivalent-propbank-construction)
    (:equivalent-cxn-key . identity)
    (:learning-modes
     ;:multi-argument-with-lemma
     :argm-with-lemma-with-v-lex-class
     :argm-pp-with-v-lemma
     :multi-argument-core-only
     ;:multi-argument-without-lemma
     ;:single-argument-with-lemma
     ;:single-argument-without-lemma
     )
    (:cxn-supplier-mode . :hashed-scored-labeled)))


(with-disabled-monitor-notifications
  (learn-propbank-grammar *opinion-sentences*
                          :cxn-inventory '*propbank-learned-cxn-inventory*
                          :fcg-configuration *training-configuration*
                          :selected-rolesets '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                          :silent t
                          :tokenize? nil))


;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(set-configuration *propbank-learned-cxn-inventory* :parse-goal-tests '(:no-valid-children))
(set-configuration *propbank-learned-cxn-inventory* :parse-order '(multi-argument-core-only argm-pp-with-lemma argm-with-lemma))

(evaluate-propbank-sentences
 *opinion-sentences-dev*
 *propbank-learned-cxn-inventory*
 :list-of-syntactic-analyses (mapcar #'syntactic-analysis *opinion-sentences-dev*)
 :selected-rolesets  '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
 :silent t)

(evaluate-propbank-sentences-per-roleset
 (subseq (shuffle (dev-split *propbank-annotations*))  0 5)
 *propbank-learned-cxn-inventory*
 :silent t)


;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;


(setf *selected-sentence*
      (find "At this , a call was made to the governor of Wadi al - Dawasir by my uncle on his mobile phone , \" later my lawyer \" thinking that he was at the Wadi , and an argument took place with him ." *opinion-sentences* :key #'sentence-string :test #'string=))

(learn-cxn-from-propbank-annotation *selected-sentence* "complete.01" *propbank-learned-cxn-inventory* :argm-pp-with-v-lemma)

(learn-propbank-grammar (list *selected-sentence*)
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :fcg-configuration *training-configuration*
                        :selected-rolesets '("expect.01")
                        :silent t
                        :tokenize? nil)
(activate-monitor trace-fcg)
(with-activated-monitor trace-fcg
  (add-element (make-html (second (multiple-value-list (comprehend *selected-sentence* :cxn-inventory *propbank-learned-cxn-inventory*)
                                                       )))))

(add-element (make-html (first (multiple-value-list (comprehend *selected-sentence* :cxn-inventory *propbank-learned-cxn-inventory*)
                                                       ))))

(add-element (make-html (de-render (sentence-string *selected-sentence*) :de-render-constituents-dependents-without-tokenisation)))

(add-element (make-html *propbank-learned-cxn-inventory*))

(evaluate-propbank-sentences
 (list *selected-sentence* *propbank-learned-cxn-inventory* :selected-rolesets  '("believe.01")  :silent t))

;; Testing new sentences with learned grammar 
;; Guardian FISH article
;;;;;;;;;;;;;;;;;;;;;;;;;

(set-configuration *restored-grammar* :parse-goal-tests '(:no-valid-children))
(set-configuration *restored-grammar* :de-render-mode :de-render-constituents-dependents)

(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *restored-grammar*)

;;threaten.01 niet gevonden (cxns met enkel core roles zouden dit oplossen)
(comprehend-and-extract-frames "The depletion of oxygen in our oceans threatens future fish stocks and risks altering the habitat and behaviour of marine life, scientists have warned, after a new study found oceanic oxygen levels had fallen by 2% in 50 years." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "He goes to the store" :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date." :cxn-inventory *restored-grammar*)

;;attribute.01 wordt niet gevonden > 'ARG1:NP - has been attributed - ARG2:PP nooit gezien in training'
(comprehend-and-extract-frames "The fall in oxygen levels has been attributed to global warming and the authors warn that if it continues unchecked, the amount of oxygen lost could reach up to 7% by 2100." :cxn-inventory *restored-grammar*)
(add-element (make-html (find-cxn "ATTRIBUTE.01-ARG1:NP+V:ATTRIBUTE+ARG2:PP+2-CXN-6" *restored-grammar* :hash-key 'PROPBANK-ENGLISH::ATTRIBUTE :test #'string=)))

;;adapt-cxn niet geleerd:
(comprehend-and-extract-frames "Very few marine organisms are able to adapt to low levels of oxygen." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The paper contains analysis of wide-ranging data from 1960 to 2010, documenting changes in oxygen distribution in the entire ocean for the first time ." :cxn-inventory *restored-grammar*)

;;verkeerde analyse (mss quotes anders zetten?)
(comprehend-and-extract-frames "Since large fish in particular avoid or do not survive in areas with low oxygen content, these changes can have far-reaching biological consequences, said Dr Sunke Schmidtko, the report's lead author . " :cxn-inventory *restored-grammar*)

;;have? mss have uitschakelen voor toepassingen?
(comprehend-and-extract-frames "Some areas have seen a greater drop than others ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "The Pacific - the planet's largest ocean - has suffered the greatest volume of oxygen loss, while the Arctic witnessed the sharpest decline by percentage ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames " ' While the slight decrease of oxygen in the atmosphere is currently considered non-critical, the oxygen losses in the ocean can have far-reaching consequences because of the uneven distribution, ' added another of the report's authors, Lothar Stramma ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "It is increasingly clear that the heaviest burden of climate change is falling on the planet's oceans, which absorb more than 30% of the carbon produced on land ." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Rising sea levels are taking their toll on many of the world's poorest places ." :cxn-inventory *restored-grammar*)

;;DEVASTATE niet gevonden!(sparseness) ARG0:NP - have devastated - ARG1:NP
(comprehend-and-extract-frames "Warming waters have devastated corals - including the Great Barrier Reef - in bleaching events." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Acidic oceans, caused by a drop in PH levels as carbon is absorbed, threaten creatures' ability to build their calcium-based shells and other structures." :cxn-inventory *restored-grammar*)

;;CAUSED niet gevonden! Triggered ook niet!
(comprehend-and-extract-frames "Warming waters have also caused reproductive problems in species such as cod, and triggered their migration to colder climates." :cxn-inventory *restored-grammar*)

;; goed
(comprehend-and-extract-frames "Lower oxygen levels in larger parts of the ocean are expected to force animals to seek out ever shrinking patches of habitable water, with significant impacts on the ecosystem and food web." :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "Callum Roberts, the author of Ocean of Life and a marine conservation biologist at the University of York, is unsurprised by the latest findings." :cxn-inventory *restored-grammar*)

;goed maar veel be's en have's(!!) >> vreemde have cxn geleerd! enkel pronoun, geen v
(comprehend-and-extract-frames "'What we're seeing is fallout from global warming,' he says." :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "'It's straightforward physics and chemistry playing out in front of our eyes, entirely in keeping with what we'd expect and yet another nail in coffin of climate change denial.'" :cxn-inventory *restored-grammar*)


(comprehend-and-extract-frames "Scientists have long predicted ocean deoxygenation due to climate change, but confirmation on this global scale, and at deep sea level, is concerning them." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Last year, Matthew Long, an oceanographer at the National Center for Atmospheric Research in Colorado, predicted that oxygen loss would become evident 'across large regions of the oceans' between 2030 and 2040." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Reacting to the German findings, Long said it was 'alarming to see this signal begin to emerge clearly in the observational data', while Roberts said, 'We now have a measurable change which is attributable to global warming.'" :cxn-inventory *restored-grammar*)



(comprehend-and-extract-frames "The report explains that the ocean's oxygen supply is threatened by global warming in two ways." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Warmer water is less able to contain oxygen than cold, so as the oceans warm, oxygen is reduced." :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames  "Warmer water is also less dense, so the oxygen-rich surface layer cannot easily sink and circulate. " :cxn-inventory *restored-grammar*)