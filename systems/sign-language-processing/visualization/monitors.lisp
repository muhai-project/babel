(in-package :slp)

(define-monitor
 trace-slp :documentation
 "traces processing using sign language grammars in the webinterface")

(define-event-handler (trace-slp parse-started)
  (add-element `((h1) "Comprehension"))
  (add-element `((h2) "Comprehending the following signed utterance:"))
  (add-element (make-html (first utterance))))


(define-event-handler (trace-slp cip-started)
  (add-element `((h2 :class "banner") "Construction application"))
  (add-element 
   `((h3) "Construction inventory:"
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size (construction-inventory cip)) (get-configuration (construction-inventory cip) 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (fcg::get-construction-inventory-title-string (original-cxn-set (construction-inventory cip)))
                (size (original-cxn-set (construction-inventory cip))))
        (make-html (original-cxn-set (construction-inventory cip))))
     ;; For printing categorial networks
     ,(if (get-configuration (visualization-configuration (construction-inventory cip)) :show-categorial-network)
        "Categorial network:"
        "")
     ,(if (get-configuration (visualization-configuration (construction-inventory cip)) :show-categorial-network)
        (if (> (nr-of-categories (categorial-network (original-cxn-set (construction-inventory cip))))
               (/ (get-configuration (construction-inventory cip) :max-size-for-html) 10))
          (format nil " (~a categories, ~a links, ~a link types)"
                  (nr-of-categories (original-cxn-set (construction-inventory cip)))
                  (nr-of-links (original-cxn-set (construction-inventory cip)))
                  (length (link-types (original-cxn-set (construction-inventory cip)))))
          (make-html (categorial-network (original-cxn-set (construction-inventory cip)))
                     :weights? t :render-program "circo" :expand-initially nil))
        ""))))

(define-event-handler (trace-slp cip-finished)
  (add-element
   `((div :style "margin:10px;")
     ,(make-html-fcg-light cip :solutions (when solution (list solution))))))

(define-event-handler (trace-slp parse-finished)
  (add-element `((h2 :class "banner") "Resulting meaning network:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                   (predicate-network-with-wiki-links->svg meaning)
                   (predicate-network->svg meaning))
                 (html-pprint meaning)))
  (unless meaning
    (add-element `((h3) "No meaning network could be constructed"))))

(define-event-handler (trace-slp produce-started)
  (add-element `((h1) "Formulation"))
  (add-element `((h2) "Formulating expression for the following meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event produce-finished (utterance signed-form-predicates))

(define-event-handler (trace-slp produce-finished)
  (add-element `((h2 :class "banner") "Resulting signed expression:"))
  (if (predicates utterance)
    (add-element (make-html utterance))
    (add-element `((h3) "No signed expression was formulated"))))

(define-event-handler (trace-slp parse-all-started)
  (add-element `((h1) "Comprehension (all solutions)"))
  (add-element `((h2) "Comprehending the following signed utterance:"))
  (add-element (make-html (first utterance))))

(define-event-handler (trace-slp parse-all-finished)
  (add-element `((h2 :class "banner") "All resulting meaning networks:"))
  (loop for meaning in fcg::meanings
        for i from 1
        do
        (add-element `((h4) ,(format nil "meaning ~a " i)))
        (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                       (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                         (predicate-network-with-wiki-links->svg meaning)
                         (predicate-network->svg meaning))
                       (html-pprint meaning))))
  (unless fcg::meanings
    (add-element `((h3) "No meaning network could be constructed"))))

(define-event-handler (trace-slp fcg::fcg-apply-w-n-solutions-started)
  (add-element `((h2 :class "banner") "Construction application"))
  (add-element 
   `((h3) "Construction inventory:"
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size construction-inventory) (get-configuration construction-inventory 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (fcg::get-construction-inventory-title-string (original-cxn-set construction-inventory))
                (size (original-cxn-set construction-inventory)))
        (make-html (original-cxn-set construction-inventory)))
     ;; For printing categorial networks
     ,(if (get-configuration (visualization-configuration construction-inventory) :show-categorial-network)
        "Categorial network:"
        "")
     ,(if (get-configuration (visualization-configuration construction-inventory) :show-categorial-network)
        (if (> (nr-of-categories (categorial-network (original-cxn-set construction-inventory)))
               (/ (get-configuration construction-inventory :max-size-for-html) 10))
          (format nil " (~a categories, ~a links, ~a link types)"
                  (nr-of-categories (original-cxn-set construction-inventory))
                  (nr-of-links (original-cxn-set construction-inventory))
                  (length (link-types (original-cxn-set construction-inventory))))
          (make-html (categorial-network (original-cxn-set construction-inventory))
                     :weights? t :render-program "circo" :expand-initially nil))
        ""))))

(define-event-handler (trace-slp fcg::fcg-apply-w-n-solutions-finished)
  (add-element (make-html-fcg-light cip :solutions fcg::solutions)))


(define-event-handler (trace-slp produce-all-started)
  (add-element `((h1) "Formulation (all solutions)"))
  (add-element `((h2) "Formulating expression for the following meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                   (predicate-network-with-wiki-links->svg meaning :only-variables nil)
                   (predicate-network->svg meaning :only-variables nil))
                 (html-pprint meaning))))


(define-event-handler (trace-slp produce-all-finished)
  (add-element `((h2 :class "banner") "Resulting signed expressions:"))
  (loop with counter = 1
        for utterance in fcg::utterances
        do (add-element `((h3) ,(format nil "utterance ~a" counter)))
           (add-element (make-html utterance))
           (incf counter)))