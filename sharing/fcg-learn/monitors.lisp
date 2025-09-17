(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Functionality for monitoring both routine application and learning ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(trace-fcg-learning trace-fcg-learning-in-output-browser))

(define-monitor trace-fcg-learning 
    :documentation "Graphically traces routine processing as well as learning.")

(define-monitor trace-fcg-learning-in-output-browser 
    :documentation "Traces routine processing as well as learning in the output browser.")

(define-monitor trace-fcg-learning-in-output-browser-verbose 
    :documentation "Traces routine processing as well as learning in the output browser interaction per interaction.")

(define-event routine-comprehension-started (speech-act speech-act) (n t))

(define-event-handler (trace-fcg-learning routine-comprehension-started)
  (add-element `((hr :style "margin-block-end: 0px;")))
  (add-element `((h2 :style "padding: 15px; margin: 0px; background-color: #33FFA4;") ,(format nil "Routine comprehension: &quot;~a&quot; ~@[~a~]"
                                                                                               (form speech-act)
                               (when n (format nil "(max ~a solution~:p considered)" n)))))
  (add-element `((hr :style "margin-block-start: 0px;"))))

(define-event-handler (trace-fcg-learning all-diagnostics-run)
  (when problems
    (add-element `((hr :style "margin-block-end: 0px;")))
    (add-element `((h2 :style "padding: 15px; margin: 0px; background-color: #ff5252;") "Meta-level learning"))
    (add-element `((hr :style "margin-block-start: 0px;")))
    (add-element `((h4) ,(format nil "~a problem~:p diagnosed:" (length problems))))
    (loop for problem in problems
          do (add-element (make-html problem)))))

(define-event-handler (trace-fcg-learning notify-learning-finished)
  (when problems
    (if fixes
      (progn
        (add-element `((h4) ,(if (> (length fixes) 1)
                               (format nil "~a fixes created:" (length fixes))
                               "1 fix created:")))
        (loop for fix in fixes
              do (add-element (make-html fix)))
        (when (find 'anti-unification-fix fixes :key #'type-of)
          (add-element '((h4) "Anti-unification process:"))
          (add-element (make-html (top-state (au-repair-processor (anti-unification-state (find 'anti-unification-fix fixes :key #'type-of))))))))
        
      (add-element `((h4) "No fixes created.")))))


(define-event routine-comprehension-finished (solution t) (cip construction-inventory-processor))

(define-event-handler (trace-fcg-learning routine-comprehension-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution))))
  (if solution
    (if (get-data cip :best-solution-matches-gold-standard)
      (add-element `((h3 :style "color:#050;") ,(format nil "Best result found in node ~a, matching the gold standard."
                                (created-at solution))))
      (add-element `((h3 :style "color:#ff0000;") ,(format nil "Best result found in node ~a, but not matching the gold standard."
                                (created-at solution)))))
    (add-element `((h3  :style "color:#ff0000;") "No solution found.")))
  (add-element `((p) " ")))


(define-event meta-level-learning-finished (cip construction-inventory-processor) (solution t)
  (consolidated-cxns list) (consolidated-categories list) (consolidated-links list))

(define-event-handler (trace-fcg-learning meta-level-learning-finished)
  (when solution
    (add-element `((h4) "Applying fixes:"))
    (let ((subtree-id (mkstr (make-id 'subtree-id))))
      (add-element `((div :id ,subtree-id)
                     ,(make-html-fcg-light 
                       (top-node cip)
                       :subtree-id subtree-id
                       :hide-subtrees-with-duplicates t
                       :configuration (configuration (construction-inventory cip)))))))
  (add-element `((h4) "Consolidation:"))
  (if (or consolidated-cxns consolidated-categories consolidated-links)
    (add-element
     `((div)
       ((table :class "two-col")
        ((tbody)
         ,@(when consolidated-cxns
             `(((tr) 
                ((td) "constructions added:")
                ((td) ,@(loop for cxn in consolidated-cxns
                              collect (make-html cxn :cxn-inventory (construction-inventory cip) :expand-initially nil))))))
         ,@(when consolidated-categories
             `(((tr) 
                ((td) "categories added:")
                ((td) ,@(loop for cat in consolidated-categories
                              collect (make-html cat))))))
         ,@(when consolidated-links
             `(((tr) 
                ((td) "categorial links added:")
                ((td) ,@(loop for link in consolidated-links
                              collect (make-html link))))))))))
    (add-element `((h4) "No constructions or categorial links were consolidated."))))


(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-started)
  (add-element 
   `((h4)
     ,(make-html (original-cxn-set construction-inventory))
     ,(make-html (categorial-network (original-cxn-set construction-inventory))
                 :weights? t :render-program "circo" :expand-initially nil))))

(define-event entrenchment-started)

(define-event-handler (trace-fcg-learning entrenchment-started)
  (add-element `((hr :style "margin-block-end: 0px;")))
  (add-element `((h2 :style "padding: 15px; margin: 0px; background-color: #34b4eb;") "Alignment"))
  (add-element `((hr :style "margin-block-start: 0px;"))))

(define-event entrenchment-finished (rewarded-cxns list) (rewarded-links list) (punished-cxns list) (punished-links list)
  (deleted-cxns list) (deleted-categories list) (deleted-links list))

(define-event-handler (trace-fcg-learning entrenchment-finished)
  (when rewarded-cxns
    (add-element `((h4) "Constructions rewarded:"))
    (loop for cxn in rewarded-cxns
          do (add-element (make-html cxn :cxn-inventory (cxn-inventory cxn) :expand-initially nil))))
  (when rewarded-links
    (add-element `((h4) "Categorial links rewarded:"))
    (loop for link in rewarded-links
          do (add-element (make-html link))))
  (when punished-cxns
    (add-element `((h4) "Constructions punished:"))
    (loop for cxn in punished-cxns
          do (add-element (make-html cxn :cxn-inventory (cxn-inventory cxn) :expand-initially nil))))
  (when punished-links
    (add-element `((h4) "Categorial links punished:"))
    (loop for link in punished-links
          do (add-element (make-html link))))
  (when deleted-cxns
    (add-element `((h4 :style "color: #FF0000") "Constructions deleted:"))
    (loop for cxn in deleted-cxns
          do (add-element (make-html cxn :cxn-inventory (cxn-inventory cxn) :expand-initially nil))))
  (when deleted-categories
    (add-element `((h4 :style "color: #FF0000") "Categories deleted:"))
    (loop for category in deleted-categories
          do (add-element (make-html category))))
  (when deleted-links
    (add-element `((h4 :style "color: #FF0000") "Categorial links deleted:"))
    (loop for link in deleted-links
          do (add-element (make-html link))))
  (unless (or rewarded-cxns punished-cxns deleted-cxns deleted-categories deleted-links)
    (add-element `((h4) "No alignment took place.")))
  (add-element `((p) " ")))
  

(define-event next-speech-act (cp corpus-processor))

(define-event-handler (trace-fcg-learning next-speech-act)
  (add-element `((hr)))
  (add-element `((h2 :style "padding: 15px; margin: 0px; color: #ffffff;  background-color: #000000;") ,(format nil "Speech act ~a" (+ 1 (counter cp))))))

(define-event-handler (trace-fcg-learning-in-output-browser next-speech-act)
  (let ((interaction-number (+ 1 (counter cp))))
    (if (= 1 interaction-number)
      (format t "~%~%## Experiment started ##~%"))))

(define-event-handler (trace-fcg-learning-in-output-browser routine-comprehension-finished)
  (when (and solution (get-data (cip solution) :best-solution-matches-gold-standard))
             (format t ".")))

(define-event-handler (trace-fcg-learning-in-output-browser meta-level-learning-finished)
  (let* ((problem (first (problems cip)))
         (fix (when problem (first (fixes problem)))))
    (when (problems cip)
      (cond ((eql 'gold-standard-elsewhere-in-search-space (type-of problem))
             (format t "e"))
            ((eql 'categorial-link-fix (type-of fix))
             (format t "c"))
            ((eql 'holophrastic-fix (type-of fix))
             (format t "h"))
            ((eql 'anti-unification-fix (type-of fix))
             (format t "a"))
            (t
             (format t "x"))))))


(define-event speech-act-finished (cp corpus-processor))

(define-event-handler (trace-fcg-learning-in-output-browser speech-act-finished)
  (when (= 0 (mod (counter cp) 100))
    (format t " (~a)~%" (counter cp))))







(define-event-handler (trace-fcg-learning-in-output-browser-verbose next-speech-act)
  (let ((interaction-number (+ 1 (counter cp))))
    (if (= 1 interaction-number)
      (format t "~%~%## Experiment started ##~%~%"))
    (format t "Speech act ~a~%" interaction-number)))

(define-event-handler (trace-fcg-learning-in-output-browser-verbose routine-comprehension-started)
  (format t "FORM: ~a~%" (form speech-act))
  (format t "MEANING: ~(~a~)~%" (meaning speech-act)))

(define-event-handler (trace-fcg-learning-in-output-browser-verbose meta-level-learning-finished)
  (format t "PROBLEMS: ~a~%" (if (problems cip) (loop for problem in (problems cip)
                                                      collect (type-of problem) into ps
                                                      finally (return (format nil "~{~(~a~)~^ ~}" ps)))
                               "/"))
  (format t "REPAIRS: ~a~%" (if (problems cip) (loop for problem in (problems cip)
                                                     for fixes = (fixes problem)
                                                     when fixes
                                                       append (mapcar #'type-of fixes) into fs
                                                     finally (return (format nil "~{~(~a~)~^ ~}" fs)))
                              "/"))
  (format t "~%"))
