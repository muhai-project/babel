
(in-package :irl)


;; ############################################################################
;; trace-irl-in-web-browser and trace-irl-in-web-browser-verbose
;; ----------------------------------------------------------------------------

(export '(trace-irl trace-irl-verbose))

(define-monitor trace-irl
    :documentation "Traces calls to the calls to high level functions
                    of irl in the web browser")

(define-monitor trace-irl-verbose
    :documentation "Same as trace-irl-in-web-browser, but more verbose")

;; ============================================================================
;; evaluate-irl-program
;; ----------------------------------------------------------------------------

(define-event-handler ((trace-irl trace-irl-verbose)
                       evaluate-irl-program-started)
  (add-element '((hr)))
  (add-element '((h2) "Evaluating irl program"))
  (add-element `((table :class "two-col")
                 ((tbody)
                  ,(make-tr-for-irl-program "irl program" irl-program))))
  (add-element '((hr)))
  (add-element `((h3) "Applying" ((br))
                 ,(make-html primitive-inventory) ((br))
                 "on the ontology" ((br))
                 ,(make-html ontology)))
  (add-element '((hr))))

(define-event-handler ((trace-irl trace-irl-verbose)
                       evaluate-irl-program-finished)
  (add-element
   (make-html-for-pip pip))
  (add-element '((h3) "Solutions:"))
  (let ((sorted-solutions (collect-solutions succeeded-nodes)))
    (solutions->html sorted-solutions)))


;; ============================================================================
;; pip-started
;; ----------------------------------------------------------------------------
(define-event-handler (trace-irl-verbose pip-started)
  (add-element '((hr)))
  (add-element
   `((h3) ,(if (children (top pip))
             "Computing next solution for evaluation of "
             "Applying ")
     ,(make-html (primitive-inventory pip)))))

;; ============================================================================
;; pip-next-node
;; ----------------------------------------------------------------------------
(define-event-handler (trace-irl-verbose pip-next-node)
  (add-element '((hr)))
  (add-element
   `((table :class "two-col")
     ((tbody)
      ((tr)
       ((td) "next node: ")
       ((td) ,(make-html pipn)))))))

;; ============================================================================
;; pip-node-expanded
;; ----------------------------------------------------------------------------
(define-event-handler (trace-irl-verbose pip-node-expanded)
  (add-element
   `((table :class "two-col")
     ((tbody)
      ((tr)
       ((td) "expansion: ")
       ((td) ,(make-html pipn)))
      ((tr)
       ((td) "new tree: ")
       ((td) ,(make-html (top (pip pipn)))))
      ((tr)
       ((td) "new queue: ")
       ((td) ,@(html-hide-rest-of-long-list
                (queue (pip pipn)) 5
                #'(lambda (pip-node)
                    (make-html pip-node :draw-children nil)))))))))

;; ============================================================================
;; pip-finished
;; ----------------------------------------------------------------------------
(define-event-handler (trace-irl-verbose pip-finished)
  (add-element '((hr)))
  (if solution
    (add-element
     `((table :class "two-col")
       ((tbody)
        ((tr)
         ((td) "next solution: ")
         ((td) ,(make-html pip :target-node solution))))))
    (add-element
     '((b) "No next solution found.")))
  (add-element '((hr))))

;; ============================================================================
;; primitive-apply-with-n-solutions-started/finished
;; ----------------------------------------------------------------------------
(define-event-handler (trace-irl-verbose primitive-apply-with-n-solutions-started)
  (add-element '((hr)))
  (add-element 
   `((h3) ,(if (numberp n)
             (format nil "Computing max ~a solutions for application of " n)
             "Computing all solutions for application of ")
     ,(make-html primitive-inventory))))

(define-event-handler (trace-irl-verbose primitive-apply-with-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html pip)))

#|
;; ============================================================================
;; match-chunk
;; ----------------------------------------------------------------------------

(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose) match-chunk-started)
  (add-element '((hr)))
  (add-element `((p) "matching chunk " ,(make-html chunk :expand-initially t)))
  (add-element `((p) "with meaning " ,(html-pprint meaning :max-width 100)
                 ,(irl-program->svg meaning))))

(define-event-handler ((trace-irl-in-web-browser 
                        trace-irl-in-web-browser-verbose) match-chunk-finished)
  (if matched-chunks
      (add-element `((p) "matched-chunks: " ((br))
                     ,@(loop for chunk in matched-chunks
                          collect (make-html chunk :expand-initially t))))
      (add-element `((p) ((b) "no results")))))

;; ============================================================================
;; chunk-composer
;; ----------------------------------------------------------------------------

(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-get-next-solutions-started)
  (add-element '((hr)))
  (add-element '((h2) "Computing next composer solution"))
  (add-element `((p) 
                 ,(make-html composer 
                             :verbose (eq monitor-id 
                                          'trace-irl-in-web-browser-verbose)))))
                   
(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-get-all-solutions-started)
  (add-element '((hr)))
  (add-element '((h2) "Computing all composer solutions"))
  (add-element `((p)
                 ,(make-html composer
                             :verbose (eq monitor-id 
                                          'trace-irl-in-web-browser-verbose)))))

(define-event-handler (trace-irl-in-web-browser-verbose chunk-composer-next-node)
  (add-element '((hr)))
  (add-element 
   `((table :class "two-col")
     ((tbody) ((tr) 
               ((td) "current node")
               ((td) ,(make-html node :draw-as-tree nil)))))))

(define-event-handler (trace-irl-in-web-browser-verbose
                       chunk-composer-node-handled)
  (add-element 
   `((table :class "two-col")
     ((tbody) ((tr)
               ((td) "node handled")
               ((td) ,(make-html node :draw-as-tree nil)))))))

(define-event-handler (trace-irl-in-web-browser-verbose chunk-composer-new-nodes)
  (let ((expand/collapse-all-id (make-id 'successors)))
    (add-element 
     `((table :class "two-col")
       ((tbody) 
        ((tr)
         ((td) ,(make-expand/collapse-all-link expand/collapse-all-id "new-nodes"))
         ((td) ,@(loop for successor in successors
                    collect (make-html
                             successor :draw-as-tree nil
                             :expand/collapse-all-id expand/collapse-all-id)))))))))

;;;; (define-event-handler (trace-irl-in-web-browser-verbose
;;;;                        chunk-composer-node-finished)
;;;;   (add-element 
;;;;    `((table :class "two-col")
;;;;      ((tbody)
;;;;       ,(make-tr-for-tree "new tree" (top-node composer))
;;;;       ,(make-tr-for-queue "new&#160;queue" (queue composer))))))

(define-event-handler ((trace-irl-in-web-browser 
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-finished)
  (add-element '((hr)))
  (add-element '((h3) "Result"))
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ,(make-tr-for-tree "composition tree" (top-node composer))
      ,(make-tr-for-queue "queue" (queue composer))
      ,(if solutions
           (make-tr-for-evaluation-results 
            (format nil "Found ~a solutions" (length solutions))
            solutions)
           `((tr) ((td :colspan "2") ((b) "Found no solutions."))))
      ,(if (and (solutions composer) 
                (not (length= (solutions composer) solutions)))
           (make-tr-for-evaluation-results "All solutions of composer so far"
                                           (solutions composer))
           "")))))

(define-event-handler ((trace-irl-in-web-browser 
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-increased-search-depth)
  (add-element '((hr)))
  (add-element `((h2) "Increased search depth to " ,(max-search-depth composer)))
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ((tr)
       ((td) "re-queued nodes")
       ((td) ,@(loop for node in queued-nodes
                  collect (make-html node :draw-as-tree nil))))
      ,(make-tr-for-tree "new tree" (top-node composer))))))
|#              
