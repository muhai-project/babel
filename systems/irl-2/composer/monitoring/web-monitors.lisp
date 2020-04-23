
(in-package :irl-2)

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
                             :verbose (eq monitor-id 'trace-irl-in-web-browser-verbose)))))
                   
(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-get-all-solutions-started)
  (add-element '((hr)))
  (add-element '((h2) "Computing all composer solutions"))
  (add-element `((p)
                 ,(make-html composer
                             :verbose (eq monitor-id 'trace-irl-in-web-browser-verbose)))))

(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose)
                       chunk-composer-get-solutions-until-started)
  (add-element '((hr)))
  (add-element '((h2) "Computing all composer solutions until stop criterion"))
  (add-element `((p)
                 ,(make-html composer
                             :verbose (eq monitor-id 'trace-irl-in-web-browser-verbose)))))

(define-event-handler (trace-irl-in-web-browser-verbose chunk-composer-next-node)
  (add-element '((hr)))
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ((tr) 
       ((td) "current node")
       ((td) ,(make-html node :draw-as-tree nil)))))))

(define-event-handler (trace-irl-in-web-browser-verbose
                       chunk-composer-node-handled)
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ((tr)
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
                    collect (make-html successor :draw-as-tree nil
                             :expand/collapse-all-id expand/collapse-all-id)))))))))

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

|#
