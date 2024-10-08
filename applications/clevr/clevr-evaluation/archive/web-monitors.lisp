;;;; web-monitors.lisp

(in-package :clevr-evaluation)

(define-monitor clevr-web-monitor)

(define-event-handler (clevr-web-monitor question-entry-evaluation-started)
  ;; parameters: image-filename, split, question and question-index
  (let ((image-source-path
         (merge-pathnames (make-pathname :directory `(:relative "images" ,split)
                                         :name image-filename :type "png")
                          *clevr-data-path*))
        (image-dest-path (make-pathname :directory `(:absolute "Users" ,(who-am-i) "Sites")
                                        :name image-filename
                                        :type "png")))
    ;; if necessary, copy the image to the Sites folder
    (unless (probe-file image-dest-path)
      (copy-file image-source-path image-dest-path))
    ;; display on the web interface
    (add-element
     `((table)
       ((tr)
        ((th) "Current scene:"))
       ((tr)
        ((td) ((img :src ,(string-append cl-user::*localhost-user-dir* image-filename)))))))
    (add-element `((h2) ,(format nil "(~a) Question: \"~a\"" question-index question)))))
    
(define-event-handler (clevr-web-monitor question-entry-evaluation-finished)
  ;; parameters: success
  (add-element `((h2) ,(if success
                         `((b :style "color:green") "Success!")
                         `((b :style "color:red") "Failure!"))))
  (add-element '((hr))))

(define-event-handler (clevr-web-monitor question-entry-compare-answers)
  ;; parameters: irl-answer, ground-truth-answer
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Found answer: \"~a\"" (downcase irl-answer))))
  (add-element `((h2) ,(format nil "Ground truth answer is \"~a\"" (downcase ground-truth-answer)))))

(define-event-handler (clevr-web-monitor clevr-evaluation-started)
  (add-element '((h1) "Evaluation Started"))
  (add-element '((hr))))

(define-event-handler (clevr-web-monitor clevr-evaluation-finished)
  (add-element '((hr)))
  (add-element '((h1) "Evaluation Finished"))
  (add-element `((h2) ,(format nil "Comprehension success: ~$\%" (* 100 comprehension-success))))
  (add-element `((h2) ,(format nil "Equal Program success: ~$\%" (* 100 equal-program-success))))
  (add-element `((h2) ,(format nil "Equal Answer success: ~$\%" (* 100 equal-answer-success)))))

(define-monitor trace-meaning-processing)

(define-event-handler (trace-meaning-processing duplicate-context-finished)
  (add-element '((h3) "Duplicate context finished"))
  (add-element `((div) ,(irl-program->svg irl-program))))

(define-event-handler (trace-meaning-processing filter-things-finished)
  (add-element '((h3) "Filter things finished"))
  (add-element `((div) ,(irl-program->svg irl-program))))

(define-event-handler (trace-meaning-processing reverse-nominal-groups-finished)
  (add-element '((h3) "Reverse nominal groups finished"))
  (add-element `((div) ,(irl-program->svg irl-program))))

(define-event-handler (trace-meaning-processing process-network-finished)
  (add-element '((h3) "Processing finished"))
  (add-element `((div) ,(irl-program->svg irl-program))))

(define-event-handler (trace-meaning-processing meaning->tree-finished)
  (add-element '((h3) "Meaning -> tree finished"))
  (add-element `((div)
                 ,(s-dot->svg
                   (make-s-dot program-tree
                               :key (lambda (node)
                                      (format nil "~a(~{~a~^, ~})"
                                              (clevr-function node)
                                              (value-inputs node)))
                               :arrowdir "back")))))