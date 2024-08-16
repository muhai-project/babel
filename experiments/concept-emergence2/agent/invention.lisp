(in-package :cle)

;; ------------------------
;; + Invention of concepts +
;; ------------------------

;; events
(define-event event-invention-end (cxn cxn))

(defmethod invent ((agent cle-agent))
  "Invent a new cxn with a new form and a meaning based on the topic of the interaction."
  (let* ((meaning (get-data agent 'topic))
         (form (make-new-word))
         (new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; set the applied-cxn slot
    (set-data agent 'applied-cxn new-cxn)
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify
    (notify event-invention-end new-cxn)))
