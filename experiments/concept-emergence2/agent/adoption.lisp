(in-package :cle)

;; ----------------------------
;; + Adoption of construction +
;; ----------------------------

;;;; events
(define-event event-adopt-start (cxn cxn))

(defmethod adopt ((agent cle-agent) meaning form)
  "Adopt a new cxn into the lexicon of the agent."
  (let ((new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify
    (notify event-adopt-start new-cxn)
    ;; return created construction
    new-cxn))

(defmethod reset-adopt ((agent cle-agent) applied-cxn meaning)
  "Resets the cxn by resetting its meaning."
  ;; if the cxn is in the trash, place it back in the lexicon
  (when (<= (score applied-cxn) 0.0)
    (push applied-cxn (lexicon agent))
    (setf (trash agent) (remove applied-cxn (trash agent))))  
  (reset-cxn agent applied-cxn meaning)
  ;; update monitor
  (setf (invented-or-adopted agent) t)
  ;; notify
  (notify event-adopt-start applied-cxn)
  ;; return created construction
  applied-cxn)
