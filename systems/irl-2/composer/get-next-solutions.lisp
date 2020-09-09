(in-package :irl-2)

;; #########################################
;; composer search
;; -----------------------------------------

(define-event chunk-composer-get-next-solutions-started
  (composer chunk-composer))
(define-event chunk-composer-get-all-solutions-started
  (composer chunk-composer))
(define-event chunk-composer-get-solutions-until-started
  (composer chunk-composer))
(define-event chunk-composer-node-handled
  (node chunk-composer-node)
  (handler symbol))
(define-event chunk-composer-new-nodes
  (nodes list))
(define-event chunk-composer-next-node
  (node chunk-composer-node))
(define-event chunk-composer-finished
  (solutions list) (composer chunk-composer))

(export '(get-next-solutions
          get-all-solutions
          get-solutions-until))

(defun enqueue-node (node composer)
  (when (next-handler node)
    (setf (queue composer)
          (sorted-insert (queue composer) node
                         :key #'node-rating :test #'<))))
                 

(defun get-next-solutions (composer &key silent)
  (unless silent
    (notify chunk-composer-get-next-solutions-started composer))
  (when (queue composer)
    ;; keep a copy of the primitive inventory configurations
    ;; and apply the local configurations, if present
    (let ((stored-primitive-inventory-configurations
           (copy-object
            (configuration
             (primitive-inventory composer)))))
      (when (primitive-inventory-configurations composer)
        (loop for (key . value) in (entries (primitive-inventory-configurations composer))
              do (set-configuration (primitive-inventory composer)
                                    key value :replace t)))
      (loop
       for node = (pop (queue composer))
       for handler = (next-handler node)
       for (solutions new-nodes)
       = (multiple-value-list
          (handle-node node handler composer))
       ;; handle the node
       do (progn
            (unless silent
              (notify chunk-composer-node-handled node handler))
            (enqueue-node node composer))
       ;; handle new nodes
       when new-nodes
       do (loop for new-node in new-nodes
                do (add-node composer new-node :parent node)
                do (enqueue-node new-node composer)
                finally
                (unless silent
                  (notify chunk-composer-new-nodes new-nodes)))
       ;; handle solutions
       when solutions
       do (progn
            (loop for solution in solutions
                  do (setf (score solution)
                           (score-solution solution composer)))
            (setf (solutions composer)
                  (sort (append solutions (solutions composer))
                        #'> :key #'score)))
       ;; continue loop
       when (and (queue composer)
                 (not silent))
       do (notify chunk-composer-next-node (first (queue composer)))
       while (queue composer)
       until solutions
       finally
       (progn
         ;; notify
         (unless silent
           (notify chunk-composer-finished solutions composer))
         ;; restore the primitive inventory configurations
         (loop for (key . value) in (entries stored-primitive-inventory-configurations)
               do (set-configuration (primitive-inventory composer)
                                     key value :replace t))
         ;; return the solutions
         (return solutions))))))


(defun get-all-solutions (composer &key silent)
  (unless silent
    (notify chunk-composer-get-all-solutions-started composer))
  (loop while (queue composer)
        do (get-next-solutions composer :silent silent))
  (solutions composer))


(defun get-solutions-until (composer &key (stop-criteria #'identity)
                                     silent)
  (unless silent
    (notify chunk-composer-get-solutions-until-started composer))
  (sort
   (loop while (and (queue composer)
                    (not (funcall stop-criteria composer)))
         append (get-next-solutions composer :silent silent))
   #'> :key #'score))