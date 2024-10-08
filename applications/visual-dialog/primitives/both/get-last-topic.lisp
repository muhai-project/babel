(in-package :visual-dialog)

(defprimitive get-last-topic ((target-set world-model)
                              (source-set world-model))
  ((source-set => target-set)
   (multiple-value-bind
      (last-set last-timestamp)
      (the-biggest #'timestamp (set-items source-set))
     (let* ((last-topic (find-last-topic (set-items source-set)))
            (last-topic-object-set
             (if (object-set last-set)
               (loop for obj in (objects (object-set last-set)) 
                     when (member (id obj) last-topic)
                       collect obj))))
       (bind (target-set 1.0
                         (make-instance 'world-model
                                        :set-items (list (make-instance 'turn
                                                                        :id (id (first (set-items source-set)))
                                                                        :object-set (make-instance 'object-set
                                                                                                   :objects last-topic-object-set)))))))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))

(defun find-last-topic (lst)
  (multiple-value-bind
      (last-set last-timestamp)
      (the-biggest #'timestamp lst)
    (if last-set
      (let ((last-topic (topic-list last-set)))
        (if last-topic
          last-topic
          (find-last-topic (rest lst)))))))

   