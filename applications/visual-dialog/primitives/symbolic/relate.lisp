(in-package :visual-dialog)

;; ------------------
;; RELATE primitive ;;
;; ------------------

(defprimitive relate ((target-set world-model)
                      (source-world-model world-model)
                      (segmented-scene world-model)
                      (scene pathname-entity)
                      (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-world-model spatial-relation segmented-scene scene => target-set)
   (let* (;(context (get-data ontology 'context))
          ;(context (cdr (find (pathname scene) (get-data ontology 'segmented-scene) :test #'equal :key #'first)))
          (context segmented-scene)
          (source-object (first (collect-objects-from-world-model source-world-model)))
          relation-list id-list id-right object-right object-list)    
     (cond ((eql (spatial-relation spatial-relation) 'right)
            (setf relation-list (immediate-right (scene-configuration (object-set (first (set-items context)))))))
           ((eql (spatial-relation spatial-relation) 'left)
            (setf relation-list (loop for item in (reverse (immediate-right (scene-configuration (object-set  (first (set-items context))))))
                                      collect (reverse item))))
           ((eql (spatial-relation spatial-relation) 'front)
            (setf relation-list (immediate-front (scene-configuration (object-set (first (set-items context)))))))
           ((eql (spatial-relation spatial-relation) 'behind)
            (setf relation-list (loop for item in (reverse (immediate-front (scene-configuration (object-set  (first (set-items context))))))
                                      collect (reverse item)))))

#|     (if relation-list
       (progn 
         (loop for item in relation-list
               do (progn
                    (if (eq (car item) (id source-object))
                      (progn 
                        (push (car (cdr item)) id-list)
                        (setf id-right (car (cdr item)))))))
         (loop for i from 1 to 5
               do (loop for item2 in relation-list
                        do (progn
                             (if (eq (car item2) id-right)
                               (progn
                                 (push (car (cdr item2)) id-list)
                                 (setf id-right (car (cdr item2))))))))))|#
     (if relation-list
       (setf id-list (collect-all-right-of (id source-object) relation-list)))
     
     (loop for id in id-list
           do (loop for object in (objects (object-set (first (set-items context))))
                    do (if (and (eq (id object) id)
                                (not (find (id object) object-list :key #'id)))
                         (push object object-list))))
     
     (if object-list
       (bind (target-set 1.0 (make-instance 'world-model
                                            :set-items (list (make-instance 'turn
                                                                            :object-set (make-instance 'object-set
                                                                                                       :objects object-list))))))
       (bind (target-set 1.0 (make-instance 'world-model
                                            :set-items (list (make-instance 'turn
                                                                            :object-set (make-instance 'object-set
                                                                                                       :id 'empty-set)))))))))
  :primitive-inventory *symbolic-primitives*)
     


(defun collect-all-right-of (id immediate-list)
  (let ((imms (find-all id immediate-list :key #'car)))
    (if imms
      (loop for imm in imms 
            append (cons (last-elt imm) (collect-all-right-of (last-elt imm) immediate-list)))
      nil)))
