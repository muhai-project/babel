(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;
;;              ;;
;; Node tests   ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defmethod cip-node-test ((node cip-node) (mode (eql :check-double-role-assignment)))
  "Node test that checks if there is a frame in the resulting meaning
in which there are duplicate role assignments (i.e. unit name of
frame-element filler occurs in more than one slot). "
  (let ((extracted-frames (group-by (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))
                                    #'third :test #'equalp)))
    (loop with double-role-assignments = nil
          for (frame-var . frame) in extracted-frames
          for frame-elements = (loop for predicate in frame
                                     when (equalp (first predicate) 'frame-element)
                                     collect predicate)
          
          when (or (> (length frame-elements)
                      (length (remove-duplicates frame-elements :key #'fourth :test #'equalp)))
                    (loop for fe in frame-elements
                          for other-fes = (remove fe frame-elements :key #'fourth :test #'equalp)
                          thereis (subconstituent-p (fourth fe) (mapcar #'fourth other-fes) (left-pole-structure (car-resulting-cfs (cipn-car node))))))
          do (push frame-var double-role-assignments)
          finally
          (return
           (if double-role-assignments
             ;;some frames contain frame-elements that have identical slot fillers
             (and (push 'double-role-assignment (statuses node)) nil)
             t)))))

(defun subconstituent-p (frame-element other-frame-elements unit-structure)
  (loop for ofe in other-frame-elements
        when (subconstituent-p-aux frame-element ofe unit-structure)
        do (return t)))

(defun subconstituent-p-aux (frame-element other-frame-element unit-structure)
  (let ((parent (cadr (find 'parent (unit-body (find frame-element unit-structure :key #'unit-name)) :key #'feature-name))))
    (cond ((null parent)
           nil)
          ((equalp parent other-frame-element)
           t)
          ((subconstituent-p-aux parent other-frame-element unit-structure)
           t))))

