(in-package :irl)

;; -------------------------------------
;; + make html for composition process +
;; -------------------------------------

(defun make-collapsed-html-for-composition-process (composer)
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    (make-html (top composer) :expand-initially nil
                              :solutions solutions)))
  
(defmethod make-html-for-composition-process ((composer chunk-composer))
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    `((div)
      ((h3) "Composition process")
      ((div :class "indent-irpf") ((div :style "margin-top:-7px")
                                        ,(make-collapsed-html-for-composition-process composer))))))