(in-package :clevr-grammar)

(defmethod understand ((utterance string)
                       (cxn-inventory fcg-construction-set)
                       (scene symbol)
                       &key silent)
  "Comprehends an utterance given a scene"
  (let* ((initial-cfs
          (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                     :scene-var scene)))
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply (processing-cxn-inventory cxn-inventory) initial-cfs '<- :notify (not silent))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning (processing-cxn-inventory cxn-inventory)))
        (values meaning solution cip)))))

(defmethod understand-and-formulate ((utterance string)
                                     (cxn-inventory fcg-construction-set)
                                     (scene symbol)
                                     &key silent)
  "Comprehend and formulate"
  (multiple-value-bind (meaning cipn cip)
      (understand utterance cxn-inventory scene
                  :silent silent)
    (declare (ignorable cipn cip))
    (formulate (fcg::instantiate-variables meaning)
               :cxn-inventory cxn-inventory
               :silent silent)))