(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Hash Mode             ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod hash ((construction construction)
                 (mode (eql :hash-lemma))
                 &key &allow-other-keys)
  "Returns the lemma from the attributes of the construction"
  (when (attr-val construction :lemma)
    (remove nil (list (attr-val construction :lemma)))))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-lemma)) 
                 &key &allow-other-keys)
  "Checks all units for a lemma feature."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lemma = (if (eq (unit-feature-value unit 'node-type) 'leaf)
                      (unit-feature-value unit 'lemma)
                      (or (unit-feature-value unit 'lemma) ;;for phrasals
                          (intern (upcase (unit-feature-value unit 'string)) :fcg-propbank)))
        when lemma
          collect it))
     