(in-package :slp)

(defmethod de-render ((signed-form-predicates signed-form-predicates)
                      (mode (eql :signed-form-predicates)) &key &allow-other-keys)
  "De-renders a set of signed-form-predicates into a transient structure"
  (make-instance
   'coupled-feature-structure 
   :left-pole `((root (form ,(predicates signed-form-predicates))))
   :right-pole `((root))))


(defmethod render ((ts coupled-feature-structure) (mode (eql :signed-form-predicates)) &key &allow-other-keys)
  "renders signed-form-predicates by extracting all form features from the resulting transient structure in production"
  (make-instance 'signed-form-predicates
                 :predicates (extract-forms (left-pole-structure ts))))






