(in-package :pn)

(defmethod variablify-predicate-network (amr-network (mode (eql :amr)))
  "Variablify an AMR network. Return the variablified network
   and the used renamings."
  (assert (null (all-variables amr-network)))
  (let* ((all-arguments
          (loop for predicate in amr-network
                append (loop for symbol in (rest predicate)
                             unless (or (stringp symbol)
                                        (numberp symbol)
                                        (find symbol *amr-constants*)
                                        (find (symbol-name symbol) *amr-constants*
                                              :key #'symbol-name :test #'equalp)
                                        (keywordp symbol))
                             collect symbol)))
         (unique-arguments
          (remove-duplicates all-arguments))
         (renamings
          (loop for arg in unique-arguments
                collect (cons arg (variablify arg))))
         (variablified-network
          (loop for predicate in amr-network
                collect (loop for i from 0
                              for arg in predicate
                              collect (or (and (> i 0) ;;never variablify predicate name!
                                               (assqv arg renamings))
                                          arg)))))
    (values variablified-network renamings)))