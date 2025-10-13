(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;
;;              ;;
;; Goal tests   ;;
;;              ;;
;;;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-valid-children)))
  "Checks whether there are no more applicable constructions when a node is
fully expanded and no constructions could apply to its children
nodes."
  (and (fully-expanded? node)
       (or (not (children node))
	   (loop for child in (children node)
                 never (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child))))))))
