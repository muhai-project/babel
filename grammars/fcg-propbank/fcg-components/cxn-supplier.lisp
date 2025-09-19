(in-package :fcg)

(export '(cxn-supplier-cxn-sets-hashed-categorial-network sort-cxns-by-frequency-and-categorial-edge-weight))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Construction suplier  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constructions are hashed and make use of links in the categorial network ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-hashed-categorial-network (cxn-supplier-cxn-sets)
  ()
  (:documentation "Construction supplier that combines hashing and categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-categorial-network)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (make-instance 'cxn-supplier-hashed-categorial-network))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-categorial-network) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
    (constructions-for-application-hashed-categorial-network-neigbours node))


(defun constructions-for-application-hashed-categorial-network-neigbours (node)
  "Computes all constructions that could be applied for this node
   based on the hash table and the constructions that are linked to
the node through the links in the categorial network."
  (let* ((lex-cat-neighbours (remove-duplicates (loop for lex-category in (lex-categories node)
                                                      append (neighbouring-categories lex-category
                                                                                      (original-cxn-set (construction-inventory node))))))
         (gram-cat-neighbours (remove-duplicates (loop for gram-category in (gram-categories node)
                                                       append (neighbouring-categories gram-category
                                                                                       (original-cxn-set (construction-inventory node))))))
         (constructions
          (remove nil (loop for cxn in (remove-duplicates (append
                                                           (gethash nil (constructions-hash-table (construction-inventory node)))
                                                           (loop for hash in (hash node (get-configuration node :hash-mode))
                                                                 append (gethash hash (constructions-hash-table (construction-inventory node))))))
                            collect (cond ((attr-val cxn :gram-category)
                                           (when (member (attr-val cxn :gram-category) lex-cat-neighbours)
                                             cxn))
                                          ((attr-val cxn :sense-category)
                                           (when (member (attr-val cxn :sense-category) gram-cat-neighbours)
                                             cxn))
                                          (t
                                           cxn))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; return constructions
    constructions))


(defun lex-categories (node)
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lex-category = (unit-feature-value unit 'lex-category)
        when lex-category
        collect it))

(defun gram-categories (node)
  (loop for unit in (fcg-get-transient-unit-structure node)
        for gram-category = (unit-feature-value unit 'gram-category)
        when gram-category
        collect it))


             