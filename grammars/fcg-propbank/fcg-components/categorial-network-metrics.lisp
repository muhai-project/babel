(in-package :graph-utils)

(export '(closest-nodes cosine dot-product norm))

(defmethod cosine ((node-1 symbol) (node-2 symbol) (graph graph) &key edge-type)
  "Returns the cosine between two sparse vectors."
  (cosine (lookup-node graph node-1) (lookup-node graph node-2) graph :edge-type edge-type))

(defmethod cosine ((node-1 integer) (node-2 integer) (graph graph) &key edge-type)
  "Returns the cosine between two sparse vectors."
  (let ((denominator (* (norm node-1 graph :edge-type edge-type)
                        (norm node-2 graph :edge-type edge-type))))
    (if (= 0 denominator)
      0
      (/ (dot-product node-1 node-2 graph :edge-type edge-type)
         denominator))))

;(cosine '4  '4 (graph fcg-propbank::*th*))


(defmethod dot-product ((node-1 symbol) (node-2 symbol) (graph graph) &key edge-type)
  "Returns the dot product of two sparse vectors."
  (dot-product (lookup-node graph node-1) (lookup-node graph node-2) graph :edge-type edge-type))

(defmethod dot-product ((node-1 integer) (node-2 integer) (graph graph) &key edge-type)
  (let* ((vector-node-1 (node-vector node-1 graph :edge-type edge-type))
         (vector-node-2 (node-vector node-2 graph :edge-type edge-type))
         (count-vector-node-1 (hash-table-count vector-node-1))
         (count-vector-node-2 (hash-table-count vector-node-2))
         (longest-vector (if (> count-vector-node-1 count-vector-node-2) vector-node-1 vector-node-2))
         (shortest-vector (if (> count-vector-node-1 count-vector-node-2) vector-node-2 vector-node-1)))
    
    (loop for id being the hash-keys in shortest-vector using (hash-value v-1)
          for v-2 = (gethash id longest-vector)
          when v-2
          sum (* v-1 v-2))))

;(dot-product 'fcg-propbank::abridge.01-3  'fcg-propbank::abridge.01-3 (graph fcg-propbank::*th*))

  
(defmethod norm ((node symbol) (graph graph) &key edge-type)
  "Returns the eucledean norm of a sparse vector."
  (norm (lookup-node graph node) graph :edge-type edge-type))

(defmethod norm ((node integer) (graph graph) &key edge-type)
  "Returns the eucledean norm of a sparse vector."
  (sqrt (sum-square-sarray-row (gethash edge-type (matrix graph)) node)))

;; (dot-product 0 8 (graph fcg-propbank::*th*))

(defmethod node-vector ((node symbol) (graph graph)  &key edge-type)
  "Returns the sparse vector for a given node."
  (node-vector (lookup-node graph node) graph :edge-type edge-type))

(defmethod node-vector ((node integer) (graph graph)  &key edge-type)
  "Returns the sparse vector for a given node."
    (gethash node (matrix (gethash edge-type (matrix graph)))))

;; (node-vector 'fcg-propbank::send.01-20 (graph *th*) :edge-type nil)

(defun closest-nodes (node graph &key edge-type)
  (loop for other-node being the hash-keys in (nodes graph)
        for cosine = (cosine node other-node graph :edge-type edge-type)
        if  (> cosine 0)
        collect (cons other-node cosine) into nodes-with-sim
        finally (return (sort nodes-with-sim #'> :key #'cdr))))

(defun closest-nodes-unweighted (node graph &key edge-type)
  (loop for other-node being the hash-keys in (nodes graph)
        for cosine = (unweighted-cosine node other-node graph :edge-type edge-type)
        if  (> cosine 0)
        collect (cons other-node cosine) into nodes-with-sim
        finally (return (sort nodes-with-sim #'> :key #'cdr))))

(defun typical-cxns-for-lexical-category (lexical-category graph)
    (loop with node-vector = (node-vector lexical-category graph :edge-type 'fcg-propbank::lex-gram)
          for gram-cat being the hash-keys in node-vector using (hash-value freq)
          collect (cons (lookup-node graph gram-cat)  freq) into cxns
          finally (return (sort cxns #'> :key #'cdr))))



;; (defparameter *th* (type-hierarchies:get-type-hierarchy fcg-propbank::*restored-300-grammar*))
;; (pprint (closest-nodes 'fcg-propbank::EXPLAIN\(V\)-73 (graph *th*) :edge-type 'fcg-propbank::lex-gram))



(defmethod unweighted-cosine ((node-1 symbol) (node-2 symbol) (graph graph) &key edge-type)
  "Returns the cosine between two sparse vectors."
  (unweighted-cosine (lookup-node graph node-1) (lookup-node graph node-2) graph :edge-type edge-type))

(defmethod unweighted-cosine ((node-1 integer) (node-2 integer) (graph graph) &key edge-type)
  "Returns the cosine between two sparse vectors."
  (let ((vector-node-1 (node-vector node-1 graph :edge-type edge-type))
        (vector-node-2 (node-vector node-2 graph :edge-type edge-type)))
    (if (and (hash-table-p vector-node-1) (hash-table-p vector-node-2))
      (let* ((nr-of-neighbours-node-1 (loop for neighbour being the hash-keys in vector-node-1
                                            using (hash-value freq)
                                            if (> freq 0)
                                            count neighbour))
             (nr-of-neighbours-node-2 (loop for neighbour being the hash-keys in vector-node-2
                                            using (hash-value freq)
                                            if (> freq 0)
                                            count neighbour))
             (longest-vector (if (> nr-of-neighbours-node-1 nr-of-neighbours-node-2) vector-node-1 vector-node-2))
             (shortest-vector (if (> nr-of-neighbours-node-1 nr-of-neighbours-node-2) vector-node-2 vector-node-1))
             (denominator (sqrt (* nr-of-neighbours-node-1
                                   nr-of-neighbours-node-2)))
             (nr-of-common-neighbours (loop for id being the hash-keys in shortest-vector using (hash-value v-1)
                                            for v-2 = (gethash id longest-vector)
                                            when (and v-2 (> v-2 0) (> v-1 0))
                                            count id)))
        (if (= 0 denominator)
          0
          (/ nr-of-common-neighbours
             denominator)))
      0)))

;(unweighted-cosine '4  '5 (graph fcg-propbank::*th*))


