;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; The code in this file extends the graph-utils package with a new       ;;
;; type of graph, which is both undirected and typed. This is the type    ;;
;; of graph that is used to represent categorial networks in FCG.         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :graph-utils)

(declaim (optimize (speed 3) (space 2)))

(export '(undirected-typed-graph make-undirected-typed-graph
          set-edge-weight incf-edge-weight decf-edge-weight))

(defclass undirected-typed-graph (graph)
  ((matrix :accessor matrix :initarg :matrix
           :initform (make-hash-table :test 'eql))
   (edge-type-comparator :accessor edge-type-comparator
                         :initarg :edge-type-comparator :initform 'eql)
   (indices :accessor indices :initarg :indices
            :initform (make-hash-table :test 'equalp))))

(defgeneric undirected-typed-graph? (thing)
  (:documentation "undirected typed graph predicate")
  (:method ((thing undirected-typed-graph)) t)
  (:method (thing) nil))

(defmethod print-object ((graph undirected-typed-graph) stream)
  "Print an undirected-typed-graph"
  (print-unreadable-object (graph stream :type t)
    (format stream "~A edge types (~A vertices & ~A edges)"
            (hash-table-count (matrix graph))
	    (node-count graph)
            (edges graph))))

(defmethod edge-types ((graph undirected-typed-graph))
  (loop for et being the hash-keys in (matrix graph) collecting et))

(defmethod add-edge-type ((graph undirected-typed-graph) edge-type)
  (or (gethash edge-type (matrix graph))
      (setf (gethash edge-type (matrix graph))
            (make-sparse-array (list (+ 1 (last-id graph)) (+ 1 (last-id graph)))
                               ;; last-id instead of node-count: some nodes might already have been
                               ;; deleted, but the remaining id's should not be out of bounds!!
                               :adjustable t
                               :element-type 'number
                               :initial-element 0))))

#+sbcl
(defmethod add-edge-index ((graph undirected-typed-graph) edge-type index-type unique? ordered? ordering-fn)
  (add-edge-type graph edge-type)
  (case index-type
    (:sp
     (cond ((and ordered? unique?)
            (setf (gethash (cons edge-type :weight) (indices graph))
                  (make-index :type 'unique-ordered-index
                              :key-equality-fn (lambda (t1 t2)
                                                 (and (funcall (comparator graph)
                                                               (subject t1) (subject t2))
                                                      (eql (predicate t1) (predicate t2))))
                              :value-equality-fn (comparator graph)
                              :ordering-fn ordering-fn
                              :edge-type edge-type)))
           ((and (null ordered?) (null unique?))
            (setf (gethash (cons edge-type :weight) (indices graph))
                  (make-index :type 'index
                              :key-equality-fn (comparator graph)
                              :value-equality-fn (comparator graph)
                              :edge-type edge-type)))))
    (otherwise
     (error "Only these index types are available: :sp :spw "))))

(defun make-undirected-typed-graph (&key (node-comparator 'equal) (saturation-point 0)
                                         (edge-type-comparator 'eql) initial-edge-types)
  "Create a new undirected-typed-graph object.
   You are responsible for making sure that
   node-comparator is a valid hash table test."
  (let ((g (make-instance 'undirected-typed-graph
                          :comparator node-comparator
                          :edge-type-comparator edge-type-comparator
                          :s-point saturation-point
                          :matrix (make-hash-table :test edge-type-comparator)
                          :nodes (make-hash-table :test node-comparator))))
    (dolist (e initial-edge-types)
      (add-edge-type g e))
    g))

(defmethod copy-graph ((graph undirected-typed-graph))
  "Make a deep copy of a graph."
  (let ((new-graph (make-instance 'undirected-typed-graph
                                  :comparator (comparator graph)
                                  :edge-type-comparator (edge-type-comparator graph)
                                  :matrix (make-hash-table :test (edge-type-comparator graph))
                                  :nodes (make-hash-table :test (comparator graph))
                                  :edges (edges graph)
                                  :id (last-id graph))))              
    (maphash #'(lambda (k v) (setf (gethash k (nodes new-graph)) v)) (nodes graph))
    (maphash #'(lambda (k v) (setf (gethash k (ids new-graph)) v)) (ids graph))
    (maphash #'(lambda (k v) (setf (gethash k (node-caps new-graph)) v)) (node-caps graph))
    (maphash #'(lambda (k v) (setf (gethash k (degree-table new-graph)) v)) (degree-table graph))
    (maphash #'(lambda (k v) (setf (gethash k (matrix new-graph))
                                   (let ((sarray (make-sparse-array
                                                  (list (row-count v)
                                                        (col-count v))
                                                  :adjustable t
                                                  :element-type 'number
                                                  :initial-element 0)))
                                     (fast-map-sarray #'(lambda (i j w)
                                                          (setf (saref sarray i j) w))
                                                      v)
                                     sarray)))
             (matrix graph))
    new-graph))

(defmethod add-node ((graph undirected-typed-graph) value &key capacity)
  "Add a node to the graph."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
        (maphash (lambda (etype matrix)
                   (declare (ignore etype))
                   (incf-sarray-dimensions matrix))
                 (matrix graph))
        (when capacity
          (setf (gethash id (node-caps graph)) capacity))
        (setf (gethash id (degree-table graph)) 0
              (gethash value (nodes graph)) id
              (gethash id (ids graph)) value)
        id)))
               
(defmethod neighbors ((graph undirected-typed-graph) (node integer)
                      &key edge-type (return-ids? t))
  "Return a list of cons-cells of the neighbors of the node.
   The car is the edge type and the cdr is the id of the neighbor."
  (let ((neighbors nil))
    (flet ((find-neighbors (matrix etype)
             (map-sarray-col (lambda (row-id value)
                               (when (>= value 0)
                                 (push (cons etype row-id) neighbors)))
                             matrix node)))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash (lambda (etype matrix)
                     (find-neighbors matrix etype))
                   (matrix graph)))
      (if return-ids?
          (nreverse neighbors)
          (mapcar (lambda (pair)
                    (lookup-node graph (cdr pair)))
                  (nreverse neighbors))))))

(defmethod neighbors ((graph undirected-typed-graph) node &key edge-type (return-ids? t))
  "Return a list of ids for this node's neighbors."
  (neighbors graph (gethash node (nodes graph))
             :edge-type edge-type
             :return-ids? return-ids?))


(defmethod edge-exists? ((graph undirected-typed-graph) (n1 integer) (n2 integer)
                         &key edge-type)
  "Is there an edge between n1 and n2 of type edge-type?"
  (let ((matrix (gethash edge-type (matrix graph))))
    (handler-case
        (when (and (sparse-array? matrix)
                   (numberp (saref matrix n1 n2)))
          (saref matrix n1 n2))
      (error (c)
        (ignore-errors
          (dbg "Problem with edge (~A,~A)->~A: ~A" n1 n2 (saref matrix n1 n2) c))
        nil))))

(defmethod edge-exists? ((graph undirected-typed-graph) n1 n2 &key edge-type)
  "Is there an edge between n1 and n2 of type edge-type?"
  (let ((node1 (lookup-node graph n1))
        (node2 (lookup-node graph n2)))
    (when (and node1 node2)
      (edge-exists? graph node1 node2 :edge-type edge-type))))

(defmethod add-edge ((graph undirected-typed-graph) (n1 integer) (n2 integer) &key
                     (weight 1) edge-type)
  "Add an edge between n1 and n2 of type edge-type."
  (unless (= n1 n2)
    (let ((matrix (gethash edge-type (matrix graph))))
      (unless (sparse-array? matrix)
        (setq matrix (add-edge-type graph edge-type)))
      (unless (> (saref matrix n1 n2) 0)
        (incf (gethash n1 (degree-table graph)))
        (incf (gethash n2 (degree-table graph)))
        (incf (edges graph)))
      (setf (saref matrix n1 n2) weight)
      (setf (saref matrix n2 n1) weight)
      (list n1 n2 edge-type))))

(defmethod add-edge ((graph undirected-typed-graph) n1 n2 &key (weight 1) edge-type)
  "Add an edge between n1 and n2 of type edge-type."
  (let ((node1 (or (lookup-node graph n1) (add-node graph n1)))
        (node2 (or (lookup-node graph n2) (add-node graph n2))))
    (when (and node1 node2)
      (add-edge graph
                node1
                node2
                :edge-type edge-type
                :weight weight))))

(defmethod delete-edge ((graph undirected-typed-graph) (n1 integer) (n2 integer)
                        &optional edge-type)
  "Remove an edge from the graph."
  (unless (= n1 n2)
    (let ((matrix (gethash edge-type (matrix graph))))
      (when (sparse-array? matrix)
        (when (> (saref matrix n1 n2) 0)
          (decf (gethash n1 (degree-table graph)))
          (decf (gethash n2 (degree-table graph)))
          (decf (edges graph))
          (setf (saref matrix n1 n2) 0)
          (setf (saref matrix n2 n1) 0))))))

(defmethod delete-edge ((graph undirected-typed-graph) n1 n2 &optional edge-type)
  (let ((node1 (or (lookup-node graph n1) (add-node graph n1)))
        (node2 (or (lookup-node graph n2) (add-node graph n2))))
    (when (and node1 node2)
      (delete-edge graph node1 node2 edge-type))))

(defmethod list-edges ((graph undirected-typed-graph) &key nodes-as-ids edge-type)
  "Return all edges as pairs of nodes."
  (let ((r nil))
    (flet ((map-it (matrix etype)
             (when matrix
               (fast-map-sarray #'(lambda (n1 n2 w)
                                    (declare (ignore w))
                                    (push (if nodes-as-ids
                                              (list n1 n2 etype)
                                              (list (gethash n1 (ids graph))
                                                    (gethash n2 (ids graph))
                                                    etype))
                                          r))
                                matrix))))
      (if edge-type
          (map-it (gethash edge-type (matrix graph)) edge-type)
          (maphash #'(lambda (etype matrix)
                       (map-it matrix etype))
                   (matrix graph)))
    (nreverse r))))

(defmethod set-edge-weight ((graph undirected-typed-graph) (n1 integer) (n2 integer) weight
                            &key edge-type)
  (let ((matrix (gethash edge-type (matrix graph))))
    (setf (saref matrix n1 n2) weight)
    (setf (saref matrix n2 n1) weight)))

(defmethod set-edge-weight ((graph undirected-typed-graph) n1 n2 weight &key edge-type)
  (set-edge-weight graph
                   (lookup-node graph n1)
                   (lookup-node graph n2)
                   weight :edge-type edge-type))

(defmethod edge-weight ((graph undirected-typed-graph) (n1 integer) (n2 integer)
                        &optional edge-type)
  (let ((matrix (gethash edge-type (matrix graph))))
    (saref matrix n1 n2)))

(defmethod edge-weight ((graph undirected-typed-graph) n1 n2 &optional edge-type)
  (edge-weight graph
               (lookup-node graph n1)
               (lookup-node graph n2)
               edge-type))

(defmethod incf-edge-weight ((graph undirected-typed-graph) (n1 integer) (n2 integer)
                             &key edge-type (delta 1))
  (let ((matrix (gethash edge-type (matrix graph))))
    (incf-sarray matrix (list n1 n2) delta)
    (incf-sarray matrix (list n2 n1) delta)))

(defmethod incf-edge-weight ((graph undirected-typed-graph) n1 n2 &key edge-type delta)
  (incf-edge-weight graph
                    (gethash n1 (nodes graph))
                    (gethash n2 (nodes graph))
                    :edge-type edge-type
                    :delta delta))

(defmethod decf-edge-weight ((graph undirected-typed-graph) (n1 integer) (n2 integer)
                             &key edge-type (delta 1))
  (let ((matrix (gethash edge-type (matrix graph))))
    (decf-sarray matrix (list n1 n2) delta)
    (decf-sarray matrix (list n2 n1) delta)))

(defmethod decf-edge-weight ((graph undirected-typed-graph) n1 n2 &key edge-type delta)
  (decf-edge-weight graph
                    (gethash n1 (nodes graph))
                    (gethash n2 (nodes graph))
                    :edge-type edge-type
                    :delta delta))

(defmethod random-edge ((graph undirected-typed-graph) &optional edge-type)
  (let ((n1 nil) (n2 nil) (w 0) (edge-types (edge-types graph)))
    (loop until (> w 0) do
         (let* ((edge-type (or edge-type
                               (elt edge-types (random (length edge-types)))))
                (matrix (gethash edge-type (matrix graph))))
           (setq n1 (random (row-count matrix))
                 n2 (random (col-count matrix))
                 w (saref matrix n1 n2))))
    (list n1 n2)))

(defmethod swap-edges ((graph undirected-typed-graph) e1 e2)
  (unless (and (= 3 (length e1)) (= 3 (length e2)))
    (error "Edges must be typed in a typed graph."))
  (apply #'delete-edge (cons graph e1))
  (apply #'delete-edge (cons graph e2))
  (add-edge graph (first e1) (first e2) :edge-type (third e1))
  (add-edge graph (second e1) (second e2) :edge-type (third e2)))

(defmethod reverse-edge ((graph undirected-typed-graph) n1 n2 &optional edge-type)
  (let ((weight (edge-weight graph n1 n2 edge-type)))
    (delete-edge graph n1 n2 edge-type)
    (add-edge graph n2 n1 :edge-type edge-type :weight weight)))

(defmethod reverse-all-edges ((graph undirected-typed-graph))
  (dolist (edge (list-edges graph :nodes-as-ids t))
    (let ((weight (edge-weight graph (first edge) (second edge) (third edge))))
      (delete-edge graph (first edge) (second edge) (third edge))
      (add-edge graph (second edge) (first edge)
                :weight weight
                :edge-type (third edge))))
  graph)

(defmethod delete-node ((graph undirected-typed-graph) (id integer))
  (let ((value (lookup-node graph id)))
    (dolist (edge-type-and-neighbor (neighbors graph id))
      (delete-edge graph id (cdr edge-type-and-neighbor)
                   (car edge-type-and-neighbor)))
    (remhash id (node-caps graph))
    (remhash id (degree-table graph))
    (remhash value (nodes graph))
    (remhash id (ids graph))
    nil))

(defmethod delete-node ((graph undirected-typed-graph) value)
  (when (gethash value (nodes graph))
    (delete-node graph (gethash value (nodes graph)))))

