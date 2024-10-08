(in-package :irl)

;; ##########################################
;; duplicate detection using hashed nodes
;; ------------------------------------------

(defgeneric chunk->hash-key (thing)
  (:documentation "Computes an isomorphism invariant hash, so that all
                    chunks that are equivalent have the same hash
                    key. But not all chunks with the same hash are
                    equivalent."))

(defmethod chunk->hash-key ((chunk chunk))
  (loop for predicate in (irl-program chunk)
        sum (sxhash (first predicate))))

(defmethod chunk->hash-key ((node chunk-composer-node))
  (chunk->hash-key (chunk node)))

(defmethod add-to-hash ((node chunk-composer-node)
                        (composer chunk-composer))
  (push node (gethash (chunk->hash-key node)
                      (get-data composer 'hashed-nodes))))

(defmethod equivalent-irl-programs? ((chunk-1 chunk) (chunk-2 chunk))
  (equivalent-irl-programs? (irl-program chunk-1) (irl-program chunk-2)))

(defmethod find-node-with-equivalent-chunk ((chunk chunk) (composer chunk-composer))
  (loop for node in (gethash (chunk->hash-key chunk)
                             (get-data composer 'hashed-nodes))
        when (equivalent-irl-programs? chunk (chunk node))
        return node))

;; ##########################################
;; run-chunk-node-tests
;; ------------------------------------------

(defun run-chunk-node-tests (node composer)
  (loop for mode in (get-configuration composer :chunk-node-tests)
        always (chunk-node-test node composer mode)))

(defgeneric chunk-node-test (node composer mode)
  (:documentation "check if the new node is good. Return t or false.
   Allowed to alter the status of the node"))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :restrict-search-depth)))
  (let ((max-depth (get-configuration composer :max-search-depth)))
    (if (> (depth node) max-depth)
      (progn (push 'max-depth-reached (statuses node)) nil)
      t)))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :restrict-irl-program-length)))
  (let ((max-program-length (get-configuration composer :max-irl-program-length)))
    (if (> (length (irl-program (chunk node))) max-program-length)
      (progn (push 'max-irl-program-length-reached (statuses node)) nil)
      t)))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :check-duplicate)))
  (let ((duplicate (find-node-with-equivalent-chunk (chunk node) composer)))
    (if duplicate
      (progn (push 'duplicate (statuses node))
        (set-data node 'duplicate-node duplicate)
        nil)
      t)))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :no-primitive-occurs-more-than-once)))
  (not (duplicates? (irl-program (chunk node)) :key #'first)))
    