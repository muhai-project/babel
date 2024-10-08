(in-package :qc)

;; PARAMS
;; queue: list of nodes to expand.
;; tree: object named tree containing all the nodes created.
;; answer: answer is the expected answer to validate the goal test.
;; tables: schema of your database.
;; KEYS
;; exclude-id: exclusion of the id attributes in the where clause.
(defun query-generator (queue tree answer &optional tables &key exclude-id)
  "This function loops on a Queue and generates different queries according to a syntactic order corresponding to the SQL language. Once the goal-test has been validated, the positive query will be returned."
  (let ((queries '()) 
        (id 1))
    (loop
        until (not queue)
        for parent = (pop queue)
        do
         (if queries
           (return-from query-generator queries))
          (if (equal (depth parent) 0)
            (progn
              (dolist (tble tables)
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (attributes tble) (length (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (mapcar #'(lambda (x) (push (name x) attributes-names)) perm)
                      (setf child-node (init-node id parent attributes-names tble))
                      (setf id (+ id 1))
                      (setf (children parent) (push child-node (children parent)))
                      (if (goal-test answer child-node)
                        (progn
                          (push (q child-node) queries)))
                      (setf queue (push-end child-node queue))))))))
            (if (equal (depth parent) 1)
              (progn
                (let ((list-of-perm '())
                      (exclude-id-attrs (remove-if #'(lambda (item) (equal (name item) "id")) (attributes (tble parent)))))
                  (if exclude-id
                    (setf list-of-perm (permutations-of-length exclude-id-attrs (length exclude-id-attrs)))
                    (setf list-of-perm (permutations-of-length (attributes (tble parent)) (length (attributes (tble parent))))))
                  (dolist (perm list-of-perm)
;get data from the query
                    (let ((result (flatten (query (concatenate 'string "SELECT " (name (first perm)) " FROM " (name (tble parent)))))))
;iterate the result value and the operators
                      (dolist (value result)
                        (dolist (operator list-of-operator)
                          (let ((child-node (where-node id parent (name (first perm)) operator value (cdr perm))))
                            (setf id (+ id 1))
                            (setf (children parent) (push child-node (children parent)))
                            (setf queue (push-end child-node queue))
                            (if (goal-test answer child-node)
                              (progn
                                (push (q child-node) queries)))))))))))
            (if (> (depth parent) 1)
              (progn
                (let ((results (flatten (query (concatenate 'string "SELECT " (name (first (attrs parent))) " FROM " (name (tble parent)))))))
                  (dolist (value results)
                    (dolist (operator list-of-operator)
                      (let ((and-child (and-node id parent (name (first (attrs parent))) operator value))
                            (or-child (or-node (+ id 1) parent (name (first (attrs parent))) operator value)))
                        (setf (children parent) (append (children parent) (list and-child or-child)))
                        (setf id (+ id 2))
                        (if (goal-test answer and-child)
                          (progn
                            (push (q and-child) queries)
                            (return-from query-generator (q and-child))))
                        (if (goal-test answer or-child)
                          (progn
                            (push (q or-child) queries)
                            (return-from query-generator (q or-child))))
                        (if (> (length (attrs parent)) 1)
                          (progn
                            (setf queue (push-end or-child queue))
                            (setf queue (push-end and-child queue)))))))))))
    queries))

       
       

;INNER JOIN CLAUSE
;;TODO
;OUTER JOIN CLAUSE
;;TODO


;Goal test
(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))

(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))