;;;; functional-programs.lisp

(in-package :clevr-evaluation)

(export '(program->rpn program->program-tree
          program-tree->alist program-tree->image))

;;;; comparing clevr program trees
(defun equal-program-node (clevr-node fcg-node)
  "Two nodes are equal when they have the same function,
   the same value inputs and the same number of children"
  (and (equalp (mkstr (function-name clevr-node))
               (mkstr (function-name fcg-node)))
       (equalp (arguments clevr-node)
               (arguments fcg-node))
       (length= (children clevr-node)
                (children fcg-node))))

(defmethod traverse-compare ((clevr-node clevr-function)
                             (fcg-node clevr-function)
                             &key test)
  "This function will recursively go down the tree and check if all nodes are
   equal. If the tree branches, all combinations are tried. The actual node equality
   testing happens while backtracking over the recursion. When the equality test succeeds
   't' is being progagated upwards. Otherwise, 'nil' will be propagated upwards."
  ;; if both nodes have children
  (if (and (children clevr-node)
           (children fcg-node))
    (cond ((and (length= (children clevr-node) 1)
                (length= (children fcg-node) 1))
           ;; if both nodes have 1 child, descent into that child
           (let ((result (traverse-compare (first (children clevr-node))
                                           (first (children fcg-node))
                                           :test test)))
             ;; when result contains t, all nodes below this level are
             ;; equal. When this is the case, test if the nodes at this
             ;; level are equal. When this is not the case, don't even
             ;; bother.
             (when result
               (funcall test clevr-node fcg-node))))
          
          ((and (length> (children clevr-node) 1)
                (length> (children fcg-node) 1))
           ;; if both nodes have multiple children, try out both combinations
           (let* ((option1 (pairlis (children clevr-node) (children fcg-node)))
                  (option2 (pairlis (children clevr-node) (reverse (children fcg-node))))
                  (solution1 (loop for (c1 . c2) in option1
                                   always (traverse-compare c1 c2 :test test)))
                  ;; only try out the second combination if the first one failed
                  (solution2 (unless solution1
                               (loop for (c1 . c2) in option2
                                     always (traverse-compare c1 c2 :test test)))))
             ;; when one of the combinations succeeded, all nodes below this
             ;; level are equal (in a certain ordering of the branches).
             ;; Test if the nodes at this level are also equal.
             ;; When this is not the case, don't even bother.
             (when (or solution1 solution2)
               (funcall test clevr-node fcg-node)))))

    ;; when the nodes have no children, you have reached leafs
    ;; test if the leafs are equal and start the backtracking
    (funcall test clevr-node fcg-node)))

(defun equal-program-tree (clevr-tree fcg-tree)
  (traverse-compare (top clevr-tree)
                    (top fcg-tree)
                    :test #'equal-program-node))

;;;; irl programs to RPN and program tree
(defun predicate->program-node (predicate linked-binding)
  "Create a CLEVR program node from a given predicate"
  (predicate->clevr-program-node predicate linked-binding (predicate-name predicate)))

(defun predicate->polish (predicate bind-statement)
  "Write a predicate in polish notation"
  (if bind-statement
    (if (eql (predicate-name predicate) 'filter)
      (list (predicate-name predicate)
            (read-from-string (downcase (first (split (mkstr (bind-statement-type bind-statement)) #\-))))
            (bind-statement-value bind-statement))
      (list (predicate-name predicate)
            (bind-statement-value bind-statement)))
    (list (predicate-name predicate))))

(define-event meaning->tree-finished (program-tree tree))

;; take the final predicate;
;; take everything after the first argument (all inputs)
;; if the input is a var, follow it and push it on the stack.
;; continue until you are at the end and the stack is empty.
(defun program->rpn+tree (irl-program)
  "Transform a predicate in reverse polish notation
   and returns a program tree"
  (let* ((target-predicate (get-target-predicate irl-program))
         (stack (list (cons target-predicate nil)))
         (program-tree (make-instance 'clevr-program))
         rpn)
    (loop while stack
          for stack-elem = (pop stack)
          for current-predicate = (car stack-elem)
          for parent-node = (cdr stack-elem)
          for in-vars = (input-vars current-predicate)
          for bind-statement = (linked-bind-statement current-predicate irl-program)
          do (let ((node (predicate->program-node
                          current-predicate
                          bind-statement)))
               (push (predicate->polish current-predicate bind-statement) rpn)
               (add-node program-tree node :parent parent-node)
               (dolist (var in-vars)
                 (when (variable-p var)
                   (let ((all-linked (all-linked-predicates current-predicate var irl-program)))
                     (dolist (p all-linked) (push (cons p node) stack)))))))
    (notify meaning->tree-finished program-tree)
    (values rpn program-tree)))

(defun program->rpn (irl-program)
  (let* ((target-predicate (get-target-predicate irl-program))
         (stack (list target-predicate))
         rpn)
    (loop while stack
          for current-predicate = (pop stack)
          for in-vars = (input-vars current-predicate)
          for bind-statement = (linked-bind-statement current-predicate irl-program)
          do (progn
               (push (predicate->polish current-predicate bind-statement) rpn)
               (dolist (var in-vars)
                 (when (variable-p var)
                   (let ((all-linked (all-linked-predicates current-predicate var irl-program)))
                     (dolist (p all-linked) (when p (push p stack))))))))
    rpn))

(defun program->tree (irl-program)
  (let* ((target-predicate (get-target-predicate irl-program))
         (stack (list (cons target-predicate nil)))
         (program-tree (make-instance 'clevr-program)))
    (loop while stack
          for stack-elem = (pop stack)
          for current-predicate = (car stack-elem)
          for parent-node = (cdr stack-elem)
          for in-vars = (input-vars current-predicate)
          do (when (member (car current-predicate) '(greater-than less-than))
               (setf in-vars (reverse in-vars)))
          do (let ((node (predicate->program-node
                          current-predicate
                          (linked-bind-statement current-predicate irl-program))))
               (add-node program-tree node :parent parent-node)
               (dolist (var in-vars)
                 (when (variable-p var)
                   (unless (eql (car current-predicate) 'segment-scene)
                     (let ((all-linked (all-linked-predicates current-predicate var irl-program)))
                       (dolist (p all-linked) (push (cons p node) stack))))))))
    (notify meaning->tree-finished program-tree)
    program-tree))

(defun program-tree->image (program-tree &key path (format "pdf") (open t))
  "Create an image from the program tree"
  (let ((out-path (or (and path (pathnamep path))
                      (make-file-name-with-time
                       (babel-pathname :directory '(".tmp") :name "" :type format)))))
    (s-dot->image (make-s-dot program-tree
                              :key (lambda (node)
                                     (format nil "~a(~{~a~^, ~})"
                                             (function-name node)
                                             (arguments node)))
                              :arrowdir "back")
                  :path out-path
                  :format format
                  :open open)
    out-path))

#|
(defun program-tree->alist (program-tree)
  (let ((node-count 0)
        (num-nodes (length (nodes program-tree))))
    (traverse program-tree (lambda (node)
                             (setf (id node) (- num-nodes node-count 1))
                             (incf node-count)))
    (traverse program-tree (lambda (node)
                             (let ((inputs (mapcar #'id (children node))))
                               (setf (inputs node)
                                     (sort inputs #'<)))))
    (let ((nodes (sort (nodes program-tree) #'< :key #'id)))
      (loop for node in nodes
            collect `((:type . ,(mkstr (function-name node)))
                      (:value--inputs . ,(mapcar #'mkstr (args node))))))))
|#

(defmethod encode-for-json ((node clevr-function))
  `((:id . ,(mkstr (id node)))
    (:function . ,(downcase (replace-char (mkstr (function-name node)) #\- #\_)))
    (:value--inputs . ,(if (null (arguments node)) '() (arguments node)))
    (:inputs . ,(if (null (children node)) '() (children node)))))

(defun tree->json (program-tree)
  "Create a json file from the program tree"
  (let ((node-count 0)
        (num-nodes (length (nodes program-tree)))
        numbered-nodes)
    (traverse program-tree
              (lambda (node)
                (push (cons node (- num-nodes node-count 1)) numbered-nodes)
                (incf node-count)))
    (traverse program-tree
              (lambda (node)
                (let ((inputs (mapcar #'(lambda (child)
                                          (rest (assoc child numbered-nodes)))
                                      (children node))))
                  (setf (children node)
                        (sort inputs #'<)))))
    (let ((nodes (mapcar #'car (sort numbered-nodes #'< :key #'cdr))))
      (loop for node in nodes
            collect (encode-for-json node)))))