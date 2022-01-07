;;;; monitors.lisp

(in-package :clevr-grammar-learning)

;;;; Printing dots
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (let ((symbol-to-print (last-elt (repair-buffer experiment))))
    (cond ((= (interaction-number interaction) 1)
           (format t "~%~a" symbol-to-print))
          ((= (mod (interaction-number interaction)
                   (get-configuration experiment :dot-interval)) 0)
           (format t "~a (~a)~%" symbol-to-print (interaction-number interaction)))
         ;(wi:clear-page))
          (t (format t "~a" symbol-to-print)))))

;;;; export failed sentences and applied cxns
(define-monitor log-interactions)

(defvar *log-file* nil)

(define-event-handler (log-interactions log-parsing-finished)
  (unless *log-file*
    (setf *log-file*
          (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data")
                          :name (format nil "log-~a" (make-random-string 5))
                          :type "txt")))
  (let ((succeededp
         (when (rest (assoc 'cipn process-result-data))
           (find 'fcg::succeeded
                 (fcg::statuses
                  (rest (assoc 'cipn process-result-data)))))))
    (unless succeededp
      (let* ((interaction-nr
              (interaction-number (current-interaction (experiment agent))))
             (applied-cxns
              (when (rest (assoc 'applied-cxns process-result-data))
                (mapcar (compose #'downcase #'mkstr #'name)
                        (rest (assoc 'applied-cxns process-result-data)))))
             (utterance (utterance agent)))
        (with-open-file (stream *log-file* :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (write-line
           (format nil "~%Interaction ~a - Parsing failed - \"~a\" - ~{~a~^, ~}"
                   interaction-nr
                   utterance
                   (if applied-cxns
                     applied-cxns '(nil)))
           stream))))))


;;;; export grammar after series
(define-monitor export-learner-grammar
                :class 'store-monitor
                :file-name (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "learner-grammar" :type "store"))
  
(define-event-handler (export-learner-grammar run-series-finished)  
  (export-grammar (grammar (learner experiment))
                  (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
                  
(defun export-grammar (cxn-inventory path)
  #-ccl (cl-store:store cxn-inventory path))


(define-monitor export-type-hierarchy-to-json
                :class 'store-monitor
                :file-name (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data")
                                            :name "type-hierarchy"
                                            :type "json"))

(defun export-th-to-json (cxn-inventory path)
  (let* (
         ;; get a list of all node names
         (all-nodes
          (mapcar #'mkstr (categories (categorial-network cxn-inventory))))
         ;; get a list of all the edges
         ;; this include the edge-type
         ;; but excludes the weight
         (all-edges (links (categorial-network cxn-inventory)))
         ;; so get the weight separately
         (all-edges-with-weight
          (loop for (from to etype) in all-edges
                for w = (link-weight (categorial-network cxn-inventory) from to etype)
                collect (list (mkstr from) (mkstr to) w))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-alist-to-string
        `((nodes . ,all-nodes)
          (edges . ,all-edges-with-weight)))
       stream)
      (force-output stream))))


(defun import-th-from-json (cxn-inventory path)
  (let* ((g-data (cl-json:decode-json-from-source path))
         (all-nodes (rest (assoc :nodes g-data)))
         (all-weighted-edges (rest (assoc :edges g-data)))
         (th (make-instance 'categorial-network)))
    (loop for node in all-nodes
          for name = (intern (upcase (mkstr node)) :grammar-learning)
          do (add-category name th))
    (loop for (from to w) in all-weighted-edges
          for from-name = (intern (upcase (mkstr from)) :grammar-learning)
          for to-name = (intern (upcase (mkstr to)) :grammar-learning)
          do (add-link from-name to-name th :weight w))
    (set-categorial-network cxn-inventory th)))
         
   
(define-event-handler (export-type-hierarchy-to-json run-series-finished)
  (let ((cxn-inventory (grammar (learner experiment)))
        (path (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
    (export-th-to-json cxn-inventory path)))


;;;; export type hierarchy to image
(define-monitor export-type-hierarchy-to-image
                :class 'store-monitor
                :file-name (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-grammar-learning" "raw-data")))

(defun export-type-hierarchy-to-image (type-hierarchy path)
  (type-hierarchy->image
   type-hierarchy :render-program "fdp" :weights? t
   :path (make-pathname :directory (pathname-directory path))
   :file-name (pathname-name path)
   :format "pdf"))

(defun remove-non-connected-nodes (th)
  (let ((graph (graph th)))
    (loop for category being each hash-key of (graph-utils::nodes graph)
          when (= (graph-utils::degree graph category) 0)
          do (graph-utils::delete-node graph category))
    th))

(define-event-handler (export-type-hierarchy-to-image run-series-finished)
  (let* ((th (categorial-network (grammar (learner experiment))))
         (th-copy (copy-object th))
         (path (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
    (export-type-hierarchy-to-image
     (remove-non-connected-nodes th-copy)
     path)))

(define-monitor export-type-hierarchy-evolution-to-jsonl
                :documentation "Export a series of states of the type-hierarchy as JSONL. Used to draw dynamic evolutionary graphs."
                :class 'store-monitor
                :file-name (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data")
                                            :name "type-hierarchy-evolution"
                                            :type "jsonl"))

(define-event-handler (export-type-hierarchy-evolution-to-jsonl interaction-finished)
  (let* ((interval (if (get-configuration experiment :type-hierarchy-export-interval)
                     (get-configuration experiment :type-hierarchy-export-interval)
                     100))
         (timestep (/ (interaction-number interaction) interval)))
    (when (= (mod (interaction-number interaction) interval) 0)
      (let* ((g (graph (categorial-network (grammar (first (interacting-agents experiment))))))
             (cn (categorial-network (grammar (first (interacting-agents experiment)))))
             (path (make-file-name-with-series (file-name monitor) (series-number experiment)))
             ;; get a list of all node names
             ;; to do: get color and type attributes from graph
             (all-nodes
              (loop for node in (categories cn)
                    collect `((label . ,(mkstr node))
                              (color . "#000000")
                              (type . nil))))
             ;; get a list of all the edges
             ;; this include the edge-type
             ;; but excludes the weight
             (all-edges (links cn))
             ;; so get the weight separately
             (all-edges-with-weight
              (loop for (from to etype) in all-edges
                    for w = (link-weight cn from to etype)
                    collect`((start-node . ,(mkstr from))
                             (end-node . ,(mkstr to))
                             (score . ,w)
                             (type . nil)))) ;; to do: get type attribute from graph
             (json-hash (make-hash-table)))
        (setf (gethash 'nodes json-hash) all-nodes)
        (setf (gethash 'edges json-hash) all-edges-with-weight)

        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output
                                :if-exists (if (= 1 timestep) :supersede :append)
                                :if-does-not-exist :create)
          (write-line
           (cl-json:encode-json-to-string
            `((time-step . ,timestep)
              (interaction-number . ,(interaction-number interaction))
              (graph . ,json-hash)))
           stream)
          (force-output stream))))))
#|
(defparameter *my-hash* (make-hash-table))
(setf (gethash 'one-entry *my-hash*) "one")
(setf (gethash 'another-entry *my-hash*) 2/4)
(cl-json:encode-json-to-string *my-hash*)

 {"label":"MATERIAL","color":"#000000","type":"lexical"}
(cl-json:encode-json-alist-to-string `((time-step . 1)
            (graph . ((nodes . (((label . "bla")
                                (color . "#000000")
                                (type . "default"))))
                      (edges . "bla")))))
|#

(defun get-all-export-monitors ()
  '("export-type-hierarchy-to-image"
    "export-type-hierarchy-to-json"
    ;"export-type-hierarchy-evolution-to-jsonl"
    "export-learner-grammar"))
  
