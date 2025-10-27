(in-package :social-network)

;; -------------------------------------------------
;; + Graphviz Visualizations of the Social Network +
;; -------------------------------------------------

;;; If you want to visualize the social network of the population, you can do so with
;;; the function #'population-network->graphviz below. This visualization can be 
;;; improved a lot but it helps to have an idea of the network structure.

(defun make-time-stamp ()
  (multiple-value-bind (seconds minutes hour day month year)
      (get-decoded-time)
    (format nil "~a-~a-~a-~a-~a-~a" year month day hour minutes seconds)))

(defun make-id-for-gv (agent)
  "Makes an ID for the graphviz output."
  (replace-char (symbol-name (id agent)) #\- #\_))

(defun population-network->graphviz (population &key
                                                (layout "circo")
                                                (directory '(".tmp"))
                                                (name (format nil "population-graph-~a" (make-time-stamp)))
                                                (use-labels? nil))
  (let ((filename (babel-pathname :directory directory
                                  :name name
                                  :type "gv")))
    
    ;; Create the Graphviz file
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      ;; Graph preamble:
      (format out "~%strict graph g1 {")
      (format out "~%     layout=\"~a\";" layout)
      (format out "~%     node [shape=~a];" (if use-labels? "circle" "point"))
      
      ;; Collect edges
      (let (edges)
        (dolist (agent population)
          (let ((agent-id (make-id-for-gv agent))
                (linked-agents (social-network agent)))
            ;; Add the node:
            (format out "~%     ~a;" agent-id)
            ;; Store edge information:
            (dolist (linked-agent linked-agents)
              (let* ((linked-id (make-id-for-gv linked-agent))
                     (link (format nil "~a -- ~a" agent-id linked-id))
                     (alt-link (format nil "~a -- ~a" linked-id agent-id)))
                ;; Avoid duplicate edges
                (unless (or (member link edges :test #'string=)
                            (member alt-link edges :test #'string=))
                  (push link edges))))))
        ;; Add edges with opacity
        (dolist (edge (reverse edges))
          (format out "~%     ~a [color=\"#00000080\"];" edge)) ; Black with 50% opacity
        ;; End the Graphviz file
        (format out "~%}")))
    filename))

(defmethod draw-graphviz-image (filename &key
                                         (layout "circo")
                                         (make-image nil) 
                                         (open-image nil))
  ;; If you do not have OmniGraffle, we can build the image ourselves. 
  ;; You can do this manually in your terminal app if you want to customize the output.
  (when make-image
    (let ((filename-string (format nil "~a~a.gv" (directory-namestring filename) (pathname-name filename)))
          (png-filename-string (format nil "~a~a.png" (directory-namestring filename) (pathname-name filename))))
      (run-prog (format nil "~a -x -Goverlap=scale -Tpng ~a > ~a" layout filename-string png-filename-string))

      (when open-image
        (cond ((equal (software-type) "Darwin")
               (run-prog "open" :args (list png-filename-string)))
              ;; Warning: not yet tested on Linux or Windows!
              ((equal (software-type) "Linux")
               (run-prog "see" :args (list png-filename-string)))
              ((equal (software-type) "Microsoft Windows")
               (run-prog "cmd"
                         :args (list "/C"
                                     (string-replace
                                      (format nil "c:~a" png-filename-string "/" "\\"))))))))))
