;;;; utils.lisp

(in-package :cgl)

;;;; UTILS FOR RUNNING GAMES
;;;; -----------------------

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors (get-all-lisp-monitors)))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'clevr-learning-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :named-configurations strategies
    :shared-configuration nil
    :monitors monitors
    :output-dir (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

;;;; UTILS FOR PLOTTING
;;;; ------------------

(defun create-graph-for-single-strategy (experiment-name measure-names
                                         &rest evo-plot-keyword-args)
  ;; take some arguments, but pass along the rest to raw-files->evo-plot
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "clevr-grammar-learning" "raw-data" ,experiment-name ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "clevr-grammar-learning" "graphs" ,experiment-name)
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))

#|
(defun create-graph-for-single-strategy (&key experiment-name measure-names (average-windows 100)
                                              y-axis (y1-min 0) y1-max y2-max xlabel y1-label y2-label
                                              captions open points series-numbers end key-location)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows average-windows
    :plot-directory `("experiments" "clevr-learning" "graphs" ,experiment-name)
    :error-bars '(:percentile 5 95)
    :error-bar-modes '(:filled)
    :captions captions
    :use-y-axis y-axis
    :y1-min y1-min
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :points points
    :series-numbers series-numbers
    :end end
    :open open
    :key-location key-location)
  (format t "~%Graphs have been created"))
|#

(defun create-graph-comparing-strategies (&key experiment-names measure-name (average-windows 100)
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions title open start end)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows average-windows
    :captions (if captions captions experiment-names)
    :title (if title title "")
    :plot-directory '("experiments" "clevr-learning" "graphs")
    :error-bars '(:percentile 5 95)
    :error-bar-modes '(:filled)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open
    :start start :end end)
  (format t "~%Graphs have been created"))



;; MONITOR UTILS
;; -------------

(in-package :monitors)

(export '(store-monitor))

;;;; store-monitor
(defclass store-monitor (monitor)
  ((file-name :documentation "The file name of the file to write"
              :initarg :file-name
              :reader file-name))
  (:documentation "Monitor that stores data using cl-store"))

(defmethod initialize-instance :around ((monitor store-monitor)
					&key &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method))