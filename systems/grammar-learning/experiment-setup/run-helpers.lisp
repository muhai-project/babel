(in-package :grammar-learning)

(defun extract-meanings-from-cipn (cipn)
  (extract-meanings
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun extract-forms-from-cipn (cipn)
  (extract-forms
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'gl::item-based)
    (1- (length (contributing-part cxn)))))

(defun get-strings-from-root (node)
  (gl::form-predicates-with-variables
   (extract-string
    (get-root
     (if (find 'fcg::second-merge-failed (fcg::statuses node))
       (car-first-merge-structure (cipn-car node))
       (left-pole-structure
        (car-resulting-cfs
         (cipn-car node))))))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))

(defun get-cxns-of-type (agent type)
  (let ((found-cxns (if (eql type 'all)
                      (constructions-list (grammar agent))
                      (find-all type (constructions-list (grammar agent))
                                :key #'get-cxn-type))))
    (loop for cxn in found-cxns
          when (non-zero-cxn-p cxn)
          collect cxn)))

(defun non-zero-cxn-p (cxn)
  (> (attr-val cxn :score) 0))

(defun cxn-score (cxn)
  (attr-val cxn :score))

(defun find-cxn-by-type-form-and-meaning (type form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (find-all type (constructions-list cxn-inventory) :key #'get-cxn-type)
        when (and (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn)))
        return cxn))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args th-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for th-link in th-links
        for lex-slot-lex-class = (cdr th-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (gl::lex-class ,lex-slot-lex-class))) into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun variablify-program (irl-program)
  (let* ((all-arguments
          (loop for predicate in irl-program
                if (eql (first predicate) 'bind)
                append (unless (variable-p (third predicate))
                         (list (third predicate)))
                else
                append (loop for arg in (subseq predicate 1)
                             unless (variable-p arg)
                             collect arg)))
         (unique-arguments
          (remove-duplicates all-arguments))
         (mappings
          (loop for arg in unique-arguments
                collect (cons arg (make-var arg)))))
    (loop for predicate in irl-program
          collect (loop for sym in predicate
                        if (assoc sym mappings)
                        append (list (cdr (assoc sym mappings)))
                        else append (list sym)))))

;;;; UTILS FOR RUNNING GAMES
;;;; -----------------------

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors (get-all-lisp-monitors)))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'grammar-learning-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :named-configurations strategies
    :shared-configuration nil
    :monitors monitors
    :output-dir (babel-pathname :directory '("experiments" "grammar-learning" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

;;;; UTILS FOR PLOTTING
;;;; ------------------

(defun create-graph-for-single-strategy (&key experiment-name measure-names
                                              y-axis (y1-min 0) y1-max y2-max xlabel y1-label y2-label
                                              captions open)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "clevr-grammar-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 100
    :plot-directory `("experiments" "clevr-grammar-learning" "raw-data" ,experiment-name)
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :captions captions
    :use-y-axis y-axis
    :y1-min y1-min
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open)
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions title open)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "clevr-grammar-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 500
    :captions (if captions captions experiment-names)
    :title (if title title "")
    :plot-directory '("experiments" "clevr-grammar-learning" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open)
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