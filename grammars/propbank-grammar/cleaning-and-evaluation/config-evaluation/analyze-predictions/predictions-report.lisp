(ql:quickload :bordeaux-threads)
(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "analyze-predictions")
                      :name "restore-predictions-evaluate" :type "lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (get-predictions-reports "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/config-evaluation/predictions-config-eval" "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/config-evaluation/grammars-config-eval" "test-output.csv")

;; (get-predictions-reports "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/config-evaluation/analyze-predictions/exp-full-train-4000-predictions-32-comb/predictions-full-train-4000-predictions-32-comb" "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/config-evaluation/analyze-predictions/exp-full-train-4000-predictions-32-comb/grammars-full-train-4000-predictions-32-comb" "frame_prediction_data.csv")


(defun get-predictions-reports (predictions-directory-path grammars-directory-path output-file-report &key (core-roles-only nil) (include-word-sense t) (include-timed-out-sentences nil) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping nil))
   "Process all prediction files in a directory, generate evaluation reports for each file, and save the results to a specified CSV file.

This function iterates over all prediction files in the specified directory, processes each file, and generates an evaluation report based on various evaluation parameters. The results of the evaluations are then written to a specified CSV output file.

Arguments:
- predictions-directory-path (string): The path to the directory containing prediction files to be processed.
- grammars-directory-path (string): The path to the directory containing grammar files.
- output-file-report (string): The path to the output CSV file where evaluation results will be saved."

  (let ((directory (uiop:directory-files predictions-directory-path))
        (results '()))
    (dolist (file directory)
      (let ((file-path-string (namestring file))
            (predictions-number ""))
        ;; Extract the predictions number from the file name
        (cl-ppcre:register-groups-bind (config-number-string)
            ("(\\d{4})\\.store" file-path-string)
          (setq predictions-number (parse-integer config-number-string)))
        ; Evaluate predictions and save results
        (let ((predictions (restore-predictions file))
              (current-excluded-rolesets (generate-excluded-rolesets predictions-number grammars-directory-path))
              (current-learning-modes (generate-core-roles-only predictions-number grammars-directory-path)))
          (let ((evaluation-report (get-evaluation-report predictions :core-roles-only current-learning-modes :include-word-sense include-word-sense :include-timed-out-sentences include-timed-out-sentences :excluded-rolesets current-excluded-rolesets :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))
            (write-evaluation-report-to-csv predictions-number evaluation-report output-file-report)))))))

(defun write-evaluation-report-to-csv (prediction-nr evaluation-data filename)
  "Writes the evaluation data to a CSV file."
  (let* ((frame-names (sort (hash-table-keys evaluation-data) #'string<))
         (header "grammar_id,frame_name,precision,recall,f1_score,support~%")
         (existing-header nil))
    ;; Check if the header already exists
    (with-open-file (in-stream filename :direction :input
                                :if-does-not-exist nil)
      (when in-stream
        (setf existing-header (read-line in-stream nil))))
    ;; Write the data to the file
    (with-open-file (out-stream filename
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
      (unless existing-header
        (format out-stream header))
      (dolist (frame-name frame-names)
        (let ((frame-data (gethash frame-name evaluation-data)))
          (when (hash-table-p frame-data)
            (let ((precision (gethash :precision frame-data))
                  (recall (gethash :recall frame-data))
                  (f1-score (gethash :f1-score frame-data))
                  (support (gethash :support frame-data)))
              (format out-stream "~D,~A,~5,2F,~5,2F,~5,2F,~5D~%" prediction-nr frame-name precision recall f1-score support) ))))
      (format out-stream "~D,Macro average,~5,2F,~5,2F,~5,2F~%" prediction-nr
              (gethash :precision-macro-average evaluation-data)
              (gethash :recall-macro-average evaluation-data)
              (gethash :f1-macro-average evaluation-data))
      (format out-stream "~D,Weighted average,~5,2F,~5,2F,~5,2F~%" prediction-nr
              (gethash :precision-weighted-average evaluation-data)
              (gethash :recall-weighted-average evaluation-data)
              (gethash :f1-weighted-average evaluation-data))))

  (format t "Evaluation report saved to file: ~A~%" filename))

(defun print-evaluation-report (evaluation-data)
  "Prints the evaluation data in a nice format."
  (format t "Frame Name                Precision  Recall    F1 Score  Support~%")
  (format t "----------                ---------  -------   --------  -------~%")
  (maphash (lambda (frame-name frame-data)
             (when (hash-table-p frame-data)
               (let ((precision (gethash :precision frame-data))
                     (recall (gethash :recall frame-data))
                     (f1-score (gethash :f1-score frame-data))
                     (support (gethash :support frame-data)))
                 (format t "~A~24T~5,2F~33T~5,2F~42T~5,2F~51T~5D~%" frame-name precision recall f1-score support))))
           evaluation-data)
  (format t "~%")
  (format t "Averages:~%")
  (format t "Macro average:~24T~5,2F~33T~5,2F~42T~5,2F~%~%"
          (gethash :precision-macro-average evaluation-data)
          (gethash :recall-macro-average evaluation-data)
          (gethash :f1-macro-average evaluation-data))
  (format t "Weighted average:~24T~5,2F~33T~5,2F~42T~5,2F~%~%"
          (gethash :precision-weighted-average evaluation-data)
          (gethash :recall-weighted-average evaluation-data)
          (gethash :f1-weighted-average evaluation-data)))

(defun get-evaluation-report (predictions &key (core-roles-only nil) (include-word-sense t) (include-timed-out-sentences nil) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping nil) (num-threads 4))
  "Computes precision, recall, and F1 score for each role set in unique-rolesets and returns a hash table."
  (let ((unique-rolesets (unique-labels predictions :core-roles-only core-roles-only
                                                       :include-word-sense include-word-sense
                                                       :include-timed-out-sentences include-timed-out-sentences
                                                       :excluded-rolesets excluded-rolesets
                                                       :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))
        (evaluation-data (make-hash-table :test 'equal))
        (threads (list))
        (mutex (bt:make-lock)))
    
    (dolist (roleset unique-rolesets)
      (let ((thread (bt:make-thread
                     (lambda ()
                       (process-roleset roleset predictions core-roles-only (list roleset) include-word-sense include-timed-out-sentences excluded-rolesets include-sentences-with-incomplete-role-constituent-mapping evaluation-data mutex)))))
        (push thread threads)))
    
    (mapc #'bt:join-thread threads)
    
     ;; Calculate averages
    (let ((precision-macro-average (macro-average evaluation-data :precision))
          (recall-macro-average (macro-average evaluation-data :recall))
          (f1-macro-average (macro-average evaluation-data :f1-score))
          (precision-weighted-average (weighted-average evaluation-data :precision))
          (recall-weighted-average (weighted-average evaluation-data :recall))
          (f1-weighted-average (weighted-average evaluation-data :f1-score)))
      
      (setf (gethash :precision-macro-average evaluation-data) (or precision-macro-average 0))
      (setf (gethash :recall-macro-average evaluation-data) (or recall-macro-average 0))
      (setf (gethash :f1-macro-average evaluation-data) (or f1-macro-average 0))
      (setf (gethash :precision-weighted-average evaluation-data) (or precision-weighted-average 0))
      (setf (gethash :recall-weighted-average evaluation-data) (or recall-weighted-average 0))
      (setf (gethash :f1-weighted-average evaluation-data) (or f1-weighted-average 0)))
    
    evaluation-data))

(defun process-roleset (roleset predictions core-roles-only selected-rolesets include-word-sense include-timed-out-sentences excluded-rolesets include-sentences-with-incomplete-role-constituent-mapping evaluation-data mutex)
  (let ((roleset-evaluation (evaluate-predictions predictions
                                                 :core-roles-only core-roles-only
                                                 :selected-rolesets selected-rolesets
                                                 :include-word-sense include-word-sense
                                                 :include-timed-out-sentences include-timed-out-sentences
                                                 :excluded-rolesets excluded-rolesets
                                                 :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))
    (let ((frame-data (make-hash-table)))
      (dolist (pair roleset-evaluation)
        (setf (gethash (car pair) frame-data) (cdr pair)))
      ;; Add support value
      (let ((support (gethash :nr-of-gold-standard-predictions frame-data)))
        (setf (gethash :support frame-data) support))
      (let ((support (gethash :nr-of-predictions frame-data)))
        (setf (gethash :nr-of-predictions frame-data) support))
      (let ((support (gethash :nr-of-correct-predictions frame-data)))
        (setf (gethash :nr-of-correct-predictions frame-data) support))
      
      (bt:with-lock-held (mutex)
        (setf (gethash roleset evaluation-data) frame-data)))))  


(defun coerce-to-number (value)
  "Coerce value to a number. If value is NIL, return 0."
  (if value value 0))

(defun macro-average (evaluation-data metric)
  "Compute the macro-average for a given metric."
  (let ((sum 0) (count 0))
    (maphash
     (lambda (key value)
       (unless (string= key "average")
         (incf count)
         (incf sum (coerce-to-number (gethash metric value)))))
     evaluation-data)
    (if (> count 0)
        (/ sum count)
        (progn
          (format t "Warning: Division by zero in macro-average calculation.")
          0))))

(defun weighted-average (evaluation-data metric)
  "Compute the weighted-average for a given metric."
  (let ((sum 0) (total-support 0))
    (maphash
     (lambda (key value)
       (unless (string= key "average")
         (let ((support (coerce-to-number (gethash :support value))))
           (incf total-support support)
           (incf sum (* support (coerce-to-number (gethash metric value)))))))
     evaluation-data)
    (if (> total-support 0)
        (/ sum total-support)
        (progn
          (format t "Warning: Division by zero in weighted-average calculation.")
          0))))


(defun sample-average (evaluation-data metric)
  "Calculates the sample average for the specified metric."
  (let ((sum 0)
        (total-samples 0))
    (maphash (lambda (key val)
               (let ((samples (gethash :nr-of-gold-standard-predictions val)))
                 (incf sum (* (gethash metric val) samples))
                 (incf total-samples samples)))
             evaluation-data)
    (/ sum total-samples)))

(defun micro-average (evaluation-data)
  "Calculates the micro average (accuracy) for multi-label or multi-class classification with a subset of classes."
  (let ((tp 0) (fn 0) (fp 0))
    (maphash (lambda (key val)
               (incf tp (gethash :true-positives val))
               (incf fn (gethash :false-negatives val))
               (incf fp (gethash :false-positives val)))
             evaluation-data)
    (/ tp (+ tp fn fp))))

(defun hash-table-keys (hash-table)
  (loop for key being the hash-keys in hash-table
        collect key))

(defun unique-labels (predictions &key (core-roles-only nil) (include-word-sense t) (include-timed-out-sentences nil) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping nil))
  "Extracts all unique labels from the given list of predictions and annotations."
  (let ((label-table (make-hash-table :test #'equalp)))
    (loop for (sentence annotation solution) in predictions
          when (and (or include-timed-out-sentences
                        (not (eql solution 'time-out)))
                    (or include-sentences-with-incomplete-role-constituent-mapping
                        (loop for gold-frame in annotation
                              always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                          :selected-role-types (if core-roles-only
                                                                                                 'core-only 'all)))))
          do (progn
               ;; Collect labels from the annotation (gold standard)
               (loop for gold-frame in annotation
                     for frame-name = (string (frame-name gold-frame))
                     do (setf (gethash frame-name label-table) t))))
    (loop for label being the hash-keys of label-table
          collect label)))

(defun unique-labels-all (predictions &key (core-roles-only t) (include-word-sense t) (include-timed-out-sentences nil) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Extracts all unique labels from the given list of predictions and annotations."
  (let ((label-table (make-hash-table :test #'equalp)))
    (loop for (sentence annotation solution) in predictions
          when (and (or include-timed-out-sentences
                        (not (eql solution 'time-out)))
                    (or include-sentences-with-incomplete-role-constituent-mapping
                        (loop for gold-frame in annotation
                              always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                          :selected-role-types (if core-roles-only
                                                                                                 'core-only 'all)))))
          do (progn
               ;; Collect labels from the annotation (gold standard)
               (loop for gold-frame in annotation
                     for frame-name = (string (frame-name gold-frame))
                     do (setf (gethash frame-name label-table) t))
               ;; Collect labels from the solution (predictions)
               (loop for predicted-frame in solution
                     for frame-name = (if include-word-sense
                                          (symbol-name (frame-name predicted-frame))
                                          (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                     do (setf (gethash frame-name label-table) t))))
    (loop for label being the hash-keys of label-table
          collect label)))

(defun restore-predictions (filename)
  "Restore variables from a binary file containing multiple CL-STORE objects."
  (with-open-file (in-stream filename :element-type '(unsigned-byte 8))
    (let ((objects '())
          (done nil))
      (loop until done
            do (let ((object (ignore-errors (cl-store:restore in-stream))))
                 (if object
                     (push object objects)
                     (setf done t))))
      (nreverse (apply #'append objects)))))
