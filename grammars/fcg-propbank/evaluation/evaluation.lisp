(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                      ;;
;; Evaluating propbank grammars.                        ;;
;;                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-and-evaluate-sentence (conll-sentence training-configuration)
  (format t "~%~%~a~%" (sentence-string conll-sentence))
  (multiple-value-bind (compatible-rolesets incompatible-rolesets)
      (loop for roleset in (mapcar #'frame-name (propbank-frames conll-sentence))
            if (spacy-benepar-compatible-annotation conll-sentence roleset)
              collect roleset into compatible-rolesets
            else collect roleset into incompatible-rolesets
              finally (return (values compatible-rolesets incompatible-rolesets)))
    (format t "Compatible rolesets: ~a ~%" compatible-rolesets)
    (format t "Incompatible rolesets: ~a ~%" incompatible-rolesets)

    (learn-propbank-grammar (list conll-sentence) :cxn-inventory '*temp* :fcg-configuration training-configuration)

    (loop with all-succeeded = t
          for (frame-name . result) in (comprehend-and-evaluate (list conll-sentence) *temp* :silent t :timeout nil
                                                                 :core-roles-only nil :per-frame-evaluation t)
          if (< (cdr (assoc :f1-score result)) 1.0)
            do (setf all-succeeded nil)
               (format t "Evaluation FAILED for roleset ~a!!!~%" frame-name)
          finally (return all-succeeded))))

(defun comprehend-and-evaluate (list-of-propbank-sentences cxn-inventory &key (timeout 60) (core-roles-only t)
                                                           (selected-rolesets nil)
                                                           (excluded-frames nil)
                                                           (include-word-sense t) (include-timed-out-sentences t)
                                                           (include-sentences-with-incomplete-role-constituent-mapping t)
                                                           (silent nil) (per-frame-evaluation nil))
  (let ((output-file (babel-pathname :directory '(".tmp")
                                     :name "results"
                                     :type "store")))

    (evaluate-propbank-corpus list-of-propbank-sentences cxn-inventory :output-file output-file :timeout timeout :silent silent)

    (let ((predictions (cl-store:restore (babel-pathname :directory '(".tmp")
                                                :name "results"
                                                :type "store"))))
      (if per-frame-evaluation
        (evaluate-predictions-per-frame predictions
                                        :core-roles-only core-roles-only
                                        :selected-rolesets selected-rolesets
                                        :excluded-frames excluded-frames
                                        :include-word-sense include-word-sense 
                                        :include-timed-out-sentences include-timed-out-sentences
                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)
        (evaluate-predictions predictions
                              :core-roles-only core-roles-only
                              :selected-rolesets selected-rolesets
                              :excluded-frames excluded-frames
                              :include-word-sense include-word-sense 
                              :include-timed-out-sentences include-timed-out-sentences
                              :silent silent
                              :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))))
 

(defun evaluate-propbank-corpus (list-of-propbank-sentences cxn-inventory &key (output-file nil) (timeout 60) (silent t))
  "Runs FCG comprehend on a corpus of sentences and stores the solutions to an external file."
  (let ((output-file (or output-file
                         (babel-pathname :directory '(".tmp")
                                         :name (format nil "~a~a" (multiple-value-bind (sec min hour day month year)
                                                               (decode-universal-time (get-universal-time))
                                                             (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                                                                     year month day hour min sec)) "evaluation")
                                         :type "store"))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          do (unless silent (format t "~%Sentence ~a: ~a" sentence-number (sentence-string sentence)))
          collect (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames sentence :cxn-inventory cxn-inventory :silent silent :timeout timeout))))
                         (annotation (propbank-frames sentence)))
                    (if (eql cipn 'time-out)
                      (progn (unless silent (format t " --> timed out .~%"))
                        (list sentence annotation 'time-out))
                      (let ((solution (remove-if-not #'frame-with-name (frames (extract-frames (car-resulting-cfs (cipn-car cipn)))))))
                        (unless silent (format t " --> done .~%"))
                        (list sentence annotation solution))))
          into evaluation
          finally (cl-store:store evaluation output-file))))

(defun evaluate-predictions (predictions &key (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-frames nil) (include-sentences-with-incomplete-role-constituent-mapping t) (silent nil))
  "Computes precision, recall and F1 score for a given list of predictions."
  (loop for (sentence annotation solution) in predictions
        when (and (or include-timed-out-sentences
                      (not (eql solution 'time-out)))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        ;;gold standard predictions
        sum (loop for frame in annotation
                  for frame-name = (if include-word-sense
                                     (frame-name frame)
                                     (truncate-frame-name (frame-name frame)))
                  if (and (null (find (truncate-frame-name (frame-name frame)) excluded-frames :test #'equalp))
                          (or (null selected-rolesets)
                              (find frame-name selected-rolesets :test #'equalp)))
                  sum (loop for role in (frame-roles frame)
                            if core-roles-only
                            sum (if (core-role-p role)
                                  (length (indices role)) 0)
                            else sum (length (indices role))))
        into number-of-gold-standard-predictions
        ;;grammar predictions
        when (and (not (eql solution 'time-out))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (null (find (truncate-frame-name (symbol-name (frame-name predicted-frame))) excluded-frames :test #'equalp))
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp))
                            (find (truncate-frame-name frame-name) annotation
                                    :key #'(lambda (frame)
                                             (truncate-frame-name (frame-name frame)))
                                    :test #'equalp))
                  sum (+ (loop for role in (frame-elements predicted-frame)
                               if core-roles-only
                               sum (if (core-role-p role)
                                     (length (indices role)) 0)
                               else sum (length (indices role)))
                         (length (indices (frame-evoking-element predicted-frame))))) ;;FEE
        into number-of-grammar-predictions
        ;;correct predictions
        when (and (not (eql solution 'time-out))
                  (or include-sentences-with-incomplete-role-constituent-mapping
                      (loop for gold-frame in annotation
                            always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                        :selected-role-types (if core-roles-only
                                                                                               'core-only 'all)))))
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (null (find frame-name excluded-frames :test #'equalp))
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp)))
                  sum (+ (loop for predicted-frame-element in (frame-elements predicted-frame) ;;frame elements
                            for predicted-indices = (indices predicted-frame-element)
                            if core-roles-only
                            sum (if (core-role-p predicted-frame-element)
                                  (loop for index in predicted-indices
                                        when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                           annotation include-word-sense)
                                         sum 1)
                                  0)
                            else sum (loop for index in predicted-indices
                                           when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                             annotation include-word-sense)
                                           sum 1))
                         (if (correctly-predicted-fee-index-p (indices (frame-evoking-element predicted-frame)) ;;FEE
                                                              predicted-frame annotation include-word-sense)
                           (length (indices (frame-evoking-element predicted-frame)))
                           0)))
        into number-of-correct-predictions
        finally (let ((evaluation-result `((:precision . ,(compute-precision number-of-correct-predictions number-of-grammar-predictions))
                                           (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                           (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-grammar-predictions number-of-gold-standard-predictions))
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-grammar-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                  (unless silent
                    (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
                    (format t "~a" evaluation-result))
                  (return evaluation-result))))




(defun evaluate-predictions-per-frame (predictions &key (core-roles-only t) (selected-rolesets nil)
                                                   (include-word-sense t) (include-timed-out-sentences t) 
                                                   (excluded-frames nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Computes precision, recall and F1 score for a given list of predictions."

  (evaluate-predictions predictions :core-roles-only core-roles-only :selected-rolesets selected-rolesets :include-word-sense include-word-sense
                        :include-word-sense include-word-sense :include-timed-out-sentences include-timed-out-sentences
                        :excluded-frames excluded-frames :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)
  
  (let ((number-of-gold-standard-predictions-per-frame (make-hash-table))
        (number-of-grammar-predictions-per-frame (make-hash-table))
        (number-of-correct-predictions-per-frame (make-hash-table)))

    (loop for (sentence annotation solution) in predictions
          when (and solution
                    (or include-timed-out-sentences
                        (not (eql solution 'time-out)))
                    (or include-sentences-with-incomplete-role-constituent-mapping
                        (loop for gold-frame in annotation
                              always (spacy-benepar-compatible-annotation sentence (frame-name gold-frame)
                                                                          :selected-role-types (if core-roles-only

                                                                                                 'core-only 'all)))))
            do ;; count all gold standard predictions / frame
              (loop for frame in annotation
                    for frame-name-as-string = (if include-word-sense
                                                 (frame-name frame)
                                                 (truncate-frame-name (frame-name frame)))
                    when (and (null (find (truncate-frame-name (frame-name frame)) excluded-frames :test #'equalp))
                              (or (null selected-rolesets)
                                  (find frame-name-as-string selected-rolesets :test #'equalp)))
                      do
                        (let* ((frame-name (intern (upcase frame-name-as-string)))
                               (hash (gethash frame-name number-of-gold-standard-predictions-per-frame))
                               (updated-count (+ (or hash 0)
                                                 (loop for role in (frame-roles frame)
                                                       if core-roles-only
                                                         sum (if (core-role-p role)
                                                               (length (indices role)) 0)
                                                       else sum (length (indices role))))))
                          (setf (gethash frame-name number-of-gold-standard-predictions-per-frame)
                                updated-count)))
              ;; count all grammar predictions / frame
              (loop for predicted-frame in solution
                    for frame-name-as-string = (if include-word-sense
                                                 (symbol-name (frame-name predicted-frame))
                                                 (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                    when (and frame-name-as-string
                              (null (find (truncate-frame-name (symbol-name (frame-name predicted-frame))) excluded-frames :test #'equalp))
                              (or (null selected-rolesets)
                                  (find frame-name-as-string selected-rolesets :test #'equalp))
                              (find (truncate-frame-name frame-name-as-string) annotation
                                    :key #'(lambda (frame)
                                             (truncate-frame-name (frame-name frame)))
                                    :test #'equalp))
                      do (let ((frame-name (intern (upcase frame-name-as-string))))
                           (setf (gethash frame-name number-of-grammar-predictions-per-frame)
                                 (+ (or (gethash frame-name number-of-grammar-predictions-per-frame) 0)
                                    (loop for role in (frame-elements predicted-frame)
                                          if core-roles-only
                                            sum (if (core-role-p role)
                                                  (length (indices role)) 0)
                                          else sum (length (indices role)))
                                    (length (indices (frame-evoking-element predicted-frame))))))) ;;FEE
              ;; count all correct predictions / frame
              (loop for predicted-frame in solution 
                    for frame-name-as-string = (if include-word-sense
                                                 (symbol-name (frame-name predicted-frame))
                                                 (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                    when (and frame-name-as-string
                              (null (find (truncate-frame-name (symbol-name (frame-name predicted-frame))) excluded-frames :test #'equalp))
                              (or (null selected-rolesets)
                                  (find frame-name-as-string selected-rolesets :test #'equalp)))
                      do (let ((frame-name (intern (upcase frame-name-as-string))))
                           (setf (gethash frame-name number-of-correct-predictions-per-frame)
                                 (+ (or (gethash frame-name number-of-correct-predictions-per-frame) 0)
                                    (loop for predicted-frame-element in (frame-elements predicted-frame) ;;frame elements
                                          for predicted-indices = (indices predicted-frame-element)
                                          if core-roles-only
                                            sum (if (core-role-p predicted-frame-element)
                                                  (loop for index in predicted-indices
                                                        when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                                          annotation include-word-sense)
                                                          sum 1)
                                                  0)
                                          else sum (loop for index in predicted-indices
                                                         when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                                           annotation include-word-sense)
                                                           sum 1))
                                    (if (correctly-predicted-fee-index-p (indices (frame-evoking-element predicted-frame)) ;;FEE
                                                                         predicted-frame annotation include-word-sense)
                                      (length (indices (frame-evoking-element predicted-frame)))
                                      0))))))

    (with-open-file (s "./evaluations-per-frame.csv"
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :direction :output)
      (format t "~%~%frame ~a P ~a R ~a F1 ~a corr. ~a pred. ~a gold~%" #\tab #\tab #\tab #\tab #\tab #\tab)
      (loop with frame-names = (loop for key being the hash-keys of number-of-gold-standard-predictions-per-frame collect key) 
            for frame-name in frame-names
            for number-of-gold-standard-predictions = (or (gethash frame-name number-of-gold-standard-predictions-per-frame) 0)
            for number-of-grammar-predictions = (or (gethash frame-name number-of-grammar-predictions-per-frame) 0)
            for number-of-correct-predictions = (or (gethash frame-name number-of-correct-predictions-per-frame) 0)
            for precision = (compute-precision number-of-correct-predictions number-of-grammar-predictions)
            for recall = (compute-recall number-of-correct-predictions number-of-gold-standard-predictions)
            for f1-score = (compute-f1-score number-of-correct-predictions number-of-grammar-predictions number-of-gold-standard-predictions)
            do ;; write frame evaluation result to the output buffer
              (format t "~a ~a ~$ ~a ~$ ~a ~$ ~a ~a ~a ~a ~a ~a~%" 
                      frame-name #\tab
                      precision #\tab
                      recall #\tab
                      f1-score #\tab
                      number-of-correct-predictions #\tab
                      number-of-grammar-predictions #\tab
                      number-of-gold-standard-predictions)
              ;; write same frame evaluation result to csv file
              (write-line (format nil "~a,~$,~$,~$,~a,~a,~a ~%"
                                  frame-name
                                  precision
                                  recall
                                  f1-score
                                  number-of-correct-predictions
                                  number-of-grammar-predictions
                                  number-of-gold-standard-predictions) s)))
    ))

      
  
(defun correctly-predicted-index-p (index predicted-frame-element predicted-frame gold-frames include-word-sense)
  "Returns t if the index form the predicted-frame occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (if include-word-sense
                                (frame-name predicted-frame)
                                (truncate-frame-name (frame-name predicted-frame))))
        (predicted-role (fe-role predicted-frame-element)))
    (loop for gold-frame in gold-frames
          for gold-frame-name = (if include-word-sense
                                  (frame-name gold-frame)
                                  (truncate-frame-name (frame-name gold-frame)))
          if (when (and (equalp gold-frame-name (symbol-name predicted-frame-name))
                        (equalp (indices (frame-evoking-element predicted-frame))
                             (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp))))
               (loop for gold-role in (find-all (symbol-name predicted-role) (frame-roles gold-frame) :key #'role-type :test #'equalp)
                     if (find index (indices gold-role))
                     return t))
          do
          (return t))))

(defun correctly-predicted-fee-index-p (indices predicted-frame gold-frames include-word-sense)
  "Returns t if the index form the frame-evoking-element occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (if include-word-sense
                                (frame-name predicted-frame)
                                (truncate-frame-name (frame-name predicted-frame)))))
    (loop for gold-frame in gold-frames
          for gold-frame-name = (if include-word-sense
                                  (frame-name gold-frame)
                                  (truncate-frame-name (frame-name gold-frame)))
          if (and (equalp gold-frame-name (symbol-name predicted-frame-name))
                  (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp)
                  (equalp indices (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp))))
          do
          (return t))))

(defmethod frame-with-name ((frame frame))
  "Return t if the frame has a non-variable name."
  (not (equalp "?" (subseq (symbol-name (frame-name frame)) 0 1))))

(defmethod core-role-p ((role propbank-frame-role))
  (unless (search "ARGM" (role-type role) :test #'equalp)
    t))
 
(defmethod core-role-p ((role frame-element))
  (unless (search "ARGM" (symbol-name (fe-role role)) :test #'equalp)
    t))


;; Precision, Recall and F1-score ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-precision (nr-of-correct-predictions total-nr-of-predictions)
  "Computes Precision."
  (if (> total-nr-of-predictions 0)
    (float (/ nr-of-correct-predictions total-nr-of-predictions))
    0.0))

(defun compute-recall (nr-of-correct-predictions nr-of-gold-standard-predictions)
  "Computes Recall."
  (if (> nr-of-gold-standard-predictions 0)
    (float (/ nr-of-correct-predictions nr-of-gold-standard-predictions))
    0.0))

(defun compute-f1-score (nr-of-correct-predictions total-nr-of-predictions nr-of-gold-standard-predictions)
  "Computes F1-Score."
  (let ((precision (compute-precision nr-of-correct-predictions total-nr-of-predictions))
        (recall (compute-recall nr-of-correct-predictions nr-of-gold-standard-predictions)))
    (if (and precision recall (> (+ precision recall) 0.0))
      (float (* 2 (/ (* precision recall) (+ precision recall))))
      0.0)))
