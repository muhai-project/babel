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
;;;;
;;;; File: data-monitors.lisp
;;;;
;;;; Basic monitoring mechanisms for recording and processing data
;;;;

(in-package :monitors)

;; ############################################################################

(export '(data-recorder record-value incf-value current-value
	  data-printer data-file-writer lisp-data-file-writer text-data-file-writer
          csv-data-file-writer
	  get-average-values file-name
	  data-handler sources))
 
;; ############################################################################
;; data-recorder
;; ----------------------------------------------------------------------------

(defclass data-recorder (monitor)
  ((values :documentation "A batch of series of 'values' for each interaction"
    :initform (list (list nil)) :reader get-values)
   (average-values :documentation "The average values for the batch of series"
    :initform (list (list nil)) :reader get-average-values)
   (default-value :documentation "A default value that is pushed
                                  onto 'values' if no other value was passed"
     :accessor default-value :initarg :default-value :initform 0.0) 
   (current-value :documentation "The value that was recorded or the initial value"
		  :initform 0.0
                  :reader current-value)
   (average-window :documentation "Values are averaged over the last n interactions"
		   :initarg :average-window :initform 100 :accessor average-window))
  (:documentation "Records a batch of series of values + their average"))




(defmethod initialize-instance :around ((monitor data-recorder) 
					&key id &allow-other-keys)
  (let* ((previous-monitor (get-monitor id)))
    (call-next-method)
    ;; set the current value to the default value
    (unless (eq (slot-value monitor 'default-value) :keep-previous-value)
      (setf (slot-value monitor 'current-value) (default-value monitor)))
    (when previous-monitor ;; copy the stored data
      (setf (slot-value monitor 'values) (get-values previous-monitor))
      (setf (slot-value monitor 'average-values) (get-average-values previous-monitor)))
    (subscribe-to-event id 'interaction-started)
    (subscribe-to-event id 'interaction-finished)
    (subscribe-to-event id 'series-finished)
    (subscribe-to-event id 'batch-finished)
    (subscribe-to-event id 'reset-monitors)))

(defmethod handle-interaction-started-event :before ((monitor data-recorder)
                                                     (monitor-id symbol)
						     (event (eql 'interaction-started)) 
						     (experiment t) (interaction t)
                                                     (interaction-number number))
  (declare (ignorable experiment interaction interaction-number))
  ;;set the current value of the monitor to the default value
  (with-slots (default-value current-value) monitor
    (unless (eq default-value :keep-previous-value)
      (setf current-value default-value))))

(defmethod handle-interaction-finished-event :after ((monitor data-recorder)
                                                     (monitor-id symbol)
						     (event (eql 'interaction-finished))
						     (experiment t) (interaction t)
                                                     (interaction-number number))
  (declare (ignorable experiment interaction interaction-number))
  ;; store the current-value and compute an average value
  (with-slots (values average-values current-value default-value no-default-value
                      average-window) monitor
    (push current-value (caar values))
    ;; if window is zero, no calculation of average is needed
    ;; otherwise, we would create a list of zeros in memory for no reason
    (when (not (zerop average-window)) 
      (typecase current-value
        (number    ;; current-value is a number, so we can compute the average
                   (loop for value in (remove nil (caar (get-values monitor))) ;; Jens (29/11/19)
                         for count from 1
                         sum value into sum-of-values
                         while (< count average-window)
                         finally (push (/ sum-of-values count) (caar average-values))))
        (otherwise ;;value is not a number
                   (push current-value (caar average-values)))))))

(defmethod handle-run-series-finished-event :after ((monitor data-recorder)
                                                    (monitor-id symbol)
                                                    (event (eql 'run-series-finished))
                                                    (experiment t))
  (with-slots (values current-value) monitor
    (push current-value (caar values))))

(defmethod handle-series-finished-event :before ((monitor data-recorder)
                                                 (monitor-id symbol)
						 (event (eql 'series-finished))
						 (series-number number))
  (push nil (car (slot-value monitor 'values)))
  (push nil (car (slot-value monitor 'average-values))))


(defmethod handle-reset-monitors-event ((monitor data-recorder) (monitor-id symbol)
					(event (eql 'reset-monitors)))
  (setf (car (slot-value monitor 'values)) nil)
  (push nil (car (slot-value monitor 'values)))
  (setf (car (slot-value monitor 'average-values)) nil)
  (push nil (car (slot-value monitor 'average-values))))


(defgeneric record-value (monitor value)
  (:documentation "Sets the current value of a data recorder."))

(defmethod record-value ((monitor data-recorder) (value t))
  (setf (slot-value monitor 'current-value) value))

(defgeneric incf-value (monitor value)
  (:documentation "Increases the current value of a data recorder."))

(defmethod incf-value ((monitor data-recorder) (value t))
  (incf (slot-value monitor 'current-value) value))

  
(defmethod print-object ((monitor data-recorder) stream)
  (format stream "<data-recorder ~a value: ~a>" (id monitor) (current-value monitor)))


;; ############################################################################
;; data-handler
;; ----------------------------------------------------------------------------

(defclass data-handler (monitor)
  ((sources :documentation "Pointers to the 'values or
                           'average-values slots of the associated data recorders"
	    :initform nil :reader sources)
   (monitor-ids-of-sources :documentation "The monitor ids of
                                           associated recorders, needed for activating."
			   :initform nil :reader monitor-ids-of-sources))
  (:documentation "Handles (outputs) the data of a set of data recorders"))

(defmethod initialize-instance :around ((monitor data-handler) 
					&key data-sources &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless data-sources  (error "Parameter :data-sources not provided."))
  (check-type data-sources list)
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (loop for data-source in data-sources 
     do (if (listp data-source)
	    (progn
	      (unless (and (= (length data-source) 2)
                           (equal (symbol-name (first data-source)) "AVERAGE")
                           (symbolp (second data-source)))
		(error "Wrong format of data-sources list.~
                       ~%Should be ({<monitor-id> | (average <monitor-id>)}*)"))
	      (let ((recorder (get-monitor (second data-source))))
		(unless recorder (error "Monitor ~a is not defined" (second data-source)))
		(unless (subtypep (type-of recorder) 'data-recorder)
		  (error "Monitor ~a is not of type data-recorder" data-source))
		(pushnew (second data-source) 
			 (slot-value monitor 'monitor-ids-of-sources) :test #'equal)
		(push (get-average-values recorder) (slot-value monitor 'sources))))
	    (let ((recorder (get-monitor data-source)))
	      (unless recorder (error "Monitor ~a is not defined" data-source))
	      (unless (subtypep (type-of recorder) 'data-recorder)
		(error "Monitor ~a is not of type data-recorder" data-source)) 
	      (pushnew data-source
		       (slot-value monitor 'monitor-ids-of-sources)
		       :test #'equal)
	      (push (get-values recorder) (slot-value monitor 'sources)))))
  (setf (error-occured-during-initialization monitor) nil))

(defmethod activate-monitor-method :after ((monitor data-handler) &optional active)
  "Activates all sources of that monitor whenever this one becomes active."
  (when active 
    (dolist (monitor-id (monitor-ids-of-sources monitor))
      (when (active (get-monitor monitor-id)) 
	;; deactivate the recorder and activate it then again to make sure it is 
	;; is in front of the 'active-monitors' list of 'interaction-finished etc.
	(activate-monitor-method monitor-id nil))
      (activate-monitor-method monitor-id t))))
	     

;; ############################################################################
;; data-printer
;; ----------------------------------------------------------------------------

(defclass data-printer (data-handler)
  ((format-string :initarg :format-string :accessor format-string 
		  :initform "~%no format string provided"
		  :documentation "Is passed to 'format' to print
                                  the most recent values of the sources")
   (interval :initarg :interval :accessor interval :initform 1
	     :documentation "Only every nth interaction is printed"))
  (:documentation "Prints the values of the sources using 'format-string' after each interaction"))

(defmethod initialize-instance :around ((monitor data-printer)
					&key id format-string &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (check-type format-string string)
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (subscribe-to-event id 'interaction-finished))

(defmethod handle-interaction-finished-event :after ((monitor data-printer) (monitor-id symbol)
						     (event (eql 'interaction-finished))
						     (experiment t) (interaction t)(interaction-number number))
  (declare (ignorable experiment interaction))
  (when (= (mod interaction-number (interval monitor)) 0)
    (format t (concatenate 'string "~{" (format-string monitor) "~}") 
	    (append (list interaction-number)
		    (mapcar #'caaar (reverse (sources monitor)))))))


;; ############################################################################
;; data-file-writer
;; ----------------------------------------------------------------------------

(defclass data-file-writer (data-handler)
  ((file-name
    :documentation "The file name of the file to write"
    :initarg :file-name
    :reader file-name)
   (add-experiment-to-file-name
    :documentation "When t, the file name is prefixed with the name of the experiment
                    class."
    :initform nil
    :initarg :add-experiment-to-file-name
    :accessor add-experiment-to-file-name)
   (add-time-to-file-name
    :documentation "When t, a yyyy-mm-dd-hh-mm-ss string is added to the file name"
    :initform nil :initarg :add-time-to-file-name
    :accessor add-time-to-file-name)
   (add-time-and-experiment-to-file-name
    :documentation "When t, the file name is prefixed with the name of the experiment
                    class and a yyyy-mm-dd-hh-mm-ss string."
    :initform nil
    :initarg :add-time-and-experiment-to-file-name
    :accessor add-time-and-experiment-to-file-name)
   (add-job-and-task-id-to-file-name
    :documentation "Adds the job and task id to the file name of the file written by the monitor. ONLY WORKS FOR SBCL"
    :initform nil
    :initarg :add-job-and-task-id-to-file-name
    :accessor add-job-and-task-id-to-file-name))
  (:documentation "Writes the recorded data into a file"))


(defgeneric write-data-to-file (monitor stream)
  (:documentation "Writes the sources of monitor to the stream"))

(defmethod write-data-to-file ((monitor data-file-writer) (stream t))
  (error "Please specialize the write-data-to-file method"))

(defmethod initialize-instance :around ((monitor data-file-writer)
					&key id file-name 
                                        &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless file-name (error "Parameter :file-name not provided"))
  (unless (pathnamep file-name)
    (error ":file-name parameter ~a should be a pathname" file-name))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (subscribe-to-event id 'batch-finished))


(defmethod handle-batch-finished-event ((monitor data-file-writer) (monitor-id symbol)
					(event (eql 'batch-finished))
					(experiment-class string))
  (let ((file-name
         (cond ((add-experiment-to-file-name monitor)
                (make-file-name-with-experiment-class (file-name monitor)
                                                      experiment-class))
               ((add-time-to-file-name monitor)
                (make-file-name-with-time (file-name monitor)))
               ((add-time-and-experiment-to-file-name monitor)
                (make-file-name-with-time-and-experiment-class (file-name monitor)
                                                               experiment-class))
               ((add-job-and-task-id-to-file-name monitor)
                (make-file-name-with-job-and-task-id (file-name monitor)
                                                     experiment-class))
               (t (file-name monitor)))))
    (with-open-file (file file-name :direction :output 
			  :if-exists :supersede :if-does-not-exist :create)
      (write-data-to-file monitor file)
      (format t "~%monitor ~(~a~):~%  wrote ~a"
	      (id monitor) file-name))))

;; ############################################################################
;; lisp-data-file-writer
;; ----------------------------------------------------------------------------

(defclass lisp-data-file-writer (data-file-writer)
  ()
  (:documentation "Writes the data as s-expressions to a lisp file"))

(defmethod write-data-to-file ((monitor lisp-data-file-writer) stream)
  (format stream "~%; This file was created by the lisp-data-file-writer ~a" (id monitor))
  (format stream "~%; The elements in the lists come from these source(s): ~{~a~^ ~}"
	  (monitor-ids-of-sources monitor))
  (format stream "~%; You can either evaluate this file directly and then (defparameter foo *) or")
  (format stream "~%; (with-open-file (stream ~s) (defparameter foo (read stream)))" 
	  (file-name monitor))
  (format stream "~%(~{~f~^~%  ~})" 
	  (mapcar #'(lambda (source) (loop for series in (cdar source)
                                          collect (reverse series)))
                  (sources monitor))))

;; ############################################################################
;; lisp-data-file-writer-v2
;; ----------------------------------------------------------------------------

(export '(lisp-data-file-writer-v2))

(defclass lisp-data-file-writer-v2 (data-file-writer)
  ()
  (:documentation "Writes the data as s-expressions to a lisp file"))

;;; You can either evaluate the resulting file directly and then (defparameter foo *),
;;; or use (with-open-file (stream pathname) (defparameter foo (read stream)))
;;;
(defmethod write-data-to-file ((monitor lisp-data-file-writer-v2) stream)
  (let ((*print-length* nil))
    (format stream "~%; This file was created by the lisp-data-file-writer-v2 ~a~%~%"
            (id monitor))
    (format stream "(:data-sources ~a~% :data~% (~{~f~^~%  ~}))"
            (monitor-ids-of-sources monitor)
            (mapcar #'cdar (sources monitor)))))

;; ############################################################################
;; text-data-file-writer
;; ----------------------------------------------------------------------------

(defclass text-data-file-writer (data-file-writer)
  ((column-separator :initarg :column-separator :accessor column-separator
                     :type character :initform #\,
                     :documentation "a character used to separate columns")
   (comment-string :initarg :comment-string :accessor comment-string
                   :type character :initform #\# 
		   :documentation "how to start a comment line"))
  (:documentation "Writes the data in columns to a text file"))

(defmethod initialize-instance :around ((monitor text-data-file-writer)
					&key column-separator comment-string
                                        &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (when column-separator (check-type column-separator character))
  (when comment-string (check-type comment-string character))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method))

(defmethod write-data-to-file ((monitor text-data-file-writer) stream)
  (let* ((number-of-rows (length (cadaar (sources monitor)))) 
         ;; (first (rest (first (first (sources ... which will indeed
         ;; give you the data for the first series
	 (columns (list (loop 
                           with column = (make-array number-of-rows :fill-pointer 0)
                           for i from (- number-of-rows 1) downto 0 
                           do (vector-push i column)
                           finally (return column))))
	 (column-names (list (format nil "~c interaction number" (comment-string monitor)))))
    (loop for source in (reverse (sources monitor))
       for source-number from 0 
       do (loop for series-number from 0
	     for series in (reverse (cdar source)) ; (cdr (car
             do (push (format nil "~c ~a-~a" 
                              (comment-string monitor)
                              (nth source-number (reverse (monitor-ids-of-sources monitor)))
                              series-number) column-names)
               (push (loop 
                        with series-array = (make-array (length series) :fill-pointer 0)
                        for el in series
                        do (vector-push el series-array)
                        finally (return series-array)) columns)))
    (format stream "~%~c This file was created by the~%~c text-data-file-writer ~a."
	    (comment-string monitor) (comment-string monitor) (id monitor))
    (format stream "~%~c The columns are:~%~c ~{~%~a~}" 
	    (comment-string monitor) (comment-string monitor) (reverse column-names))
    (loop 
       with reversed-columns = (reverse columns)
       for row from (- number-of-rows 1) downto 0  ; long
	do (format stream "~%") 
	 (loop for column in reversed-columns ;short
	    do (format stream "~f~c" (aref column row) (column-separator monitor))))))
  

;; ############################################################################
;; csv-data-file-writer
;; ----------------------------------------------------------------------------

(defclass csv-data-file-writer (text-data-file-writer)
  () (:documentation "Writes the data in columns to a csv file"))

(defmethod write-data-to-file ((monitor csv-data-file-writer) stream)
  (let ((number-of-rows (length (cadaar (sources monitor))))
        (number-of-columns (length (cdaar (sources monitor))))
        ;; (first (rest (first (first (sources ... which will indeed
        ;; give you the data for the first series
        (columns nil))
    
    (loop for source in (reverse (sources monitor))
          for source-number from 0 
          do (loop for series-number from 0
                   for series in (reverse (cdar source))
                   for column-name = (format nil "~a-~a" 
                                        (nth source-number
                                             (reverse (monitor-ids-of-sources monitor)))
                                        series-number)
                   for column-data = (loop 
                                      with series-array
                                      = (make-array (+ (length series) 1) :fill-pointer 0)
                                      for el in series
                                      do (vector-push el series-array)
                                      finally (return series-array))
                   do 
                   (vector-push column-name column-data)
                   (push column-data columns)))
    
    (format stream "~%~c This file was created by the~%~c csv-data-file-writer ~a."
	    (comment-string monitor) (comment-string monitor) (id monitor))
    (loop 
     with reversed-columns = (reverse columns)
     for row from number-of-rows downto 0  ; long
     do (format stream "~%") 
     (loop for column in reversed-columns ;short
           for i from 1
           if (= i number-of-columns)
           do (format stream "~f" (aref column row))
           else
           do (format stream "~f~c" (aref column row) (column-separator monitor))))))

