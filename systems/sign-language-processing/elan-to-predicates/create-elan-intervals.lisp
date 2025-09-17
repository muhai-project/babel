(in-package :slp)

;;---------------------------------------------;;
;; + A class for representing ELAN-intervals + ;;
;;---------------------------------------------;;

(defclass elan-interval ()
  ((elan-id
    :documentation "The elan-id to refer to this elan-interval"
    :initarg :elan-id
    :accessor elan-id
    :initform nil
    :type symbol)
   (fcg-id
    :documentation "The fcg-id to refer to this elan-interval"
    :initarg :fcg-id
    :accessor fcg-id
    :initform nil
    :type symbol)
   (value
    :documentation "The value for the elan-interval"
    :initarg :value
    :accessor value
    :initform nil
    :type symbol)
  (begin
    :documentation "The begin of the elan-interval"
    :initarg :begin
    :accessor begin
    :initform nil
    :type cons)
  (end
    :documentation "The end of the elan-interval"
    :initarg :end
    :accessor end
    :initform nil
    :type cons)
  (interval-type
    :documentation "The type of the elan-interval"
    :initarg :interval-type
    :accessor interval-type
    :initform nil
    :type symbol)
  (hamnosys
   :documentation "The hamnosys of the elan-interval"
   :initarg :hamnosys
   :accessor hamnosys
   :initform nil
   :type string)))


;;-----------------------------------------------;;
;; + Functions for initializing elan-intervals + ;;
;;-----------------------------------------------;;

(defun retrieve-time-points (annotation-document-xmls)
  "Retrieves the time order constraint nodes from the xmls node 'annotation-document-xmls' and returns
   a hash-table with the elan-id of each time-point as the key and the actual
   time-point as the value"
  (loop with time-points-hash = (make-hash-table)
        ;;find child-tag-xmls of the root annotation-document-xmls node that has the name "TIME_ORDER"
        for child-tag-xmls in (xmls:node-children annotation-document-xmls)
        do (when (string=
                  (xmls:node-name child-tag-xmls)
                  "TIME_ORDER")
             ;;extract the elan-id and value of each child tag and add it to time-points-hash
             (loop for time-point in (xmls:node-children child-tag-xmls)
                   for elan-id = (read-from-string
                                  (upcase
                                   (second
                                    (second
                                     (xmls:node-attrs
                                      time-point)))))
                   for value = (parse-integer
                                (second
                                 (first
                                  (xmls:node-attrs
                                   time-point))))
                   do (setf (gethash elan-id time-points-hash)
                            value))
             ;return the hash-table (keys = elan-ids, values = time-points)
             (return time-points-hash))))

(defun retrieve-intervals (tier-xmls time-points &key (type 'left-hand-articulation))
  "uses the extracted time-points (hash table) to initialize an elan-interval for each
   interval in the provided tier-xmls-node. The 'type' keyword specifies the types of
   elan-intervals that are initialized"
  (loop with intervals = '()
        
        ;; loop over all annotations on tier-xmls
        for annotation-xmls in (xmls:node-children tier-xmls)
        for alignable-annotation-xmls = (first
                                         (xmls:node-children annotation-xmls))
        
        ;; extract id used to refer to the start point of the interval
        for start-reference-id = (read-from-string
                                  (upcase
                                   (second
                                    (second
                                     (xmls:node-attrs
                                      alignable-annotation-xmls)))))
        
        ;; extract id used to refer to the start point of the interval
        for end-reference-id = (read-from-string
                                (upcase
                                 (second
                                  (first
                                   (xmls:node-attrs
                                    alignable-annotation-xmls)))))
        
        ;; extract the actual annotation-value and clean it
        for value = (read-from-string
                     (upcase
                      (replace-round-brackets
                       (escape-colons
                        (first
                         (xmls:node-children
                          (first
                           (xmls:node-children
                            alignable-annotation-xmls))))))))
        
        ;; extract the id that elan uses for the annotation
        for elan-id = (loop for attribute in (xmls:node-attrs alignable-annotation-xmls)
                            when (string=
                                  (first attribute)
                                  "ANNOTATION_ID")
                              do (return (read-from-string (upcase (second attribute)))))
        
        ;; initialize the elan-interval and push it to list of all intervals
        do (push
            (make-instance 'elan-interval
                           :begin (cons
                                   start-reference-id
                                   (gethash start-reference-id time-points))
                           :end (cons
                                 end-reference-id
                                 (gethash end-reference-id time-points)) 
                           :elan-id elan-id
                           :fcg-id (make-const (format nil "~a" value))
                           :interval-type type
                           :value value)
            intervals)
        ;; return the list containing all intervals extracted for the tier
        finally
          (return (reverse intervals))))

(defun add-hamnosys (hamnosys-tier elan-intervals)
  "extracts all hamnosys representations from hamnosys-tier and adds them to their
   corresponding interval in the list of elan-intervals"
  ;; loop over all annotations on the hamnosys-tier
  (loop for annotation in (xmls:node-children hamnosys-tier)
        ;; extract the reference to the annotation on which the hamnosys depends
        for annotation-ref = (read-from-string
                              (upcase
                               (second
                                (first
                                 (xmls:node-attrs
                                  (first
                                   (xmls:node-children annotation)))))))
        ;; extract the actual hamnosys string
        for hamnosys = (first
                        (xmls:node-children
                         (first
                          (xmls:node-children
                           (first
                            (xmls:node-children annotation))))))
        ;; find the interval in elan-intervals that has annotation-ref as its elan-id
        for ref-interval = (find-by-elan-id elan-intervals annotation-ref)
        ;; set the hamnosys of the retrieved ref-interval to the retrieved hamnosys
        do (setf (hamnosys ref-interval) hamnosys)))


