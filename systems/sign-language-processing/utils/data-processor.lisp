(in-package :slp)

;;----------------------------------------------;;
;; tools for processing data from GeoQuery-LSFB ;;
;;----------------------------------------------;;

(defclass sign-act (blackboard)
  ((form
    :type signed-form-predicates
    :accessor form
    :initform nil
    :initarg :form)
   (meaning
    :type list
    :accessor meaning
    :initform nil
    :initarg :meaning)
   (id
    :type integer
    :accessor id
    :initform nil
    :initarg :id)))

(defclass data-processor ()
  ((counter :accessor counter :initform 0 :type number)
   (current-sign-act :accessor current-sign-act :initform nil)
   (data :accessor data :initarg :data)
   (source-file :accessor source-file :initarg :source-file :type pathname :initform nil)))

(defun xml->predicates (lsfb-node)
  "transforms the xml list representation of the lsfb form into
   a list of predicates"
  (make-instance
   'signed-form-predicates
   :predicates
   (loop for child in (xmls:node-children lsfb-node)
         collect
           (loop with counter = 0
                 for li in (xmls:node-children child)
                 for value = (first (xmls:node-children li))
                 when (or (= counter 0)
                          (find #\- value))
                   collect (read-from-string (escape-colons value))
                 when (and (/= counter 0)
                           (not (find #\- value)))
                   collect value
                 do (incf counter)))))
  
(defun json->predicates (json-list)
  "transforms the josn list representation of the lsfb form into
   a list of predicates"
  (make-instance
   'signed-form-predicates
   :predicates
   (loop for element in json-list
         collect
           (loop with counter = 0
                 for value in element
                 when (or (= counter 0)
                          (find #\- value))
                   collect (read-from-string (escape-colons value))
                 when (and (/= counter 0)
                           (not (find #\- value)))
                   collect value
                 do (incf counter)))))
                         
                     

(defun load-geoquery-corpus-xml (pathname)
  "reads in the xml corpus at pathname and returns it as an instance of data-processor class"
  (let* ((xml (read-xml pathname))
         (sign-acts
          (loop for example in (xmls:node-children xml)
                collect
                  (make-instance
                   'sign-act
                   :form
                   (xml->predicates
                    (find-lsfb example))
                   :meaning
                   (geo-prolog-to-predicates
                    (find-mrl-in-xml-example example))
                   :id
                   (find-id example)))))
    (make-instance 'data-processor
                   :data sign-acts
                   :source-file pathname)))

(defun load-geoquery-corpus-jsonl (pathname)
  "reads in the json corpus at pathname and returns it as an instance of data-processor class"
  (let* ((data (jsonl->list-of-json-alists pathname))
         (sign-acts
          (loop for example in data
                collect
                  (make-instance
                   'sign-act
                   :form
                   (json->predicates
                    (cdr (assoc :lsfb example)))
                   :meaning
                   (geo-prolog-to-predicates
                    (cdr (assoc :geo-prolog example)))
                   :id
                   (parse-integer
                    (cdr (assoc :id example)))))))
    (make-instance 'data-processor
                   :data sign-acts
                   :source-file pathname)))
    