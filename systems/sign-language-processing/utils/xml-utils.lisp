(in-package :slp)

(defun read-xml (pathname)
  "Reads in the xml file at pathname and parses it into an xmls-object"
  (with-open-file
      (stream
       pathname
       :external-format :utf-8
       :element-type 'cl:character) 
    (xmls::parse stream)))

(defun find-elan-ref (xmls-node)
  "finds the elan-ref of an xmls-node in the xml dataset file"
  (loop with elan-id = nil
        for line in (xmls::node-children xmls-node)
        do (loop for attr in (xmls::node-attrs line)
                 when
                   (string=
                    (first attr)
                    "elan-ref")
                   do
                     (setf
                      elan-id
                      (second attr))
                     ;; when an elan-ref is found, end embedded loop early
                     (return))
        ;; if an elan-id was found, end the larger loop as well
        when elan-id
          do (return elan-id)))

(defun find-lsfb (xmls-node)
  "finds the lsfb form of an xmls-node in the xml dataset file"
  (loop with lsfb = nil
        for line in (xmls::node-children xmls-node)
        do (loop for attr in (xmls::node-attrs line)
                 when (string=
                         (second attr)
                         "lsfb")
                   do (setf
                       lsfb
                       (first
                        (xmls::node-children
                         line)))
                      (return))
        when lsfb
          do (return lsfb)))

(defun find-id (xmls-node)
  "finds the id of an xmls-node in the xml dataset file and returns it as an integer"
  (parse-integer
   (loop for attr in (xmls:node-attrs xmls-node)
         do (when
                (string=
                 (first attr)
                 "id")
              (return
               (second attr))))))

(defun find-type-nr (xmls-node)
  "finds the type-nr of the xmls-node and returns it as an integer"
  (parse-integer
   (loop for attr in (xmls:node-attrs xmls-node)
        do (when (string= (first attr) "type")
             (return (second attr))))))

(defun find-example-by-id (xmls-dataset id)
  "find the example in xmls-dataset that has the specified id (integer)"
  (loop for xml-line in (xmls::node-children xmls-dataset)
        when (= id
                (find-id xml-line))
          do
            (return xml-line)))

(defun find-translation-in-xml-example (example &key (language "en"))
  "finds and returns the translation in the provided language from the example"
  (loop for child in (xmls:node-children example)
        for node-name = (xmls:node-name child)
        for attribute =
          (second
           (first
            (xmls:node-attrs child)))
        when
          (and
           (string=
            node-name
            "nl")
           (string=
            attribute
            language))
          do
            (return
             (first
              (xmls:node-children child)))))

(defun find-mrl-in-xml-example (example &key (mrl "geo-prolog"))
  "finds and returns the meaning representation in the provided language from the example"
  (loop for child in (xmls:node-children example)
        for node-name = (xmls:node-name child)
        for attribute =
          (second
           (first
            (xmls:node-attrs child)))
        when
          (and
           (string=
            node-name
            "mrl")
           (string=
            attribute
            mrl))
          do
            (return
             (first
              (xmls:node-children child)))))


(defun string->xml-nodes (predicate-string)
  "transforms predicate-string into a list of
   xmls-nodes where each node represents one predicate"
       (let ((predicates
              (string->predicates
               predicate-string)))
         (loop for predicate in predicates
               collect
                 (xmls:make-node
                  :name
                  (downcase
                   (format
                    nil
                    "~a"
                    (first predicate)))
                  :attrs
                  `(("arg1"
                     ,(downcase
                       (format
                        nil
                        "~a"
                        (second predicate))))
                    ("arg2"
                     ,(downcase
                       (format
                        nil
                        "~a"
                        (third predicate)))))))))
         
       
              