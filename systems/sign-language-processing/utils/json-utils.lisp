(in-package :slp)

(defun jsonl->list-of-json-alists (jsonl-path)
  "reads in a jsonl file and returns it as a list of json-alists"
  (with-open-file (in-stream jsonl-path :direction :input :external-format :utf-8
       :element-type 'cl:character)
    (loop with json-alists = nil
          for line = (read-line in-stream nil)
          while line do (push (cl-json::decode-json-from-string line) json-alists )
          finally (return (reverse json-alists))
          )))


(defun string->json-object (predicate-string)
  "transforms predicate-string into a list of
   json-objects where each object relates to one predicate"
  (let* ((predicates
          (string->predicates
           predicate-string))
         (json-object-strings
          (loop for predicate in predicates
                collect
                 (downcase
                  (format
                  nil
                  "\{\"~a\":\[\"~a\",\"~a\"\]\}"
                  (first predicate)
                  (second predicate)
                  (third predicate))))))
    (loop with output = "["
          with counter = 0
          for object in json-object-strings
          do (if (eql counter 0)
               (setf output
                     (concatenate
                      'string
                      output
                      object))
               (setf output
                     (concatenate
                      'string output
                      ","
                      object)))
            (incf counter)
          finally
            (return
             (concatenate
              'string
              output
              "]")))))