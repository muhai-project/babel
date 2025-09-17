(in-package :concept-representations)

;; ---------------------------------------------
;; + Methods for loading a dataset of entities +
;; ---------------------------------------------

(defmethod load-dataset (path)
  "Returns a list of entities by loading the dataset at the given path."
  (when (not (probe-file path))
    (error "Could not find ~a~%" path))
  
  (format t "~% Loading entities in '~a'" path)
  (let* ((raw-data (read-jsonl path))
         (entities (loaded-data->entities raw-data)))
    (format t "~% Completed loading.~%~%")
    entities))

(defmethod loaded-data->entities (hash-tables)
  "Given a list of hash-tables (each representing a single entity), create a list of entities."
  (loop for hash-table in hash-tables
        for entity = (create-entity (gethash :attributes hash-table)
                                    (gethash :description hash-table))
        collect entity))

;; helper functions
(defun parse-keyword (string)
  "Inters a string into a keyword."
  (intern (string-upcase (string-left-trim ":" string)) :keyword))

(defun read-jsonl (path)
  "Loads a .jsonl corpus into a list of hash-tables.
  
   Each line of the .jsonl file is expected to be a JSON object.
   The parser (jzon) will create a hash-table for each JSON object ."
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          for data = (when line (jzon::parse line :key-fn #'parse-keyword))
          while data
          collect data)))
