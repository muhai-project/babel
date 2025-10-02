(in-package :ofef-parser)
;;(ql:quickload :fcg)

(export 'ofef-parser)

(defun hashtable-keys->json (ht)
  "Convert a hash table to a JSON array of its keys.
Keys are lowercased strings (drop leading colon)."
  (with-output-to-string (out)
    (write-char #\[ out)
    (let ((first t))
      (maphash
       (lambda (key val)
         (declare (ignore val))
         (unless first (write-string ", " out))
         (setf first nil)
         (format out "\"~A\"" (stringify-key key)))
       ht))
    (write-char #\] out)))


(defun alist->json-list (alist)
  "Convert an ALIST to a JSON array-of-arrays string.
Each entry (key . values) is turned into a sublist [key, v1, v2, ...].
Keys are lowercased strings (colon removed).
Values are lowercased strings, colon preserved if keyword."
  (with-output-to-string (out)
    (write-char #\[ out)
    (let ((first t))
      (dolist (entry alist)
        (unless first (write-string ", " out))
        (setf first nil)
        (destructuring-bind (key . values) entry
          (write-char #\[ out)
          ;; print key
          (format out "\"~A\"" (stringify-key key))
          ;; print values (could be atom or list)
          (dolist (v (if (listp values) values (list values)))
            (write-string ", " out)
            (format out "\"~A\"" (stringify-value v)))
          (write-char #\] out))))
    (write-char #\] out)))

(defun alist->json (alist &optional (wrap t))
  (cond
    ;; Single atom
    ((atom alist)
     (stringify-value alist))
    ;; Single-element list containing atom
    ((and (listp alist) (= (length alist) 1) (atom (car alist)))
     (stringify-value (car alist)))
    ;; Regular alist
    (t
     (with-output-to-string (out)
       (when wrap (write-char #\{ out))
       (let ((first t))
         (dolist (entry alist)
           (unless first (write-string ", " out))
           (setf first nil)
           (if (and (consp entry) (consp (cdr entry)))
               (destructuring-bind (key . values) entry
                 ;; print key
                 (format out "\"~A\": " (stringify-key key))
                 ;; decide how to render values
                 (cond
                   ((and (listp values) (listp (car values)))
                    (write-string (alist->json values t) out))
                   ((listp values)
                    (format out "[~{ \"~A\" ~^, ~}]"
                            (mapcar #'stringify-value values)))
                   (t
                    (format out "\"~A\"" (stringify-value values)))))
               ;; If entry is not a key-value pair, just stringify
               (write-string (stringify-value entry) out))))
       (when wrap (write-char #\} out))))))

(defun stringify-key (key)
  "Convert key to lowercase string (drop leading colon)."
  (string-downcase (symbol-name key)))

(defun stringify-value (val)
  "Convert value to lowercase string, preserving colon if keyword."
  (cond
    ((stringp val) (string-downcase val))
    ((keywordp val) (concatenate 'string ":" (string-downcase (symbol-name val))))
    (t (string-downcase (princ-to-string val)))))

(defun links->json (alist-of-triples)
  "Convert a list of triples into a JSON list of unique 2-element links.
Each triple is (a b something). Links are undirected (order ignored).
Duplicates are removed. Symbols are downcased, colons preserved."
  (let ((seen (make-hash-table :test #'equal)))
    ;; Collect unique links
    (dolist (triple alist-of-triples)
      (destructuring-bind (a b &rest ignore) triple
        (declare (ignore ignore))
        (let* ((sa (stringify-value a))
               (sb (stringify-value b))
               ;; canonical order: sort the two nodes
               (pair (if (string< sa sb)
                         (list sa sb)
                         (list sb sa))))
          (setf (gethash pair seen) t))))
    ;; Render as JSON
    (with-output-to-string (out)
      (write-char #\[ out)
      (let ((first t))
        (maphash
         (lambda (pair _)
           (unless first (write-string ", " out))
           (setf first nil)
           (destructuring-bind (x y) pair
             (format out "[\"~A\", \"~A\"]" x y)))
         seen))
      (write-char #\] out))))



(defun hashtable->json (ht)
  "Convert a hash table HT to a JSON string.
Keys are lowercase strings (drop leading colon).
Values are lowercase strings, preserving colon if keyword.
If a value is a list, it is represented as a JSON array of strings.
Omit any key-value pair where a value's printed representation starts with #<.
Convert Lisp booleans: t to true, nil to false."
  (with-output-to-string (out)
    (write-char #\{ out)
    (let ((first t))
      (maphash
       (lambda (key val)
         (let ((val-str (if (listp val)
                            nil
                            (princ-to-string val))))
           ;; Skip if value string starts with "#<"
           (unless (and val-str
                        (>= (length val-str) 2)
                        (char= (char val-str 0) #\#)
                        (char= (char val-str 1) #\<))
             (unless first
               (write-string ", " out))
             (setf first nil)
             ;; convert key
             (let ((key-str (stringify-key key)))
               (format out "\"~A\": " key-str)
               ;; handle value(s)
               (cond
                 ;; Boolean t
                 ((eq val t)
                  (write-string "true" out))
                 ;; Boolean nil
                 ((null val)
                  (write-string "false" out))
                 ;; JSON array
                 ((listp val)
                  (write-char #\[ out)
                  (loop for v in val
                        for i from 0
                        do (progn
                             (when (> i 0) (write-string ", " out))
                             (cond
                               ((eq v t) (write-string "true" out))
                               ((null v) (write-string "false" out))
                               (t (format out "\"~A\"" (stringify-value v))))))
                  (write-char #\] out))
                 ;; single value (non-boolean atom)
                 (t
                  (format out "\"~A\"" (stringify-value val))))))))
       ht))
    (write-char #\} out)))

(defun pretty-print-json-file (path &key (indent-size 2))
  "Read JSON from PATH, pretty-print it, and overwrite the same file."
  (let ((json-string
          (with-open-file (in path :direction :input)
            (with-output-to-string (s)
              (loop for line = (read-line in nil nil)
                    while line
                    do (write-line line s))))))
    (let ((pretty (pretty-json json-string :indent-size indent-size)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string pretty out)))))

(defun pretty-json (json-string &key (indent-size 2))
  "Pretty-print a JSON string with indentation.
Each object/array element goes on its own line, nested by INDENT-SIZE spaces."
  (with-output-to-string (out)
    (let ((indent 0)
          (in-string nil)
          (escaped nil))
      (loop for ch across json-string do
           (cond
             ;; handle quotes
             ((char= ch #\")
              (write-char ch out)
              (unless escaped
                (setf in-string (not in-string)))
              (setf escaped nil))
             ;; handle escapes inside strings
             ((and in-string (char= ch #\\))
              (write-char ch out)
              (setf escaped (not escaped)))
             ;; if inside string, just copy characters
             (in-string
              (write-char ch out)
              (setf escaped nil))
             ;; structural characters (when not inside string)
             ((find ch "{[")
              (write-char ch out)
              (incf indent indent-size)
              (terpri out)
              (write-string (make-string indent :initial-element #\Space) out))
             ((find ch "}]")
              (decf indent indent-size)
              (terpri out)
              (write-string (make-string indent :initial-element #\Space) out)
              (write-char ch out))
             ((char= ch #\,)
              (write-char ch out)
              (terpri out)
              (write-string (make-string indent :initial-element #\Space) out))
             ((char= ch #\:)
              (write-string ": " out))
             (t
              (write-char ch out)))))))

(defstruct json-object alist)
;;;; Utilities
(defun stringify-atom (val)
  "Lower-case representation of atomic VAL, preserving colons in keywords/symbols."
  (cond
    ((stringp val) (string-downcase val))
    ((symbolp val)  (string-downcase (princ-to-string val)))
    (t              (string-downcase (princ-to-string val)))))

;;;; MOP-based reflective dumper for any CLOS instance -> alist of (\"slot\" . value)
(defun instance->alist (obj)
  "Dump all slots of OBJ (a CLOS instance) into an alist of (\"slot-name\" . value)."
  (mapcar (lambda (slot-def)
            (let ((name (closer-mop:slot-definition-name slot-def)))
              (cons (string-downcase (symbol-name name))
                    (slot-value obj name))))
          (closer-mop:class-slots (class-of obj))))

(defun list-to-json (lst)
  "Convert a Common Lisp list to a JSON array string.
Each element is stringified using `princ-to-string`."
  (format nil "[~{\"~A\"~^, ~}]" 
          (mapcar (lambda (x)
                    (cond
                      ((stringp x) x)
                      ((symbolp x) (string-downcase (symbol-name x)))
                      (t (princ-to-string x))))
                  lst)))

(defun list-of-lists->json (lol &optional (wrap t))
  "Convert a list of 2-element lists into a JSON object string.
Each sublist (key value) becomes \"key\": value.
If WRAP is NIL, omit the outer braces."
  (with-output-to-string (out)
    (when wrap (write-char #\{ out))
    (let ((first t))
      (dolist (pair lol)
        (unless first (write-string ", " out))
        (setf first nil)
        (destructuring-bind (key value) pair
          ;; stringify key
          (format out "\"~A\": " (string-downcase (symbol-name key)))
          ;; stringify value
          (cond
            ((atom value)
             (format out "\"~A\"" (if (symbolp value)
                                      (string-downcase (symbol-name value))
                                      value)))
            ((listp value)
             (format out "[~{ \"~A\" ~^, ~}]" (mapcar #'princ-to-string value)))
            (t
             ;; fallback
             (format out "\"~A\"" value))))))
    (when wrap (write-char #\} out))))

(defun list-of-lists->json-list (lol)
  "Convert a list of lists to a JSON array of arrays string.
Wrap elements that are strings in extra quotes."
  (with-output-to-string (out)
    (write-char #\[ out)
    (let ((first t))
      (dolist (sublist lol)
        (unless first (write-string ", " out))
        (setf first nil)
        (write-char #\[ out)
        (let ((subfirst t))
          (dolist (elem sublist)
            (unless subfirst (write-string ", " out))
            (setf subfirst nil)
            (cond
              ((stringp elem)
               (format out "\"\\\"~A\\\"\"" elem))  ; wrap string in extra quotes
              ((symbolp elem)
               (format out "\"~A\"" (string-downcase (symbol-name elem))))
              (t
               (format out "\"~A\"" elem)))))
        (write-char #\] out)))
    (write-char #\] out)))

(defun contributing->json (contrib ft)
  (let ((body "[")
        (first t))
    (loop for unit in contrib
          do (progn
               (unless first
                 (setf body (concatenate 'string body ",")))
               (setf first nil)
               (setf body (concatenate 'string body "["))
               (setf body (concatenate 'string body "\"" (stringify-atom (name unit)) "\","))
               (setf body (concatenate 'string body "{"))
               (let ((first2 t))
                 (loop for feature in (unit-structure unit)
                       for feature-type = (cdr (assoc (car feature) ft))
                       do (progn
                            (unless first2
                              (setf body (concatenate 'string body ",")))
                            (setf first2 nil)
                            (setf body (concatenate 'string body "\"" (stringify-atom (car feature)) "\":"))
                            (if (or (equal (symbol-name (car feature-type)) "SET") (equal (symbol-name (car feature-type)) "SEQUENCE"))
                              (setf body (concatenate 'string body (list-to-json (car (cdr feature)))))
                              (let ((value (cdr feature)))
                                (when (and (listp value) (= (length value) 1))
                                  (setf value (first value)))
                                (cond
                                 ;; Case 1: single atom
                                 ((atom value)
                                  ;; atomic value
                                  (setf body (concatenate 'string body "\"" (stringify-atom value) "\""))
                                  )

                                 ((and (listp value)
                                       (every #'listp value))
                                  ;; nested sublists
                                  (setf body (concatenate 'string body (list-of-lists->json (cdr feature))))
                                  )
                                 
                                 (t
                                  ;; mixed or unexpected structure
                                  ))))
                          )))
               (setf body (concatenate 'string body "}"))
               (setf body (concatenate 'string body "]"))
               ))
    (concatenate 'string body "]")
  ))

(defun conditional->json (condit ft)
  (let ((body "[")
        (first t))
    (loop for unit in condit
          do (progn
               (unless first
                 (setf body (concatenate 'string body ",")))
               (setf first nil)
               (setf body (concatenate 'string body "["))
               (setf body (concatenate 'string body "\"" (stringify-atom (name unit)) "\","))
               (setf body (concatenate 'string body "{"))
               ;formulation lock
               (if (and (equal (symbol-name (first (first (formulation-lock unit)))) "HASH") (equal (symbol-name (second (first (formulation-lock unit)))) "MEANING"))
                 (progn
                   (setf body (concatenate 'string body "\"#meaning\":"))
                   (setf body (concatenate 'string body (alist->json-list (third (first (formulation-lock unit)))))))
                 (let ((first2 t))
                   (loop for feature in (formulation-lock unit)
                       for feature-type = (cdr (assoc (car feature) ft))
                       do (progn
                            (unless first2
                              (setf body (concatenate 'string body ",")))
                            (setf first2 nil)
                            (setf body (concatenate 'string body "\"" (stringify-atom (car feature)) "\":"))
                            (if (or (equal (symbol-name (car feature-type)) "SET") (equal (symbol-name (car feature-type)) "SEQUENCE"))
                              (setf body (concatenate 'string body (list-to-json (car (cdr feature)))))
                              (let ((value (cdr feature)))
                                (when (and (listp value) (= (length value) 1))
                                  (setf value (first value)))
                                (cond
                                 ;; Case 1: single atom
                                 ((atom value)
                                  ;; atomic value
                                  (setf body (concatenate 'string body "\"" (stringify-atom value) "\""))
                                  )
                                 ;; Case 3: several sublists
                                 ((and (listp value)
                                       (every #'listp value))
                                  ;; nested sublists
                                  (setf body (concatenate 'string body (list-of-lists->json (cdr feature))))
                                  )
                                 
                                 (t
                                  ;; mixed or unexpected structure
                                  ))))
                          ))))
               (setf body (concatenate 'string body "},"))
               (setf body (concatenate 'string body "{"))
               ;comprehension lock
               (if (and (equal (symbol-name (first (first (comprehension-lock unit)))) "HASH") (equal (symbol-name (second (first (comprehension-lock unit)))) "FORM"))
                 (progn
                   (setf body (concatenate 'string body "\"#form\":"))
                   (setf body (concatenate 'string body (list-of-lists->json-list (third (first (comprehension-lock unit)))))))
                 (let ((first2 t))
                 (loop for feature in (comprehension-lock unit)
                       for feature-type = (cdr (assoc (car feature) ft))
                       do (progn
                            (unless first2
                              (setf body (concatenate 'string body ",")))
                            (setf first2 nil)
                            (setf body (concatenate 'string body "\"" (stringify-atom (car feature)) "\":"))
                            (if (or (equal (symbol-name (car feature-type)) "SET") (equal (symbol-name (car feature-type)) "SEQUENCE"))
                              (setf body (concatenate 'string body (list-to-json (car (cdr feature)))))
                              (let ((value (cdr feature)))
                                (when (and (listp value) (= (length value) 1))
                                  (setf value (first value)))
                                (cond
                                 ;; Case 1: single atom
                                 ((atom value)
                                  ;; atomic value
                                  (setf body (concatenate 'string body "\"" (stringify-atom value) "\""))
                                  )
     
                                 ;; Case 3: several sublists
                                 ((and (listp value)
                                       (every #'listp value))
                                  ;; nested sublists
                                  (setf body (concatenate 'string body (list-of-lists->json (cdr feature))))
                                  )
                                 
                                 (t
                                  ;; mixed or unexpected structure
                                  ))))
                          ))))               
               (setf body (concatenate 'string body "}"))
               
               (setf body (concatenate 'string body "]"))
               ))
    (concatenate 'string body "]")))

;;;; High-level: convert a list of cxn instances into the desired JSON object.
(defun cxns->json (cxns ft)
  (with-output-to-string (out)
    (write-char #\{ out)
    (let ((first t))
      (dolist (cxn cxns)
        (let* ((name (stringify-atom (slot-value cxn 'name)))
               (contrib (slot-value cxn 'contributing-part))
               (condit  (slot-value cxn 'conditional-part)))
          (unless first (write-string ", " out))
          (setf first nil)
          (format out "\"~A\": " name)
          (write-char #\{ out)
          (format out "\"name\": \"~A\", " name)
          (format out "\"contributing-pole\": ~A, "
                  (contributing->json contrib ft))
          (format out "\"conditional-pole\": ~A"
                  (conditional->json condit ft))
          (write-char #\} out))))
    (write-char #\} out)))


(defun ofef-parser (fcg-constructions-object &optional (filename "ofef"))
  "Parse the fcg-constructions-object into an OFEF (JSON) file to be imported in PyFCG. The file is saved in /babel/.tmp. The fcg-constructions-object is typically *fcg-constructions*, unless specified otherwise in the grammar definition"
  (let ((filepath (babel-pathname :name filename :type "json" :directory '(".tmp"))))

    (with-open-file (out filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "{")
      
      (format out "\"configuration\":")
  ;(format out "~A" (hash-table-to-list-of-bindings (configuration (configuration (processing-cxn-inventory *fcg-constructions*)))))
      (let ((config (configuration (configuration (processing-cxn-inventory fcg-constructions-object)))))
        (format out (hashtable->json config))
        )
      (format out ",\"features-types\":")
      (let ((ft (feature-types fcg-constructions-object)))
        (format out (alist->json-list ft))
        (format out ",\"categorial-network\":{\"nodes\":")
        (format out (hashtable-keys->json (graph-utils::nodes (graph (categorial-network fcg-constructions-object)))))
        (format out ",\"edges\":")
        (format out (links->json (graph-utils::list-edges (graph (categorial-network fcg-constructions-object)))))
        (format out "},\"cxns\":")
        (format out (cxns->json (constructions fcg-constructions-object) ft))
        (format out "}")))

    (pretty-print-json-file filepath)))




