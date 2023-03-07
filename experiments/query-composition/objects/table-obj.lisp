(in-package :qc)

(defclass table ()
  ((name
    :type string
    :initarg :name
    :accessor name)
   (attributes
    :type list
    :initform nil
    :initarg :attributes
    :accessor attributes)))

;;OK
(defun init-table (table-name attrs)
  "Function that instantiates a database table with all attributes"
  (let ((table (make-instance 'table :name table-name :attributes '())))
    (dolist (attr attrs)
       (let ((attribute-check (flatten (query (concatenate 'string "SELECT " (concat-array attr) " FROM " table-name " LIMIT 1")))))
         (push (make-instance 'attribute :name (concat-array attr) :type-att (type-of (first attribute-check))) (attributes table))))
    table))

;;OK
(defun init-schema ()
  "Function that instantiates the whole database connected with the postmodern library"
  (let ((tables '())
        (table-name nil)
        (attrs '()))
    (dolist (res (query "SELECT table_name, column_name FROM information_schema.columns WHERE table_schema='public' ORDER BY table_name, column_name"))
      (if (not table-name)
        (setf table-name (first res)))
      (if (equal table-name (first res))
        (push (last res) attrs)
        (progn
          (push (init-table table-name attrs) tables)
          (setf attrs '())
          (setf table-name (first res))
          (push (last res) attrs))))
    (push (init-table table-name attrs) tables)
  tables))