(in-package :qc)

(defclass node ()
  ((id :type integer :initarg :id :accessor id)
   (parent :type node :initarg :parent :initform nil :accessor parent)
   (children :type list :initarg :children :initform '() :accessor children)
   (depth :type integer :initarg :depth :initform nil :accessor depth)
   (tble :type table :initarg :tble :initform nil :accessor tble)
   (ref-tbles :type list :initarg :ref-tbles :initform '() :accessor ref-tbles)
   (attrs :type attrs :initarg :attrs :initform nil :accessor attrs)
   (q :type integer :initarg :q :initform "" :accessor q)))

;;OK
(defun init-node (id node attributes table &key join)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q "SELECT ")
           (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1)  :q "" :tble table :ref-tbles (list table)))
           (last-elem (last attributes)))
      (dolist (att-n attributes)
          (if join
            (progn
              (if (or (equal (length attributes) 1) (equal att-n (first last-elem)))
                (setf q (concatenate 'string q (name table) "." att-n))
                (setf q (concatenate 'string q (name table) "." att-n ","))))
            (progn
              (if (or (equal (length attributes) 1) (equal att-n (first last-elem)))          
                (setf q (concatenate 'string q att-n))
                (setf q (concatenate 'string q att-n ","))))))
      (setf q (concatenate 'string q " FROM "(name  table)))
      (setf (q child) q)
      child))

(defun join-node (id node ref-info table &key foreign-ref outer-join)
  "function that create a node with the ... INNER JOIN || OUTER JOIN ... ON ... =  ... clause and return the newly created node."
  (let* ((f-table "")
          (f-column "")
          (table "")
          (column "")
          (join " INNER JOIN "))
    (if foreign-ref
      (progn
        (setf f-table (foreign-table ref-info))
        (setf f-column (foreign-column ref-info))
        (setf table (table-name ref-info))
        (setf column (column-name ref-info)))
      (progn
        (setf f-table (table-name ref-info))
        (setf f-column (column-name ref-info))
        (setf table (foreign-table ref-info))
        (setf column (foreign-column ref-info))))
    (if outer-join
      (setf join " FULL OUTER JOIN "))
  (make-instance 'node :id id :parent node :depth (depth node) :ref-tbles (push-end table (ref-tbles node)) :q (concatenate 'string (q node) join f-table " ON " f-table "." f-column "=" table "." column))))


;;OK
(defun where-node (id node attribute operator value att)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
         (q (concatenate 'string (q node) " WHERE " attribute " " operator " '"val"'"))
          (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1) :q q :attrs att :tble (tble node))))
    child))
;;OK
(defun and-node (id node attribute operator value)
  "function that creates a node with the AND clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (q (concatenate 'string (q node) " AND " (name attribute) " " operator " '" val "'"))
          (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1) :q q :attrs (append (attrs node) (list attribute)) :tble (tble node))))
    child))
;;OK
(defun or-node (id node attribute operator value)
  "function that creates a node with the OR clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (q (concatenate 'string (q node) " OR " (name attribute) " " operator " '" val "'"))
          (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1) :q q :attrs (append (attrs node) (list attribute)) :tble (tble node))))
    child))