(in-package :qc)

; not empty
(defun notempty (list)
  (not (null list)))

;empty
(defun empty (list)
  (null list))

; remove duplicates form lisp
(defun remove-duplicates-from-list (list)
  (remove-duplicates list :test #'equal))

; coefficient binomial
(defun binomial-coefficient (n k)
  (let ((result 1))
    (dotimes (i k)
      (setf result (* result (- n i) (/ (+ i 1) (1+ i)))))
    result))

; get all items of the list where depth = x in param
(defun get-all-items-from-depth(tree depth)
  (let ((items '()))
    (dolist (node (nodes tree))
      (if (= (depth node) depth)
        (push node items)))
    items))

;get all attributes from list of tables
(defun get-all-attributes-from-table (tables)
  (let ((list-of-attribute '()))
    (dolist (table tables)
      (if (not (notempty list-of-attribute))
        (setf list-of-attribute (attributes table))
        (setf list-of-attribute (append list-of-attribute (attributes table)))))
    list-of-attribute))
        
(defun get-all-permutations (att len subsets)
  (let ((sub subsets))
    (if (<= len (length att))
      (progn
        (setf sub (append sub (permutations-of-length att len)))
        (get-all-permutations att (+ len 1) sub))
      sub)))

(defun change-type (val)
  "Function that change the type of returning value of database"
  (cond ((typep val 'double-float)
         (return-from change-type (write-to-string (round val)))))
  (cond ((typep val 'integer)
         (return-from change-type (write-to-string val))))
  (return-from change-type val))

(defun concat-array (array)
  (concatenate 'string "" (first array)))

(defun push-end (item lst)
  "Push at the end of a sequence an item and returning the sequence"
  (setf lst (append lst (list item))))




(defun is-table-present (name lst &key get-obj)
  (let ((result nil))
    (dolist (table lst)
      (if (equal name (name table))
        (if get-obj
          (setf result table)
          (setf result t))))
    result))


(defun random-between (min max)
  (+ min (random (+ 1 (- max min)))))



(defun sort-type (table answer)
  (let ((result '()))
    (dolist (val answer)
      (dolist (att (attributes table))
        (if (typep val (type-att att))
          (push att result))))
    result))