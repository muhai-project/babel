(in-package :qc)

(defclass query-tree ()
  ((nodes :type list :initarg :nodes :accessor nodes)
   (root :type node :initarg :root :accessor root)
   (q :type string :initarg :q :accessor q)))
