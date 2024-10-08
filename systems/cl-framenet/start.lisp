(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Getting Started with cl-framenet ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package requires that either your the global variable *framenet-data-directory* is set to point to the directory where the raw framenet data can be found, or that a file framenet-data.store is found in this directory.

(ql:quickload :cl-framenet)
(in-package :cl-framenet)


(defun read-frame-from-xml (frame)
  (with-open-file (inputstream (merge-pathnames
                                (make-pathname :directory '(:relative "frame")
                                               :name (string-downcase (symbol-name frame)
                                                                      :start 1)
                                               :type "xml")
                                               *framenet-data-directory*)
                               :direction :input)
    (xmls:parse inputstream)))


; (read-frame-from-xml 'opinion)


;; (setf *A* (xml-frame-object (read-frame-from-xml 'transitive_action)))
;; (setf *A* (xml-frame-elements (read-frame-from-xml 'manipulation)))
;; (setf *A* (xml-frame-relations (read-frame-from-xml 'manipulation)))

