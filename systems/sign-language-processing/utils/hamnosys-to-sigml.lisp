(in-package :slp)

(defparameter *requirements-folder*
  (babel-pathname :directory '("systems" "sign-language-processing" "sign-language-processing-requirements")))

;; load inventory to make mapping from hamnosys characters to their
;; sigml names
(defparameter *hamnosys->sigml-inventory*
  (with-open-file
      (stream
       (merge-pathnames
        (make-pathname
         :name "hamnosys-to-sigml-inventory"
         :type "txt")
        *requirements-folder*)
       :external-format :utf-8 :element-type 'cl:character) 
    (loop for line =
            (read-line stream nil)
          for splitted-line =
            (when line
              (split-sequence::split-sequence #\, line))
          while line
            collect
            (cons (parse-integer
                   (second splitted-line)
                   :radix 16)
                  `(,(third splitted-line)
                    ,(first splitted-line))))))

(defun find-sigml-name (hamnosys-character)
  "finds the sigml name of a hamnosys character"
  (second
   (cdr (assoc 
         (char-code hamnosys-character)
         *hamnosys->sigml-inventory*))))

(defun find-hamnosys-character-type (hamnosys-character)
  "finds what the type of a hamnosys character is (i.e. handshape, orientation, symmetry, location, movement, or various)"
  (first
   (cdr (assoc 
         (char-code hamnosys-character)
         *hamnosys->sigml-inventory*))))

(defun make-hamnosys-node (hamnosys)
  "makes an xmls node for a hamnosys representation
   which can be used as part of a sigml representation"
  (xmls::make-node
       :name "hns_sign"
       :children
       `(,(xmls::make-node
           :name "hamnosys_manual"
           :children
           (loop for character across hamnosys
                  collect
                    (xmls::make-node
                     :name (find-sigml-name character)))))))

(defun hamnosys->sigml (hamnosys)
  "transforms hamnosys string into sigml notation and returns
   this sigml as an xmls-node"
  (xmls::make-node
   :name "sigml"
   :children
   `(,(make-hamnosys-node hamnosys))))


(defun hamnosys-list->sigml (hamnosys-list)
  "transforms list of hamnosys strings into sigml notation and returns
   this sigml as an xmls-node"
  (xmls::make-node
   :name "sigml"
   :children
   (loop for hamnosys-string in hamnosys-list
         collect
           (make-hamnosys-node hamnosys-string))))

  
  