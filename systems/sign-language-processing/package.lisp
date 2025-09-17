(in-package :cl-user)

(defpackage :slp
  (:documentation "Package for representing and processing sign languages using FCG")
  (:use
   :common-lisp
   :utils
   :web-interface
   :monitors
   :irl
   :fcg
   :xmls-system
   :grammar-learning)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :cl-user
   *babel-corpora*))
