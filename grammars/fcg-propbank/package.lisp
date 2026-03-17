(in-package :cl-user)

(defpackage :fcg-propbank
  (:documentation "An FCG subsystem for learning large-scale construction grammars from PropBank-annotated data.")
  (:use :common-lisp :cl-user :utils :monitors :wi :fcg)
  (:export
   #:read-propbank-conll-file
   #:learn-propbank-grammar
   #:comprehend-and-extract-frames
   #:connl->fcg-propbank))
