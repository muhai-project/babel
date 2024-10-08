(in-package :cl-user)

(defpackage :seq2seq-fcg
  (:documentation "Using Seq2Seq models for the FCG search heuristics")
  (:use :common-lisp :utils :monitors :irl :fcg :clevr-world :clevr-primitives :clevr-grammar)
  (:import-from :trivial-timeout :with-timeout
                :timeout-error)
  (:import-from :cl-csv :do-csv :write-csv-row)
  (:shadowing-import-from :fcg :size :attributes))
