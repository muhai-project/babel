(in-package :asdf)

(defsystem #:seq2seq-fcg
  :description "Using Seq2Seq models for the FCG search heuristics"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:trivial-timeout
               :utils
               :corpus-processing
               :monitors
               :fcg
               :clevr-grammar
               :clevr-dialog-grammar)
  :serial t
  :components ((:file "package")
               (:file "apply-predictions")
               (:file "enhance-data")))
