(in-package :asdf)

(defsystem #:clevr-grammar-learning
  :description "Tutor-learner experiment to learn the clevr grammar, based on clevr-learning by Jens Nevens"
  :author "Jonas Doumen & Paul Van Eecke <ehai@ai.vub.ac.be>"
  :maintainer "Jonas Doumen <jonas.doumen@kuleuven.be>"
  :license "to be decided on"
  :depends-on (:utils
               :experiment-framework
               :plot-raw-data
               :monitors
               :web-interface
               :meta-layer-learning
               :irl
               :fcg
               :category-hierarchies
               :clevr-world
               :grammar-learning
               :clevr-learning)
  :serial t
  :components ((:file "package")
               (:file "run-helpers")
               (:file "grammar")
               (:file "agent")
               (:file "learner")
               (:file "experiment")
               (:file "interaction")
               (:file "web-monitors")
               (:file "monitors")
               (:file "lisp-monitors")
               (:file "csv-monitors")))