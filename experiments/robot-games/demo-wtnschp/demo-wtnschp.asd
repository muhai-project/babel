
(in-package :asdf)

(defsystem :demo-wtnschp
  :description "Demo with the Nao robot for Science Festival"
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Apache 2.0"
  :depends-on (:utils
               :experiment-framework
               :test-framework
               :monitors
               :plot-raw-data
               :tasks-and-processes
               :meta-layer-learning
               :irl
               :fcg
               :assoc-utils
               #+:hunchentoot-available-on-this-platform :web-interface
               :nao-interface
               :robot-interface
               :printer-interface
               :asdf-encodings
               )
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "print-lexicon")
               (:file "color-conversions")
               (:file "colours")
               (:file "world")
               (:file "categories")
               (:file "lexicon")
               (:file "processes")
               (:file "agent")
               (:file "experiment")
               (:file "interaction")
               (:file "html")
               (:file "web-monitors")))
               