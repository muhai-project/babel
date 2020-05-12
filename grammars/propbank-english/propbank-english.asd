(in-package :asdf)

(defsystem :propbank-english
  :description "A large propbank-based construction grammar for English."
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI"
  :license "To be determined."
  :depends-on (:utils
               :monitors
               :fcg
               :nlp-tools
               :irl
               :web-interface)
  :serial t
  :components ((:file "package")
               (:file "de-render")
               (:file "grammar")
               (:file "visualisation")
               (:file "utils")))
