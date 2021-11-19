(in-package :asdf)

(defsystem :propbank-grammar
  :description "A Babel package for learning large-scale PropBank-based construction grammars."
  :author "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "To be determined."
  :depends-on (:utils :nlp-tools :cl-store :fcg)
  :serial t
  :components ((:file "package")
               
               (:module propbank-annotations
                :serial t
                :components ((:file "propbank-annotations")))
               (:module fcg-components
                :serial t
                :components ((:file "de-render")))))
