(in-package :asdf)

(defsystem :fcg-propbank
  :description "An FCG subsystem for learning large-scale construction grammars from PropBank-annotated data."
  :depends-on (:utils :nlp-tools :cl-store :fcg :irl :monitors)
  :serial t
  :components ((:file "package")
               (:file "classes")
               (:module fcg-components
                :serial t
                :components ((:file "de-render")
                             (:file "goal-tests")
                             (:file "node-tests")
                             (:file "cxn-supplier")
                             (:file "hash-mode")
                             (:file "heuristics")
                             (:file "frame-visualisation")
                             (:file "extract-frames")
                             (:file "comprehend")))
               (:module annotation
                :serial t
                :components ((:file "conll-annotations-interface")
                             (:file "ewt-ontonotes-english")))
               (:module learning
                :serial t
                :components ((:file "utils")
                             (:file "learn-propbank-grammar")))))
