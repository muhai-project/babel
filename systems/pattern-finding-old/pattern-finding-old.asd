(defsystem pattern-finding-old
  :author "Paul Van Eecke, Katrien Beuls, Jonas Doumen, Veronica Juliana Schmalz, and Jens Nevens"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/ehai-babel/"
  :depends-on (:utils
               :monitors
               :plot-raw-data
               :web-interface
               :fcg
               :irl
               :amr
               :meta-layer-learning
               :experiment-framework
               :test-framework
               :plot-raw-data
               :cl-json)
  :serial t
  :components ((:file "package")
               (:module utils
                :serial t
                :components ((:file "cxn-supplier")
                             (:file "render-and-de-render")
                             (:file "goal-tests")
                             (:file "fcg-utils")
                             (:file "utils")))
               (:module diagnostics-and-repairs
                :serial t
                :components ((:file "handle-fix")
                             (:file "problems-diagnostics")
                             (:file "cxn-skeletons")
                             (:file "handle-potential-holistic-cxn")
                             (:file "compute-args")
                             (:file "repair-add-categorial-links")
                             (:file "repair-nothing-to-holophrase-cxn")
                             (:file "repair-anti-unify-cxns")
                             (:file "repair-holistic-partial-analysis")
                             (:file "repair-item-based-partial-analysis")))
               (:module experiment-setup
                :serial t
                :components ( (:file "run-helpers")
                              (:file "agent")
                              (:file "alignment")
                              (:file "experiment")
                              (:file "interaction")
                              (:module monitors
                               :serial t
                               :components ((:file "utils")
                                            (:file "web-monitors")
                                            (:file "export-monitors")
                                            (:file "lisp-monitors")))))
               (:module tests
                :serial t
                :components ((:file "utils"))
               ;              (:module irl
               ;               :serial t
               ;               :components ((:file "test-substitutions")
               ;                            (:file "test-additions")
               ;                            (:file "test-deletions"))
                ))
  :description "A Common Lisp package for learning construction grammars.")


