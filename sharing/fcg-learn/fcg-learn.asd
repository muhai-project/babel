(in-package :asdf)

(defsystem :fcg-learn
  :description "A Babel package for learning construction grammars through anti-unification."
  :depends-on (:amr :fcg :irl :predicate-networks :au-lib)
  :serial t
  :components ((:file "classes")
               (:file "corpus-processor")
               (:file "monitors")
               (:file "cxn-suppliers")
               (:file "goal-tests")
               (:file "comprehend-and-formulate")
               (:file "learn-and-consolidate")
               (:file "align")
               (:file "diagnostics")
               (:file "repairs")
               (:file "pattern-finding")
               (:file "handle-fix")
               (:file "best-solution")
               (:file "equivalent-cxn")
               (:file "fix-selection")
               (:file "fcg-helper-functions")
               (:file "html")))
               
