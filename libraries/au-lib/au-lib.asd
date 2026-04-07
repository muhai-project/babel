(in-package :asdf)

(defsystem "au-lib"
  :description "Library of anti-unification algorithms"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Apache 2.0 License"
  :depends-on ("cl-pcp"
               "trivial-timeout"
               "clingon"
               "split-sequence"
               "alexandria"
               "com.inuoe.jzon"
               "monitors" ;; !!! monitors package from babel
               "fcg";; !!! fcg system from babel
               )
  :serial t
  :components ((:file "packages")
               (:module utils
                :serial t
                :components ((:file "lists-and-sets")
                             (:file "symbols-and-strings")
                             (:file "math")
                             (:file "queue")
                             (:file "variables")
                             (:file "bindings")
                             (:file "equivalent-predicate-networks")))
               (:file "classes")
               (:file "cost")
               (:file "anti-unify-predicate-sequence")
               (:module algorithms
                :serial t
                :components ((:module msg
                              :serial t
                              :components ((:file "msg-utils")
                                           (:file "monitors")
                                           (:file "generic")
                                           (:file "baseline")
                                           (:file "exhaustive")
                                           (:file "1-swap")
                                           (:file "2-swap"))))))
  :in-order-to ((test-op (test-op "au-benchmark/tests"))))

;; to run unit tests, execute (asdf:test-system :au-benchmark)
(defsystem "au-benchmark/tests"
  :depends-on ("fiveam" "au-benchmark")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "unit-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* 'au-benchmark-tests :au-benchmark-tests))))
