(in-package :asdf)

(defsystem :slp
  :description "Package for representing and processing sign languages using fcg"
  :maintainer "Liesbet De Vos"
  :depends-on (:utils
               :web-interface
               :monitors
               :irl
               :fcg
               :cl-json
               :xmls
               :grammar-learning)
  :serial t
  :components ((:file "package")
               (:module utils
                :serial t
                :components ((:file "string-manipulation")
                             (:file "xml-utils")
                             (:file "elan-utils")
                             (:file "make-fingerspelled-forms")
                             (:file "predicate-utils")
                             (:file "json-utils")
                             (:file "data-processor")
                             (:file "prolog-to-predicates")
                             (:file "classes")
                             (:file "hamnosys-to-sigml")))
               (:file "render-derender")
               (:module elan-to-predicates
                :serial t
                :components ((:file "create-elan-intervals")
                             (:file "create-predicates")
                             (:file "elan-to-predicates")))
               (:module visualization
                :serial t
                :components ((:file "css")
                             (:file "make-html")
                             (:file "make-sign-table")
                             (:file "javascript")
                             (:file "monitors")))
               (:module example-grammar
                :serial t
                :components ((:file "grammar-configurations")
                             (:file "constructions")))))