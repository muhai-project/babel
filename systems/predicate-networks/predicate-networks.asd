(in-package :asdf)

(defsystem :predicate-networks
  :description "Package for manipulating predicate networks"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface
               :fcg :irl)
  :serial t
  :components 
  ((:file "package")
   (:file "utils")
   (:file "predicate-networks")
   (:module "amr"
    :serial t
    :components
    ((:file "utils")
     (:file "variablify")))
   (:module "irl"
    :serial t
    :components
    ((:file "utils")
     (:file "variablify")))
   (:module "anti-unification"
    :serial t
    :components
    ((:file "web-monitors")
     (:file "reorder-source-network")
     (:file "anti-unify")))
   (:module "tests"
    :serial t
    :components
    ())))