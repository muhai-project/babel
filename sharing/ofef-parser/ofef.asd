(in-package :asdf)

(defsystem :ofef
  :depends-on (:fcg)
  :components ((:file "export"))
  :description "Common Lisp FCG interface to the Open FCG Exchange Format (ofef).")
