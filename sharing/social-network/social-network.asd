(in-package :asdf)

(defsystem :social-network
  :description "System for representing social networks."
  :author "Hermes I Research Team"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
  :depends-on (:utils 
               :test-framework
               :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "social-network")
               (:file "visualisation")))