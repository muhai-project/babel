(in-package :asdf)

(defsystem :partner-selection
  :description "System for representing partner preferences."
  :author "Hermes I Research Team"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
  :depends-on (:utils 
               :test-framework)
  :serial t
  :components ((:file "package")
               (:file "boltzmann-exploration")))