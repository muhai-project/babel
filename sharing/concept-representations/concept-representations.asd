(in-package :asdf)

(defsystem :concept-representations
  :description "System for representing, updating and comparing concepts."
  :author "Hermes I Research Team"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
  :depends-on (:utils 
               :test-framework
               :monitors
               :irl
               #+:hunchentoot-available-on-this-platform :web-interface
               :com.inuoe.jzon)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "entity")
               (:file "concept")
               (:file "distribution")
               (:file "similarity")
               (:file "update")
               (:file "dataloader")
               (:file "visualisation")
               ))