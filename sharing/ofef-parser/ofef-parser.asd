(defsystem "ofef-parser"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:fcg :graph-utils)
  :components ((:file "package")
               (:file "parser"))
  :description "Parser to go from a *fcg-constructions* object in CL to the OFEF (JSON) format to load in PyFCG"
  )

