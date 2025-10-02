(in-package :cl-user)

(defpackage :ofef-parser
  (:documentation "Parser to go from a *fcg-constructions* object in CL to the OFEF (JSON) format to load in PyFCG")
  (:use :cl
        :fcg
        :graph-utils
        :parse-float)
  (:shadowing-import-from :fcg "UNIFY"))
        



