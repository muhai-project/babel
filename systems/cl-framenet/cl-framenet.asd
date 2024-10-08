(in-package :asdf)

(defsystem :cl-framenet
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license "To be determined"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/cl-framenet"
  :depends-on (:cl-store :xmls :cl-ppcre)
  :description "A Common Lisp interface to FrameNet."
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "configuration")
               (:file "framenet-data")
               (:file "frame")
               (:file "frame-element")
               (:file "load-fn-data")))
