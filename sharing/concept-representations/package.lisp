(in-package :cl-user)

(defpackage :concept-representations
  (:documentation "System for representing, updating, visualising and comparing concepts (and entities).")
  (:use :common-lisp
        :test-framework
        :utils
        :web-interface
        :monitors
        :irl)
  (:local-nicknames (:jzon :com.inuoe.jzon)))
