(in-package :cl-user)

(defpackage :clevr-primitives
  (:documentation "The IRL primitives for CLEVR")
  (:use :common-lisp :utils :irl :clevr-world))

(in-package :clevr-primitives)

(export '(*clevr-primitives*))

(def-irl-primitives clevr-primitives
  :primitive-inventory *clevr-primitives*)
