(in-package :cl-user)

(defpackage :naming-game
  (:use :common-lisp :cl-user
   :experiment-framework
   :utils
   :monitors
   :fcg
   :plot-raw-data
   #+:hunchentoot-available-on-this-platform :web-interface)
  (:documentation "functions to create a naming game"))

