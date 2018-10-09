
(in-package :common-lisp-user)

(defpackage :frame-extractor
  (:use :cl-user
	:common-lisp
        :utils
	:test-framework
	:fcg
        :irl :pie
        :nlp-tools
        #+:hunchentoot-available-on-this-platform :web-interface
        :monitors
        :meta-layer-learning
       ; :experiment-framework ;;needed for data monitors
        :type-hierarchies
        :cl-json
        :tasks-and-processes)
  (:shadow "PROTOTYPE" "PP")
  )