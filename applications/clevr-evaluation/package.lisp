;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-evaluation
  (:documentation "Evaluating the CLEVR grammar and primitives")
  (:use :common-lisp :utils :fcg :clevr :clevr-primitives :clevr-grammar)
  (:import-from :monitors
                :define-event
                :define-monitor
                :define-event-handler
                :notify
                :activate-monitor
                :deactivate-monitor)
  (:import-from :web-interface
                :add-element
                :s-dot->svg
                :s-dot->image)
  (:import-from :irl
                :bind :var
                :evaluate-irl-program
                :irl-program->svg
                :get-target-var
                :trace-irl-in-web-browser)
  (:import-from :cl-json
                :decode-json-from-source
                :decode-json-from-string
                :encode-json-to-string
                :encode-json-alist-to-string)
  (:import-from :trivial-timeout
                :with-timeout)
  (:shadowing-import-from :fcg :size))
  
