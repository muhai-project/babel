;;;; start-server.lisp

(ql:quickload :clevr-web-service)

(in-package :hunchentoot)

(export '(cors-acceptor))

(defclass cors-acceptor (easy-acceptor)
  ()
  (:documentation "Subclass of easy-acceptor to be able to set
cross-origin headers in the accetor-dispatch-request method"))

(defmethod acceptor-dispatch-request ((acceptor cors-acceptor) request)
  "The easy request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop for dispatcher in *dispatch-table*
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

(defmethod acceptor-dispatch-request :around ((acceptor cors-acceptor) request)
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type,Accept,Origin")
  (call-next-method))

(in-package :clevr-web-service)

;; need to set the *CLEVR* configurations to depth-first!
(set-configurations *CLEVR*
                    '((:cxn-supplier-mode . :ordered-by-label-hashed)
                      (:priority-mode . :nr-of-applied-cxns)
                      (:parse-order hashed nom cxn)
                      (:production-order hashed-lex nom cxn hashed-morph)
                      (:max-nr-of-nodes . 10000)))

(defvar *clevr-app* (snooze:make-hunchentoot-app))
(push *clevr-app* hunchentoot:*dispatch-table*)
(defvar *clevr-acceptor* (make-instance 'hunchentoot:cors-acceptor :port 9003))
(load-validation-set)
(hunchentoot:start *clevr-acceptor*)

;; (hunchentoot:stop *clevr-acceptor*)
