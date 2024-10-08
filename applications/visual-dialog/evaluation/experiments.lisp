(in-package :visual-dialog)

(define-configuration-default-value :dataset :clevr)
(define-configuration-default-value :datasplit :train)
(define-configuration-default-value :mode :symbolic)
(define-configuration-default-value :server-address "http://127.0.0.1:2560/")
;; default of cookie jar is make-instance, so a new session is started
(define-configuration-default-value :cookie-jar (make-instance 'drakma:cookie-jar))
(define-configuration-default-value :evaluation-mode :normal)
(define-configuration-default-value :search-mode :best-first)

(defun evaluate-clevr-dialogs-symbolic (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :clevr)
                                         (:datasplit . :val)
                                         (:mode . :symbolic)))))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-mnist-dialogs-symbolic (start-scene end-scene)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :mnist)
                                         (:datasplit . :test)
                                         (:mode . :symbolic)))))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-clevr-dialogs-hybrid (start-scene end-scene &optional server-address)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :clevr)
                                         (:datasplit . :val)
                                         (:mode . :hybrid)
                                         ))))
    (set-configuration *subsymbolic-primitives* :search-mode :best-first)
    (if server-address
      (set-configuration world :server-address server-address))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-mnist-dialogs-hybrid (start-scene end-scene &optional server-address)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :mnist)
                                         (:datasplit . :test)
                                         (:mode . :hybrid)))))
    (if server-address
      (set-configuration world :server-address server-address))
    (evaluate-dialogs start-scene end-scene world)))


(defun evaluate-clevr-dialogs-hybrid-guess (start-scene end-scene &optional server-address)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :clevr)
                                         (:datasplit . :val)
                                         (:mode . :hybrid)
                                         (:evaluation-mode . :guess)
                                         ))))
    (if server-address
      (set-configuration world :server-address server-address))
    (evaluate-dialogs start-scene end-scene world)))

(defun evaluate-mnist-dialogs-hybrid-guess (start-scene end-scene &optional server-address)
  (let ((world (make-instance 'world 
                              :entries '((:dataset . :mnist)
                                         (:datasplit . :test)
                                         (:mode . :hybrid)
                                         (:evaluation-mode . :guess)))))
    (if server-address
      (set-configuration world :server-address server-address))
    (evaluate-dialogs start-scene end-scene world)))