(in-package :concept-representations)

;; --------------------
;; + concept -> s-dot +
;; --------------------

;; TODO remove global parameters
(defparameter *white* "#FFFFFF")
(defparameter *red* "#E32D2D")
(defparameter *green* "#26AD26")
(defparameter *black* "#000000")

(defun add-concept-to-interface (concept &key (weight-threshold 0.0))
  "Adds a concept to the web interface.

   Weighted distributions with weights above the given weight treshold are visualised."
  (add-element
   `((div :style ,(format nil "margin-left: 50px;"))
     ,(s-dot->svg
       (concept->s-dot concept :weight-threshold weight-threshold)))))

(defmethod concept->s-dot ((concept weighted-multivariate-distribution-concept) &key (weight-threshold 0.0))
  "Creates an s-dot graph given a concept"
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; node for concept
    (push
     `(s-dot::record  
       ((s-dot::style "rounded")
        (s-dot::fillcolor "#FFFFFF")
        (s-dot::fontcolor ,*black*)
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(mkdotstr (id concept)))
                     (s-dot::label ,(format nil "~a" (mkdotstr (format nil "CONCEPT-~a" (first (last (split-string (symbol-name (id concept)) "-")))))))
                     (s-dot::fontcolor "#AA0000"))))
     g)
    ;; nodes for features
    (loop with weighted-distributions = (sort (get-weighted-distributions concept)
                                              (lambda (x y) (string< (symbol-name (feature-name x))
                                                                     (symbol-name (feature-name y)))))
          for weighted-distribution in weighted-distributions and weight-idx from 1
          for record = (weighted-distribution->s-dot weighted-distribution weight-idx)
          when (>= (weight weighted-distribution) weight-threshold)
            do (push record g))
    ;; edges for weights (between concept node and features)
    (loop with weighted-distributions = (sort (get-weighted-distributions concept)
                                              (lambda (x y) (string< (symbol-name (feature-name x))
                                                                     (symbol-name (feature-name y)))))
          for weighted-distribution in weighted-distributions and weight-idx from 1
          when (>= (weight weighted-distribution) weight-threshold)
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id concept)))
                   (s-dot::to ,(mkdotstr (downcase (feature-name weighted-distribution))))
                   (s-dot::label ,(format nil "&omega;~a: ~,2f"
                                          ;(mkdotstr (downcase (feature-name weighted-distribution)))
                                          (integer-to-subscript-html weight-idx)
                                          (weight weighted-distribution)))
                   (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                         #-(or :win32 :windows) "Arial")
                   (s-dot::fontcolor ,*black*)
                   (s-dot::fontsize "8.5")
                   (s-dot::arrowsize "0.5")
                   (s-dot::style ,(if (>= (weight weighted-distribution) 0.99) "solid" "dashed"))))
                g))
    ;; return
    (reverse g)))


(defgeneric weighted-distribution->s-dot (weighted-distribution weight-idx &key)
  (:documentation "Creates an s-dot record given a weighted-distribution."))

(defmethod weighted-distribution->s-dot ((weighted-distribution weighted-distribution) weight-idx &key)
  "Dispatches the s-dot creation process based on the type of distribution."
  (if (eq 'categorical (type-of (distribution weighted-distribution)))
    (categorical->s-dot weighted-distribution weight-idx)
    (gaussian->s-dot weighted-distribution weight-idx)))

(defmethod gaussian->s-dot ((weighted-distribution weighted-distribution) weight-idx &key)
  "Creates an s-dot object for gaussian (weighted) distributions."
  (let* ((st-dev (st-dev (distribution weighted-distribution))))
    `(s-dot::record
      ((s-dot::style "rounded")
       (s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::fontcolor "#000000")
       (s-dot::height "0.01"))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name weighted-distribution))))
                    (s-dot::label ,(format nil "~a\\n&mu;~a ~,3f ~~ &sigma;~a ~,3f"
                                           (downcase (mkdotstr (feature-name weighted-distribution)))
                                           (integer-to-subscript-html weight-idx)
                                           (mean (distribution weighted-distribution))
                                           (integer-to-subscript-html weight-idx)
                                           st-dev)))))))

(defmethod categorical->s-dot ((weighted-distribution weighted-distribution) weight-idx &key)
  "Creates an s-dot object for categorical (weighted) distributions."
  `(s-dot::record
    ((s-dot::style "rounded")
     (s-dot::fontsize "9.5")
     (s-dot::fontname #+(or :win32 :windows) "Sans"
                      #-(or :win32 :windows) "Arial")
     (s-dot::fontcolor "#000000")
     (s-dot::height "0.01"))
    (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name weighted-distribution))))
                  (s-dot::label ,(format nil "~a\\n&#123;~{~a~^, ~}&#125;"
                                         ;"Categorical"
                                         (downcase (mkdotstr (feature-name weighted-distribution)))
                                         (loop with tuples = (loop for key being the hash-keys of
                                                                     (frequencies (distribution weighted-distribution))
                                                                     using (hash-value value)
                                                                   if (> value 0)
                                                                     collect (cons key value))
                                               with sorted-tuples = (sort tuples #'> :key #'cdr)
                                               for (key . value) in sorted-tuples
                                               if (> value 0)
                                                 collect (format nil "~a:~a" key value))))))))

;; Helper functions

(defun integer-to-subscript-html (n &optional (prefix ""))
  "Converts an integer N into a sequence of Unicode subscript HTML entities,
   optionally prefixing the result with PREFIX."
  (let ((subscript-digit-table (make-hash-table)))
    (loop for digit in '((#\0 "&#x2080;") (#\1 "&#8321;") (#\2 "&#8322;") (#\3 "&#8323;")
                         (#\4 "&#8324;") (#\5 "&#8325;") (#\6 "&#8326;") (#\7 "&#8327;")
                         (#\8 "&#8328;") (#\9 "&#8329;"))
          do (setf (gethash (car digit) subscript-digit-table) (cadr digit)))
    (concatenate 'string prefix
                 (with-output-to-string (out)
                   (dolist (digit (coerce (princ-to-string n) 'list))
                     (format out "~a" (gethash digit subscript-digit-table digit)))))))
