
(in-package :irl)

(def-irl-primitives expand-chunk-inventory
  :primitive-inventory *expand-chunk-inventory*)

(defprimitive foo ((a string) (b number) (c string))
              :primitive-inventory *expand-chunk-inventory*)
(defprimitive bar ((a number) (b string))
              :primitive-inventory *expand-chunk-inventory*)
(defprimitive baz ((a string) (b string))
              :primitive-inventory *expand-chunk-inventory*)

(deftest test-expand-chunk (&key (show-results nil))
  
  (let* ((chunk-1
          (make-instance 'chunk
                         :id 'chunk-1
                         :irl-program '((foo ?s1 ?n1 ?s2) 
                                        (bar ?n1 ?s3) (baz ?s2 ?s4))
                         :target-var '(?s1 . string)
                         :open-vars '((?s3 . string) (?s4 . string))))
         (composer
          (make-instance 'chunk-composer
                         :chunks
                         (list
                          (make-instance 'chunk
                                         :id 'chunk-2
                                         :irl-program '((baz ?s1 ?s2) (baz ?s2 ?s3))
                                         :target-var '(?s1 . string)
                                         :open-vars '((?s3 . string))))
                         :primitive-inventory *expand-chunk-inventory*))
         (chunks-and-source-chunks
          (loop for mode in (list :combine-program
                                  :combine-call-pattern
                                  :recombine-open-variables
                                  :link-open-variables)
                append (expand-chunk chunk-1 composer mode)))
         (chunks (mapcar #'first chunks-and-source-chunks)))

    (when show-results
      (clear-page)
      (add-element 
       `((p) "initial-chunk: " ,(make-html chunk-1 :expand-initially t)))
      (add-element 
       `((p) "extensions: " 
         ,@(loop for c in chunks
              collect (make-html c)))))

    (test-equal (length chunks) 6)

    (test-equal (length (open-vars (first chunks))) 2)
    (test-equal (length (irl-program (first chunks))) 5)
    (test-equal (car (fifth (irl-program (first chunks)))) 'baz)

    (test-equal (length (open-vars (third chunks))) 2)
    (test-equal (length (irl-program (third chunks))) 4)
    (test-equal (car (fourth (irl-program (third chunks)))) 'chunk-2)

    (test-equal (length (open-vars (fifth chunks))) 1)
    (test-equal (length (irl-program (fifth chunks))) 3)

    (test-equal (length (open-vars (sixth chunks))) 1)
    (test-equal (length (irl-program (sixth chunks))) 3)
    ))

;;(test-expand-chunk :show-results t)

