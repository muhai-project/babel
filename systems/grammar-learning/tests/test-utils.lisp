(in-package :grammar-learning)

(deftest test-diff-clevr-networks ()
         (let ((network-1 '((query ?target-4 ?target-object-1 ?attribute-2)
                            (unique ?target-object-1 ?target-33324)
                            (filter ?target-33324 ?target-33323 ?size-4)
                            (filter ?target-33323 ?target-2 ?color-2)
                            (filter ?target-2 ?target-1 ?material-4)
                            (filter ?target-1 ?source-1 ?shape-8)
                            (get-context ?source-1)
                            (bind attribute-category ?attribute-2 shape)
                            (bind size-category ?size-4 large)
                            (bind color-category ?color-2 gray)
                            (bind material-category ?material-4 metal)
                            (bind shape-category ?shape-8 thing)))
               (network-2 '((query ?target-4 ?target-object-1 ?attribute-2)
                            (unique ?target-object-1 ?target-2)
                            (filter ?target-2 ?target-1 ?size-4)
                            (filter ?target-1 ?source-1 ?shape-8)
                            (get-context ?source-1)
                            (bind attribute-category ?attribute-2 shape)
                            (bind size-category ?size-4 large)
                            (bind shape-category ?shape-8 thing)))
               (network-3 '((query ?target-4 ?target-object-1 ?attribute-2)
                            (unique ?target-object-1 ?target-33324)
                            (filter ?target-33324 ?target-33323 ?size-4)
                            (filter ?target-33323 ?target-2 ?color-2)
                            (filter ?target-2 ?target-1 ?material-4)
                            (filter ?target-1 ?source-1 ?shape-8)
                            (get-context ?source-1)
                            (bind attribute-category ?attribute-2 shape)
                            (bind size-category ?size-4 large)
                            (bind color-category ?color-2 blue)
                            (bind material-category ?material-4 metal)
                            (bind shape-category ?shape-8 thing)))
               
               (result-1-vs-2 '((bind material-category ?material-4 metal)
                                (bind color-category ?color-2 gray)
                                (filter ?target-2 ?target-1 ?material-4)
                                (filter ?target-33323 ?target-2 ?color-2)
                                ))
               (result-3-vs-1 '(((bind color-category ?color-2 blue)
                                 (filter ?target-33323 ?target-2 ?color-2))
                                ((bind color-category ?color-2 gray)
                                 (filter ?target-33323 ?target-2 ?color-2))))
               (result-1-vs-3 '(((bind color-category ?color-2 gray)
                                 (filter ?target-33323 ?target-2 ?color-2))
                                ((bind color-category ?color-2 blue)
                                 (filter ?target-33323 ?target-2 ?color-2)))))

           (test-equal (first (multiple-value-list (diff-clevr-networks network-1 network-2)))
                       result-1-vs-2)
           (test-equal (second (multiple-value-list (diff-clevr-networks network-2 network-1)))
                       result-1-vs-2)
           (test-equal (multiple-value-list (diff-clevr-networks network-1 network-3))
                       result-1-vs-3)
           (test-equal (multiple-value-list (diff-clevr-networks network-3 network-1))
                       result-3-vs-1)
           ))

(deftest test-extract-args-from-irl-network ()
  (let ((network-1 '((query ?target-4 ?target-object-1 ?attribute-2)
                     (unique ?target-object-1 ?target-33324)
                     (filter ?target-33324 ?target-33323 ?size-4)
                     (filter ?target-33323 ?target-2 ?color-2)
                     (filter ?target-2 ?target-1 ?material-4)
                     (filter ?target-1 ?source-1 ?shape-8)
                     (get-context ?source-1)
                     (bind attribute-category ?attribute-2 shape)
                     (bind size-category ?size-4 large)
                     (bind color-category ?color-2 gray)
                     (bind material-category ?material-4 metal)
                     (bind shape-category ?shape-8 thing)))
        (network-2 '((bind material-category ?material-4 metal)
                     (bind color-category ?color-2 gray)
                     (filter ?target-2 ?target-1 ?material-4)
                     (filter ?target-33323 ?target-2 ?color-2)
                     ))
        (network-3 '((bind color-category ?color-2 blue)
                     (filter ?target-33323 ?target-2 ?color-2))))
                                
    (test-equal (extract-args-from-irl-network network-1) '(?target-4))
                       
    (test-equal (extract-args-from-irl-network network-2) '(?target-1 ?target-33323))
                       
    (test-equal (extract-args-from-irl-network network-3) '(?target-2 ?target-33323))))
                       
           
           

;(test-diff-clevr-networks)
;(test-extract-args-from-irl-network)
