(in-package :grammar-learning)




(deftest test-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-39552 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-39552)
                                         (bind color-category ?color-2 gray)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-2)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The large yellow object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))


(deftest test-substitution-repair-comprehension-right ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-39552 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-39552)
                                         (bind color-category ?color-2 gray)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-2)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The large gray object is what material?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 gray)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 material)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-substitution-repair-comprehension-multi-diff ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The large yellow object is what material?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 material)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 large)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))


(deftest test-double-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The tiny yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-double-discontinuous-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The tiny yellow object is what material?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 material)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))


(deftest test-triple-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The small purple thing is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-66128 ?target-2 ?size-2)
                                             (unique ?source-10 ?target-66128)
                                             (bind color-category ?color-12 purple)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-2 ?target-1 ?color-12)
                                             (bind size-category ?size-2 small)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The tiny blue object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-93243 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-93243)
                                         (bind size-category ?size-2 small)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind color-category ?color-6 blue)
                                         (bind attribute-category ?attribute-2 shape)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-reordered-form-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy and reordering. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "What shape is the small purple thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-66128 ?target-2 ?size-2)
                                             (unique ?source-10 ?target-66128)
                                             (bind color-category ?color-12 purple)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-2 ?target-1 ?color-12)
                                             (bind size-category ?size-2 small)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'nothing->holistic
                        (second (multiple-value-list
                                 (comprehend "The tiny blue object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-93243 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-93243)
                                         (bind size-category ?size-2 small)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind color-category ?color-6 blue)
                                         (bind attribute-category ?attribute-2 shape)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-varying-word-order-substitution-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The tiny gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'nothing->holistic
                        (second (multiple-value-list
                                 (comprehend "What is the material of the tiny gray object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (bind attribute-category ?attribute-8 material)
                                       (bind size-category ?size-2 small)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (unique ?target-object-1 ?target-77105)
                                       (filter ?target-77105 ?target-2 ?size-2)
                                       (query ?target-4 ?target-object-1 ?attribute-8))))))))


(deftest test-varying-length-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-14197 ?target-1 ?color-16)
                                         (query ?target-8 ?source-10 ?attribute-2))))))))
(deftest test-varying-length-substitution-repair-comprehension-reversed ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-14197 ?target-1 ?color-16)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-discontinuous-substitution-common-middle-element ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "How many brown shiny objects are there?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)
                                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-5862 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?COLOR-10)
                                         (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
                                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8)
                                         (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING)
                                         (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
                                         (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-10 CLEVR-WORLD:BROWN)
                                         (CLEVR-WORLD:COUNT! GRAMMAR-LEARNING::?TARGET-16 GRAMMAR-LEARNING::?TARGET-5862)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "How many big green shiny blocks are there?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1)
                                                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-9641 GRAMMAR-LEARNING::?TARGET-9626 GRAMMAR-LEARNING::?SIZE-4)
                                                                      (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-8 CLEVR-WORLD:GREEN)
                                                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?MATERIAL-4)
                                                                      (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-2 CLEVR-WORLD:CUBE)
                                                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-2)
                                                                      (UTILS:BIND CLEVR-WORLD:MATERIAL-CATEGORY GRAMMAR-LEARNING::?MATERIAL-4 CLEVR-WORLD:METAL)
                                                                      (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-9626 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?COLOR-8)
                                                                      (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-4 CLEVR-WORLD:LARGE)
                                                                      (CLEVR-WORLD:COUNT! GRAMMAR-LEARNING::?TARGET-16 GRAMMAR-LEARNING::?TARGET-9641))))))))




;; (activate-monitor trace-fcg)
;; (test-substitution-repair-comprehension) ;ok
;; (test-substitution-repair-comprehension-right) ;ok
;; (test-substitution-repair-comprehension-multi-diff) ;ok
;; (test-double-substitution-repair-comprehension) ;ok
;; (test-double-discontinuous-substitution-repair-comprehension) ok
;; (test-triple-substitution-repair-comprehension) ;ok
;; (test-reordered-form-substitution-repair-comprehension) ;should be holophrase
;; (test-varying-word-order-substitution-comprehension) ;should be holophrase
;; (test-varying-length-substitution-repair-comprehension) ;ok
;; (test-varying-length-substitution-repair-comprehension-reversed) ;ok
;; (test-discontinuous-substitution-common-middle-element)

