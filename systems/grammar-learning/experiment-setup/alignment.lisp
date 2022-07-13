;;;; alignment.lisp

(in-package :grammar-learning)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

(defun alter-ego-cxn (original-cxn cxn-inventory)
  (when (attr-val original-cxn :bare-cxn-name)
    (loop for cxn in (constructions cxn-inventory)
          when (and (attr-val cxn :bare-cxn-name)
                    (eq (attr-val cxn :bare-cxn-name) (attr-val original-cxn :bare-cxn-name))
                    (not (eql (name cxn) (name original-cxn))))
          do (return cxn))))
  
(defmethod run-alignment ((agent clevr-learning-learner)
                          cipn (strategy (eql :lateral-inhibition)))

  (notify alignment-started)
  ;; align categorial links
  (loop for (cat-1 . cat-2) in (extract-used-categorial-links cipn)
        do (incf-link-weight cat-1 cat-2 (categorial-network (construction-inventory cipn))))

  ;; comprehend-all
  (set-configuration (grammar agent) :use-meta-layer nil)

  (multiple-value-bind (meanings solution-cipns)
      (comprehend-all (utterance agent)
                      :silent t
                      :gold-standard-meaning (list (meaning agent))
                      :cxn-inventory (grammar agent))
    (let* ((cxns-to-punish-solutions (loop for solution-node in solution-cipns
                                           for competitor-cxns = (set-difference (applied-constructions solution-node) (applied-constructions cipn) :key #'name)
                                           append (mapcar #'original-cxn competitor-cxns)))
           (cxns-to-punish-non-solutions
            (loop for current-node in (traverse-depth-first (top-node (cip cipn)) :collect-fn #'identity)
                  when (and (field? (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)
                            (not (get-data (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)))
                  append (loop for bad-node in (set-difference (remove (top-node (cip cipn))
                                                                       (cons current-node (all-parents current-node)))
                                                               (remove (top-node (cip cipn))
                                                                       (cons cipn (all-parents cipn)))
                                                               :key #'(lambda (node) (name (car-applied-cxn (cipn-car node)))))
                               collect (original-cxn (car-applied-cxn (cipn-car bad-node))))))
           (cxns-to-punish (remove-duplicates (append cxns-to-punish-solutions cxns-to-punish-non-solutions))))
      (dolist (cxn cxns-to-punish)
        (dec-cxn-score agent cxn))
                     
    
      (set-configuration (grammar agent) :use-meta-layer t)

      ;; align cxns
      (let ((applied-cxns (original-applied-constructions cipn)))
    
        ;; reward the applied cxns 
        (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
              for cxn in applied-cxns
              for alter-ego-cxn = (alter-ego-cxn cxn (grammar agent))
              do (inc-cxn-score cxn :delta cxn-delta)
              (inc-cxn-score alter-ego-cxn :delta cxn-delta)
              finally (notify cxns-rewarded applied-cxns))
    
        ))))

      
