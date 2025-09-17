(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;; Functionality for comprehension and production ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod comprehend ((corpus-processor corpus-processor) &key (cxn-inventory *fcg-constructions*) (nr-of-speech-acts 1)
                       (silent nil) (learn t) (align t) (consolidate t) (n nil)  &allow-other-keys)
  "Comprehends the next nr-of-speech-acts speech acts of corpus-processor."
  (loop for i from 1 upto nr-of-speech-acts
        do (unless silent (notify next-speech-act corpus-processor))
           (comprehend (next-speech-act corpus-processor) :cxn-inventory cxn-inventory :silent silent :learn learn
                       :align align :consolidate consolidate :n n)
           (unless silent (notify speech-act-finished corpus-processor))))


(defmethod comprehend ((speech-act speech-act) &key (cxn-inventory *fcg-constructions*)
                       (silent nil) (learn t) (align t) (consolidate t) (n nil)  &allow-other-keys)
  "Comprehend speech act, with or without learning and alignment."
  ;; Store speech act in blackboard of cxn-inventory, add diagnostics to cip
  ;; and notify that routine processing starts
  (set-data (blackboard cxn-inventory) :speech-act speech-act)
  (unless silent (notify routine-comprehension-started speech-act n))
  
  ;; Compute result of routine processing, then (optinally) learn, (optionally) align and return.
  (let* ((cip (second (multiple-value-list (fcg-apply-with-n-solutions (processing-cxn-inventory cxn-inventory)
                                                                       (de-render speech-act (get-configuration cxn-inventory :de-render-mode))
                                                                       '<- n
                                                                       :notify (not silent)))))
         (solution-node nil))

    (annotate-cip-with-used-categorial-links cip)
    (setf solution-node (best-solution cip (get-configuration cxn-inventory :best-solution-mode))) ; can be nil

    (if (and solution-node
             (gold-standard-solution-p (car-resulting-cfs (cipn-car solution-node)) speech-act (direction cip) (configuration cxn-inventory)))
      (set-data cip :best-solution-matches-gold-standard t)
      (set-data cip :best-solution-matches-gold-standard nil))
    
    (unless silent (notify routine-comprehension-finished solution-node cip))

    (let* ((node-and-fixes-from-learning (when learn
                                           (multiple-value-list (learn solution-node
                                                                       cip
                                                                       consolidate
                                                                       (get-configuration cxn-inventory :learning-mode) :silent silent))))
           (solution-node-after-learning (first node-and-fixes-from-learning))
           (applied-fixes (second node-and-fixes-from-learning)))

    
      (when (and solution-node align)
        (align solution-node cip (get-configuration cxn-inventory :alignment-mode)))

      ;; Return
      (cond ((and solution-node (not applied-fixes))
             (values (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution-node))))
                     solution-node
                     cip))
            ((and (not solution-node) (not applied-fixes))
             (values nil nil cip))
            ((and solution-node-after-learning applied-fixes)
             (values (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution-node-after-learning))))
                     solution-node-after-learning
                     cip))
            (t
             (warn "Fixes were applied, but did not yield a solution.")
             (values nil nil cip))))))

;; De-rendering ;;
;;;;;;;;;;;;;;;;;;

(defmethod de-render ((speech-act speech-act) mode &key &allow-other-keys)
  "De-render form of speech act."
  (de-render (form speech-act) mode))