(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Descriptive statistics of a learnt grammar   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grammar-report (grammar)
  "Writes a report on the grammar to Output."
  (format t "~%~%---------------------------------------------------------------------------------~%")
  (format t "GRAMMAR REPORT FOR ~a (~a)~%" (name grammar) grammar)
  (format t "---------------------------------------------------------------------------------~%~%")


  (let ((sum-average-median-non-hapax-frequency (multiple-value-list (sum-average-median-non-hapax-frequency grammar)))
        (fe-cxn-sum-average-median-non-hapax-frequency (multiple-value-list
                                                        (sum-average-median-non-hapax-frequency grammar
                                                                                                :type 'lexical-cxn)))
        (argst-cxn-sum-average-median-non-hapax-frequency (multiple-value-list
                                                           (sum-average-median-non-hapax-frequency grammar
                                                                                                   :type 'argument-structure-cxn)))
        (roleset-cxn-sum-average-median-non-hapax-frequency (multiple-value-list
                                                             (sum-average-median-non-hapax-frequency grammar
                                                                                                     :type 'word-sense-cxn)))
        (nr-of-fe-cxns (nr-of-cxns-of-type grammar 'lexical-cxn))
        (nr-of-argst-cxns (nr-of-cxns-of-type grammar 'argument-structure-cxn))
        (nr-of-roleset-cxns (nr-of-cxns-of-type grammar 'word-sense-cxn))
        (total-nr-of-cxns (size grammar)))

      
    (format t "Number of constructions:~%")
    (format t "   All cxns: ~a~%" total-nr-of-cxns)
    (format t "   Frame-evoking cxns: ~a~%" nr-of-fe-cxns)
    (format t "   Argument structure cxns: ~a~%" nr-of-argst-cxns )
    (format t "   Roleset cxns: ~a~%" nr-of-roleset-cxns)
    (format t "~%---------------------------------------------------------------------------------~%~%")
    (format t "Individual construction frequency information:~%")
    (format t "   All cxns: ~%" )
    (format t "      Absolute frequency: ~a ~%" (first sum-average-median-non-hapax-frequency))
    (format t "      Mean frequency: ~a ~%" (second sum-average-median-non-hapax-frequency))
    (format t "      Median frequency: ~a ~%" (third sum-average-median-non-hapax-frequency))
    (format t "      Number of non-hapax cxns: ~a of ~a ~%" (fourth sum-average-median-non-hapax-frequency) total-nr-of-cxns)

    (format t "   Frame-evoking cxns: ~%" )
    (format t "      Absolute frequency: ~a ~%" (first fe-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Mean frequency: ~a ~%" (second fe-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Median frequency: ~a ~%" (third fe-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Number of non-hapax cxns: ~a of ~a ~%" (fourth fe-cxn-sum-average-median-non-hapax-frequency) nr-of-fe-cxns)

`    (format t "   Argument structure cxns: ~%" )
    (format t "      Absolute frequency: ~a ~%" (first argst-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Mean frequency: ~a ~%" (second argst-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Median frequency: ~a ~%" (third argst-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Number of non-hapax cxns: ~a of ~a ~%" (fourth argst-cxn-sum-average-median-non-hapax-frequency) nr-of-argst-cxns)

    (format t "   Roleset cxns: ~%" )
    (format t "      Absolute frequency: ~a ~%" (first roleset-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Mean frequency: ~a ~%" (second roleset-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Median frequency: ~a ~%" (third roleset-cxn-sum-average-median-non-hapax-frequency))
    (format t "      Number of non-hapax cxns: ~a of ~a ~%" (fourth roleset-cxn-sum-average-median-non-hapax-frequency) nr-of-roleset-cxns)
    (format t "~%---------------------------------------------------------------------------------~%~%")
    (format t "Construction network information:~%")
    (format t "   Average degree: ~a ~%" (average-degree grammar))
    
    (format t "~%---------------------------------------------------------------------------------~%")
    ))

;; (grammar-report *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*)

(defun nr-of-cxns-of-type (grammar type)
  "Count nr of type in grammar."
  (loop for cxn in (constructions-list grammar)
        count (eql type (attr-val cxn :label))))

(defun sum-average-median-non-hapax-frequency (grammar &key (type nil))
  "Returns the sum, average and median construction frequency of type in grammar."
  (loop for cxn in (constructions-list grammar)
        when (or (not type)
                 (eql type (attr-val cxn :label)))
          collect (attr-val cxn :score) into frequencies
        finally (return (values (sum frequencies)
                                (average frequencies)
                                (median frequencies)
                                (count-if #'(lambda (freq) (> freq 1)) frequencies)))))
        
;; (sum-average-median-non-hapax-frequency *propbank-grammar-ontonotes-ewt-core-roles* :type 'word-sense-cxn)


       
(defun average-degree (grammar)
  "Computes average degree of nodes in grammar network"
  (loop for cxn in (constructions-list grammar)
        for cxn-cat = (or (attr-val cxn :fe-category)
                          (attr-val cxn :argst-category)
                          (attr-val cxn :roleset-category))
        for node-id = (gethash cxn-cat (graph-utils::nodes (fcg::graph (categorial-network grammar))))
        for neighbours-hash-table = (gethash node-id (graph-utils::matrix
                                                      (gethash nil (graph-utils::matrix (fcg::graph (categorial-network grammar))))))
        collect (hash-table-count neighbours-hash-table) into degrees
        finally (return (average degrees))))
        
;; (average-degree *grammar*)