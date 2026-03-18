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
        (total-nr-of-cxns (size grammar))
        (average-and-median-network-degree (multiple-value-list (average-degree grammar)))
        (average-and-median-degree-fe-argst (multiple-value-list (average-degree grammar :from-to '(fe argst))))
        (average-and-median-degree-argst-fe (multiple-value-list (average-degree grammar :from-to '(argst fe))))
        (average-and-median-degree-argst-roleset (multiple-value-list (average-degree grammar :from-to '(argst roleset))))
        (average-and-median-degree-roleset-argst (multiple-value-list (average-degree grammar :from-to '(roleset argst))))
        (average-and-median-degree-roleset-fe (multiple-value-list (average-degree grammar :from-to '(roleset fe))))
        (average-and-median-degree-fe-roleset (multiple-value-list (average-degree grammar :from-to '(fe roleset)))))
      
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

    (format t "   Argument structure cxns: ~%" )
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
    (format t "   All cxns: ~%" )
    (format t "     Average degree: ~a ~%" (first average-and-median-network-degree))
    (format t "     Median degree: ~a ~%~%" (second average-and-median-network-degree))

    (format t "   Average degrees: ~%" )
    (format t "     Frame-evoking -> Argument structure cxns: ~a ~%" (first average-and-median-degree-fe-argst))
    (format t "     Argument structure -> Frame-evoking cxns: ~a ~%" (first average-and-median-degree-argst-fe))
    (format t "     Frame-evoking -> Roleset cxns: ~a ~%" (first average-and-median-degree-fe-roleset))
    (format t "     Roleset -> Frame-evoking cxns: ~a ~%" (first average-and-median-degree-roleset-fe))
    (format t "     Argument structure -> Roleset cxns: ~a ~%" (first average-and-median-degree-argst-roleset))
    (format t "     Roleset -> Argument structure cxns: ~a ~%" (first average-and-median-degree-roleset-argst))
    
    (format t "~%---------------------------------------------------------------------------------~%")
    ))

;; (grammar-report *grammar*)

;(add-element (make-html (find-cxn 'ARG0\(NP\)+V\(V\)+ARG1\(NP\)-8+3-CXN  *grammar*)))
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

   
(defun degrees-sorted (grammar &key from-to)
  "Computes average degree of nodes in grammar network, returning the median as a second value."
  (loop for cxn in (constructions-list grammar)
        for cxn-cat = (cond ((eql (first from-to) 'fe)
                             (attr-val cxn :fe-category))
                            ((eql (first from-to) 'argst)
                             (attr-val cxn :argst-category))
                            ((eql (first from-to) 'roleset)
                             (attr-val cxn :roleset-category))
                            (t
                             (or (attr-val cxn :fe-category)
                                 (attr-val cxn :argst-category)
                                 (attr-val cxn :roleset-category))))
          when cxn-cat
          collect (let* ((node-id (gethash cxn-cat (graph-utils::nodes (fcg::graph (categorial-network grammar)))))
                         (edge-type (cond ((or (equalp from-to '(fe argst))
                                               (equalp from-to '(argst fe)))
                                           'lex-gram)
                                          ((or (equalp from-to '(roleset argst))
                                               (equalp from-to '(argst roleset)))
                                           'gram-sense)
                                          ((or (equalp from-to '(fe roleset))
                                               (equalp from-to '(roleset fe)))
                                           'lex-sense)
                                          (t nil)))
                         (neighbours-hash-table (gethash node-id (graph-utils::matrix
                                                                  (gethash edge-type (graph-utils::matrix
                                                                                      (fcg::graph (categorial-network grammar))))))))
                    (hash-table-count neighbours-hash-table)) into degrees
        finally (return (values (sort degrees  #'>) (median degrees)))))




#|
(progn
  (setf *fe->argst* (degrees-sorted *grammar* :from-to '(fe argst)))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "fe->argst"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *fe->argst*) f))

(setf *argst->fe* (degrees-sorted *grammar* :from-to '(argst fe)))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "argst->fe"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *argst->fe*) f))


(setf *argst->roleset* (degrees-sorted *grammar* :from-to '(argst roleset)))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "argst->roleset"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *argst->roleset*) f))

(setf *roleset->argst* (degrees-sorted *grammar* :from-to '(roleset argst))) 

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "roleset->argst"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *roleset->argst*) f))

(setf *roleset->fe* (degrees-sorted *grammar* :from-to '(roleset fe)))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "roleset->fe"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *roleset->fe*) f))


(setf *fe->roleset* (degrees-sorted *grammar* :from-to '( fe roleset)))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "fe->roleset"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *fe->roleset*) f))

;(ql:quickload :plot-raw-data)

(plot-raw-data::raw-files->evo-plot  
 :raw-file-paths '((".tmp" "fe->argst")
                   (".tmp" "argst->fe")
                   (".tmp" "argst->roleset")
                   (".tmp" "roleset->argst")
                   (".tmp" "roleset->fe")
                   (".tmp" "fe->roleset"))
 :logscale 'xy
 :y1-label "Number of links (log)"
 :x-label "Rank (log)"
 ;:colors '("medium-blue")
 ;:captions '("Frame-evoking cxns" "Argument structure cxns" "Roleset cxns")
 :fsize 11
 :grid-line-width 0.1
 :key-box t
 :key-location 'top
 :line-width 2.5
 :step 1
  ))|#


(defun average-degree (grammar &key from-to)
  "Computes average degree of nodes in grammar network, returning the median as a second value."
  (loop for cxn in (constructions-list grammar)
        for cxn-cat = (cond ((eql (first from-to) 'fe)
                             (attr-val cxn :fe-category))
                            ((eql (first from-to) 'argst)
                             (attr-val cxn :argst-category))
                            ((eql (first from-to) 'roleset)
                             (attr-val cxn :roleset-category))
                            (t
                             (or (attr-val cxn :fe-category)
                                 (attr-val cxn :argst-category)
                                 (attr-val cxn :roleset-category))))
          when cxn-cat
          collect (let* ((node-id (gethash cxn-cat (graph-utils::nodes (fcg::graph (categorial-network grammar)))))
                         (edge-type (cond ((or (equalp from-to '(fe argst))
                                               (equalp from-to '(argst fe)))
                                           'lex-gram)
                                          ((or (equalp from-to '(roleset argst))
                                               (equalp from-to '(argst roleset)))
                                           'gram-sense)
                                          ((or (equalp from-to '(fe roleset))
                                               (equalp from-to '(roleset fe)))
                                           'lex-sense)
                                          (t nil)))
                         (neighbours-hash-table (gethash node-id (graph-utils::matrix
                                                                  (gethash edge-type (graph-utils::matrix
                                                                                      (fcg::graph (categorial-network grammar))))))))
                    (hash-table-count neighbours-hash-table)) into degrees
        finally (return (values (average degrees) (median degrees)))))
        
;; (average-degree *grammar* :from-to '(fe argst))
;; (average-degree *grammar* :from-to '(argst fe))



(defun weighted-average-degree (grammar)
  "Computes the weighted average degree of nodes in grammar network"
  (loop for cxn in (constructions-list grammar)
        for cxn-cat = (or (attr-val cxn :fe-category)
                          (attr-val cxn :argst-category)
                          (attr-val cxn :roleset-category))
        for node-id = (gethash cxn-cat (graph-utils::nodes (fcg::graph (categorial-network grammar))))
        for neighbours-hash-table = (gethash node-id (graph-utils::matrix
                                                      (gethash nil (graph-utils::matrix (fcg::graph (categorial-network grammar))))))
        collect (loop for value being the hash-values of neighbours-hash-table
                      collect value into weighted-values
                      finally (return (* (average weighted-values)
                                         (hash-table-count neighbours-hash-table)))) into weighted-degrees
        finally (return (average weighted-degrees))))

;; (weighted-average-degree *grammar*)

(defun cxn-network-connection-strengths (grammar)
  "Returns the weights in the construction network, ordered by strength."
  (loop for cxn in (constructions-list grammar)
        for cxn-cat = (or (attr-val cxn :fe-category)
                          (attr-val cxn :argst-category)
                          (attr-val cxn :roleset-category))
        for node-id = (gethash cxn-cat (graph-utils::nodes (fcg::graph (categorial-network grammar))))
        for neighbours-hash-table = (gethash node-id (graph-utils::matrix
                                                      (gethash nil (graph-utils::matrix (fcg::graph (categorial-network grammar))))))
        append (loop for neighbour being the hash-keys of neighbours-hash-table
                        collect (gethash neighbour neighbours-hash-table) into cxn-connection-strengths
                        finally (return cxn-connection-strengths)) into network-connection-strengths
        finally (return (sort network-connection-strengths #'>))))

#|(setf *strengths* (cxn-network-connection-strengths *grammar*))

(with-open-file (f (babel-pathname :directory '(".tmp")
                                   :name "cxn-connection-strenghts"
                                   :type "lisp")
                   :if-does-not-exist :create :direction :output :if-exists :supersede)
  (write-line (format nil "((~a))" *strengths*) f))

;(ql:quickload :plot-raw-data)

(plot-raw-data::raw-files->evo-plot  
 :raw-file-paths '((".tmp" "cxn-connection-strenghts"))
 ;:average-windows 1
 :logscale 'xy
 :y1-label "Connection strength (log)"
 :x-label "Rank (log)"
 :colors '("medium-blue")
 ;:captions '("Frame-evoking cxns" "Argument structure cxns" "Roleset cxns")
 :fsize 11
 :grid-line-width 0.1
 :line-width 2.5
 ;:end 1000
 ;:step 1
  )|#