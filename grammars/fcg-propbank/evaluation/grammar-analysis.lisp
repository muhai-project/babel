(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar Analysis        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains functions to investigate the inner workings of a
;; learned grammar. It allows you to:
;; - Find constructions based on a schema
;; - Find example utterances in which a schema is instantiated
;; - Find lexical items that fill the V slot of a schema (sorted)
;; - Find all schemata that a lexical item appears in (sorted)
;; - Find closest lexical items for a given lexical item



;; Restore a grammar to analyse:
;;---------------------------------------------------------------------------------------

#|(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                           :name "propbank-grammar-ontonotes-ewt-cleaned-300"
                           :type "fcg")))|#





;; Finding constructions by schema
;;---------------------------------------------------------------------------------------

(defun find-constructions-by-schema (schema cxn-inventory &key (collect-fn #'identity))
  "Returns the constructions of which the schema matches the given schema."
  (loop for cxn in (constructions-list cxn-inventory)
        for cxn-schema = (attr-val cxn :schema)
        if (and schema
                (unify schema cxn-schema))
        collect (funcall collect-fn cxn)))


#|
;; Find dative constructions:
(pprint (find-constructions-by-schema '((:arg0  np)
                                        (:v v)
                                        (:arg1 np)
                                        (:arg2  pp))
                                      *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*
                                      :collect-fn #'name))


;; Find example utterances of the dative schema:
(pprint (find-constructions-by-schema '((:arg0  np)
                                        (:v v)
                                        (:arg1 np)
                                        (:arg2 pp))
                                      *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*
                                      :collect-fn #'(lambda (cxn) (fcg::description cxn))))


;; Find ditransitive constructions:
(pprint (find-constructions-by-schema '((:arg0  np)
                                        (:v v)
                                        (:arg2 np)
                                        (:arg1 np))
                                      *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*
                                      :collect-fn #'name))

;; Find example utterances of the dative schema:
(pprint (find-constructions-by-schema '((:arg0  np)
                                        (:v v)
                                        (:arg2 np)
                                        (:arg1 np))
                                      *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*
                                      :collect-fn #'(lambda (cxn) (fcg::description cxn)))) |#


;; Finding constructions by schema
;;---------------------------------------------------------------------------------------

(defun rolesets-for-schema (schema cxn-inventory &key (edge-type 'gram-sense ))
  "Find all lexical items that fill the V slot of the given schema and
return them acccording to the edge weights."
  (let* ((gram-categories (find-constructions-by-schema schema cxn-inventory
                                                        :collect-fn #'(lambda (cxn)
                                                                        (attr-val cxn :argst-category))))
         (graph (fcg::graph (categorial-network cxn-inventory)))
         (lex-items-with-frequency (loop with lexical-items-with-frequency = (make-hash-table)
                                         for g in gram-categories
                                         for node-vector = (graph-utils::node-vector  g graph :edge-type edge-type)
                                         do (loop for lex-item being the hash-keys in node-vector using (hash-value frequency)
                                                  for lex-item-symbol = (graph-utils::lookup-node graph lex-item)
                                                  if (gethash lex-item-symbol lexical-items-with-frequency)
                                                    do (incf (gethash lex-item-symbol lexical-items-with-frequency) frequency)
                                                  else do (setf (gethash lex-item-symbol lexical-items-with-frequency) frequency))
                                         finally (return lexical-items-with-frequency))))
    
    (loop for lex-item being the hash-keys in lex-items-with-frequency using (hash-value frequency)
          collect (cons lex-item frequency) into lex-items-with-frequency-list
          finally (return (sort lex-items-with-frequency-list #'> :key #'cdr)))))
          
#|
;; Lexical items that are used in the to-dative schema (with edge weight):
(pprint (rolesets-for-schema '((:arg0  np)
                                (:v v)
                                (:arg1 ?y)
                                (:arg2  pp))
                              *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*))


;; Lexical items that are used in the ditransitive schema (with edge weight):
(pprint (rolesets-for-schema '((:arg0  np)
                                (:v v)
                                (:arg2 np)
                                (:arg1  np))
                              *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*))


;; Lexical items used in ergative/passive schema:
(pprint (rolesets-for-schema '((:arg1 np)
                                (:v v))
                              *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*)) |#




;; Finding schemata for a lexical item
;;---------------------------------------------------------------------------------------

(defun make-symbol-for-schema (schema)
  (loop for slot in schema
        collect (string-append (mkstr (car slot))
                               (mkstr (cdr slot))) into schema-string
        finally (return (intern (format nil "~{~a~^+~}" schema-string)))))

;;(make-symbol-for-schema '((arg0 np) (v v) (arg2 pp)))
        

(defun find-schemata-for-roleset (roleset-cat cxn-inventory &key (edge-type 'gram-sense))
  "Find all schemata that a lexical item occurred in and rank them by
the sum of their edge weights."
  (loop with categorial-network = (categorial-network cxn-inventory)
        with schemata = (make-hash-table)
        for gram-category in (graph-utils::neighbors (fcg::graph categorial-network)
                                                     roleset-cat :return-ids? nil :edge-type edge-type)
        for cxn = (find gram-category (gethash nil (constructions-hash-table cxn-inventory))
                        :key #'(lambda (c) (attr-val c :argst-category)))
        when cxn
        do (let ((schema-key (make-symbol-for-schema (attr-val cxn :schema))))
          (if (gethash schema-key schemata)
            (setf (gethash schema-key schemata) (+ (gethash schema-key schemata)
                                              (graph-utils:edge-weight (fcg::graph categorial-network)
                                                                       roleset-cat gram-category)))
            (setf (gethash schema-key schemata) (graph-utils:edge-weight (fcg::graph categorial-network)
                                                                       roleset-cat gram-category))))
        finally (return (loop for schema being the hash-keys in schemata using (hash-value frequency)
                              collect (cons schema frequency) into schemata-w-frequency
                              finally (return (sort schemata-w-frequency #'> :key #'cdr))))))


;; Inspect first the nodes of the categorial network to get node ids:
;(defparameter *categorial-network* (categorial-network *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*))


;; Find all schemata for the verb lemma 'explain':
;(pprint (find-schemata-for-roleset 'TELL.01-1553 *propbank-grammar-ontonotes-ewt-core-roles-full-corpus*))




;; Find closest lexical items
;;---------------------------------------------------------------------------------------

;; Inspect first the nodes of the categorial network to get node ids:
;; (defparameter *th* (type-hierarchies:get-type-hierarchy *restored-grammar*))

;; (pprint (closest-nodes 'propbank-english::EXPLAIN\(V\)-73 (graph *th*) :edge-type 'propbank-english::lex-gram))
