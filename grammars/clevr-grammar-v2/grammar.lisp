;;;; grammar.lisp

(in-package :clevr-grammar-v2)

(def-fcg-constructions clevr-grammar
    :feature-types
    ((args set-of-predicates)
     (form set-of-predicates)
     (meaning set-of-predicates)
     (subunits set)
     (superunits set)
     (footprints set))    
    :fcg-configurations
    ((:de-render-mode . :clevr-de-renderer)
     (:render-mode . :generate-and-test)
     (:create-initial-structure-mode . :clevr-initial-structure)
     (:form-predicates meets precedes)
     (:node-tests :check-duplicate
      :connected-structure-for-morph
      :restrict-nr-of-nodes)
     (:parse-goal-tests :no-applicable-cxns
      :connected-semantic-network
      :connected-structure
      :no-strings-in-root)
     (:production-goal-tests :no-applicable-cxns
      :connected-structure
      :no-meaning-in-root)

    
     
     ;; For heuristic search with seq2seq:
     ;(:cxn-supplier-mode . :hashed+seq2seq-heuristic)
     ;(:priority-mode . :seq2seq-heuristic-additive)
     ;(:seq2seq-endpoint . "http://localhost:8888/next-cxn")
     ;(:seq2seq-model-comprehension . "clevr_comprehension_model")
     ;(:seq2seq-model-formulation . "clevr_formulation_model")
     ;(:seq2seq-rpn-fn . clevr-meaning->rpn)

     ;; depth first search
     (:cxn-supplier-mode . :ordered-by-label-hashed)
     (:priority-mode . :nr-of-applied-cxns)
     (:parse-order hashed nom cxn)
     (:production-order hashed-lex nom cxn hashed-morph)
     
     ;; For guiding search:
     (:cxn-sets-with-sequential-application hashed-lex hashed-morph)
     (:node-expansion-mode . :multiple-cxns)
     (:queue-mode . :greedy-best-first)
     (:max-nr-of-nodes . 10000)
     (:hash-mode . :hash-string-meaning-lex-id))
    :visualization-configurations
    ((:show-constructional-dependencies . nil)
     (:hide-features . nil) ; (footprints superunits)
     (:with-search-debug-data . t))
    :hierarchy-features (subunits)
    :hashed t
    :cxn-inventory *CLEVR*
    (generate-lexical-constructions *CLEVR*)
    (generate-morphological-constructions *CLEVR*)
    )

;; This is to be able to call comprehend and formulate without specifying the cxn-inventory
; (setf *fcg-constructions* *CLEVR*)
