(in-package :fcg)

;; #############################################################################################
;;                                                                                               ;;
;; Functionality for exporting Common Lisp FCG grammars into the Open FCG Exchange Format (OFEF) ;;
;;                                                                                               ;;
;; #############################################################################################


;; There are two export methods - the regular one 'export-ofef that specialises on classes and
;; the special one export-ofef-special that specialises on a mode that is passed.

(defgeneric export-ofef (thing)
  (:documentation "Exports thing in the Open FCG Exchange Format (ofef)"))

(defgeneric export-ofef-special (thing mode)
  (:documentation "Exports thing according to mode in the Open FCG Exchange Format (ofef)"))

;;;;;;;;;;;;;;;;;
;; export-ofef ;;
;;;;;;;;;;;;;;;;;


;; Basic types ;;
;;;;;;;;;;;;;;;;;

(defmethod export-ofef ((number number))
  "Export number."
  number)

(defmethod export-ofef ((string string))
  "Export string."
  (format nil "\"\\\"~(~a~)\\\"\"" string))

(defmethod export-ofef ((symbol symbol))
  "Export symbol."
  (cond ((eq symbol t)
         "true")
        ((eq symbol nil)
         "false")
        ((keywordp symbol)
         (format nil "\":~(~a~)\"" symbol))
        (t
         (format nil "\"~(~a~)\"" symbol)))) 

(defmethod export-ofef ((list list))
  "Export list."
  (if (listp (cdr list))
    ;; proper list
    (loop for element in list
          collect (export-ofef element) into ofef-elements
          finally (return (if (<= (length ofef-elements) 3)
                              (format nil "[~{~a~^, ~}]" ofef-elements)
                              (format nil "[~{~a~^,~% ~}]" ofef-elements))))
    ;; dotted pair
    (format nil "{~a: ~a}" (export-ofef (car list)) (export-ofef (cdr list)))))

(defmethod export-ofef ((function function))
  "Ofef-export of function object."
  (format nil "\"#'~(~a~)\"" (third (multiple-value-list (function-lambda-expression function)))))


;; FCG classes ;;
;;;;;;;;;;;;;;;;;

(defmethod export-ofef ((grammar fcg-construction-set))
  "Ofef-export of fcg-construction-set."
  (format nil
          "{~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a}"
          (export-ofef 'hashed) (if (hashed-cxn-inventory-p grammar) (export-ofef t) (export-ofef nil))
          (export-ofef 'configuration) (export-ofef (configuration grammar))
          (export-ofef 'visualization-configuration) (export-ofef (visualization-configuration grammar))
          (export-ofef 'feature-types) (export-ofef (feature-types grammar))
          (export-ofef 'hierarchy-features) (export-ofef (hierarchy-features grammar))
          (export-ofef 'categorial-network) (export-ofef (categorial-network grammar))
          (export-ofef 'cxns) (export-ofef-special (constructions-list grammar) :constructions-list)))


(defmethod export-ofef ((cxn fcg-construction))
  "Export construction."
  (let ((cxn-name (name cxn)))
    (format nil "~a: {~%\"name\": ~a, ~%\"contributing-pole\": ~a, ~%\"conditional-pole\": ~a, ~%\"attributes\": ~a, ~%\"feature-types\": ~a}"
            (export-ofef cxn-name)
            (export-ofef cxn-name)
            (export-ofef (contributing-part cxn))
            (export-ofef (conditional-part cxn))
            (export-ofef-special (attributes cxn) :alist)
            (export-ofef (feature-types cxn)))))

(defmethod export-ofef ((unit contributing-unit))
  "Export contributing unit."
  )

(defmethod export-ofef ((unit conditional-unit))
  "Export conditional unit."
  )

(defmethod export-ofef ((configuration configuration))
  "Export configuration."
  (export-ofef (configuration configuration)))

(defmethod export-ofef ((hash-table hash-table))
  "Export hash-table"
  (loop for key being the hash-keys of hash-table using (hash-value value)
        collect (format nil "~a: ~a" (export-ofef-special key :eliminate-keyword-colon) (export-ofef value))
          into feature-value-pairs
        finally (return (format nil "{~{~a~^,~%~}}" feature-value-pairs))))

(defmethod export-ofef ((network categorial-network))
  "Export configuration."
  (format nil "{~a: ~a,~%~a: ~a}"
          (export-ofef 'nodes) (export-ofef (categories network))
          (export-ofef 'edges) (export-ofef (links network))))







;; (load-demo-grammar)
;; (categorial-network *fcg-constructions*)
;; (pprint (export-ofef *fcg-constructions*))


(add-categories '(a b c hello) *fcg-constructions*)
(add-link 'a 'c *fcg-constructions*)
(add-link 'c 'b *fcg-constructions*)
(add-link 'a 'hello *fcg-constructions*)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; export-ofef-special ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod export-ofef-special (symbol (mode (eql :eliminate-keyword-colon)))
  "Ofef-export of feature-value pair."
  (format nil "\"~(~a~)\"" symbol))


(defmethod export-ofef-special (cxns (mode (eql :constructions-list)))
  "Ofef-export of feature-value pair."
  (loop for cxn in cxns
        collect (export-ofef cxn) into ofef-cxns
        finally (return (format nil "{~{~a~^,~%~}}" ofef-cxns))))

(defmethod export-ofef-special (alist (mode (eql :alist)))
  "Ofef-export of feature-value pair."
  (loop for (key . value) in alist
        for ofef-key = (export-ofef-special key :eliminate-keyword-colon)
        for ofef-value = (export-ofef value)
        collect (format nil "~a: ~a" ofef-key ofef-value) into ofef-alist
        finally (return (format nil "{~{~a~^,~%~}}" ofef-alist))))

(defmethod export-ofef-special (feature-value-pairs (mode (eql :list-of-feature-value-pairs)))
  "Ofef-export of feature-value pair."
  (loop for (feature . value) in feature-value-pairs
          collect (format nil "~a: ~a" (export-ofef feature) (export-ofef value)) into ofef-pairs
        finally (return (format nil "{~{~a~^,~%~}}" ofef-pairs))))





#|

;; Testing 
;; #######

(load-demo-grammar)
(export-ofef (configuration *fcg-constructions*))
(export-ofef 50)
(export-ofef 'katrien-paul)
(export-ofef :hello)
(format t (export-ofef "hello"))
(export-ofef 'hello)
(export-ofef '?hello)
(export-ofef t)
(export-ofef nil)
(export-ofef '(a . b))
(export-ofef '(a b c))
(export-ofef '(hallo 50 "car" (sublist a b)))

(export-ofef-special (attributes (first (constructions-list  *fcg-constructions*))) :alist)
(json:decode-json-from-string "{\":hello\": \"hello\"}")




|#
