(in-package :fcg)

;; #############################################################################################
;;                                                                                               ;;
;; Functionality for exporting Common Lisp FCG grammars into the Open FCG Exchange Format (OFEF) ;;
;;                                                                                               ;;
;; #############################################################################################


;; There are two export methods - the regular one 'export-ofef that specialises on classes and
;; the special one export-ofef-special that specialises on a mode that is passed.

(defgeneric export-ofef (thing &key feature-types &allow-other-keys)
  (:documentation "Exports thing in the Open FCG Exchange Format (ofef)"))

(defgeneric export-ofef-special (thing mode &key (feature-types) &allow-other-keys)
  (:documentation "Exports thing according to mode in the Open FCG Exchange Format (ofef)"))

;;;;;;;;;;;;;;;;;
;; export-ofef ;;
;;;;;;;;;;;;;;;;;


;; Basic types ;;
;;;;;;;;;;;;;;;;;

(defmethod export-ofef ((number number) &key &allow-other-keys)
  "Export number."
  number)

(defmethod export-ofef ((string string) &key &allow-other-keys)
  "Export string."
  (format nil "\"\\\"~(~a~)\\\"\"" string))

(defmethod export-ofef ((symbol symbol) &key &allow-other-keys)
  "Export symbol."
  (cond ((eq symbol t)
         "true")
        ((eq symbol nil)
         "false")
        ((keywordp symbol)
         (format nil "\":~(~a~)\"" symbol))
        (t
         (format nil "\"~(~a~)\"" symbol)))) 

(defmethod export-ofef ((list list) &key &allow-other-keys)
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

(defmethod export-ofef ((function function) &key &allow-other-keys)
  "Ofef-export of function object."
  (format nil "\"#'~(~a~)\"" (third (multiple-value-list (function-lambda-expression function)))))


;; FCG classes ;;
;;;;;;;;;;;;;;;;;

(defmethod export-ofef ((grammar fcg-construction-set) &key &allow-other-keys)
  "Ofef-export of fcg-construction-set."
  (format nil
          "{~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a,~%  ~a: ~a}"
          (export-ofef 'hashed) (if (hashed-cxn-inventory-p grammar) (export-ofef t) (export-ofef nil))
          (export-ofef 'configuration) (export-ofef (configuration grammar))
          (export-ofef 'visualization-configuration) (export-ofef (visualization-configuration grammar))
          (export-ofef 'feature-types) (export-ofef (feature-types grammar))
          (export-ofef 'hierarchy-features) (export-ofef (hierarchy-features grammar))
          (export-ofef 'categorial-network) (export-ofef (categorial-network grammar))
          (export-ofef 'cxns) (export-ofef-special (constructions-list grammar) :constructions-list)))


(defmethod export-ofef ((cxn fcg-construction) &key &allow-other-keys)
  "Export construction."
  (let ((cxn-name (name cxn)))
    (format nil "~a: {~%\"name\": ~a, ~%\"contributing-pole\": ~a, ~%\"conditional-pole\": ~a, ~%\"attributes\": ~a, ~%\"feature-types\": ~a}"
            (export-ofef cxn-name)
            (export-ofef cxn-name)
            (export-ofef-special (contributing-part cxn) :contributing-part :feature-types (feature-types cxn))
            (export-ofef-special (conditional-part cxn) :conditional-part :feature-types (feature-types cxn))
            (export-ofef-special (attributes cxn) :alist)
            (export-ofef (feature-types cxn)))))


(defmethod export-ofef ((unit contributing-unit) &key feature-types &allow-other-keys)
  "Export contributing unit."
  (format nil "~%[~% ~a, ~%~a~%]"
          (export-ofef (name unit))
          (export-ofef-special (unit-structure unit) :unit-structure :feature-types feature-types)))

(defmethod export-ofef ((unit conditional-unit) &key feature-types &allow-other-keys)
  "Export conditional unit."
  (format nil "~%[~% ~a, ~%~a, ~%~a~%]"
          (export-ofef (name unit))
          (export-ofef-special (formulation-lock unit) :unit-structure :feature-types feature-types)
          (export-ofef-special (comprehension-lock unit) :unit-structure :feature-types feature-types)))

(defmethod export-ofef ((configuration configuration) &key &allow-other-keys)
  "Export configuration."
  (export-ofef (configuration configuration)))

(defmethod export-ofef ((hash-table hash-table) &key &allow-other-keys)
  "Export hash-table"
  (loop for key being the hash-keys of hash-table using (hash-value value)
        collect (format nil "~a: ~a" (export-ofef-special key :eliminate-keyword-colon) (export-ofef value))
          into feature-value-pairs
        finally (return (format nil "{~{~a~^,~%~}}" feature-value-pairs))))

(defmethod export-ofef ((network categorial-network) &key &allow-other-keys)
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


(defmethod export-ofef-special (symbol (mode (eql :eliminate-keyword-colon)) &key (feature-types)  &allow-other-keys)
  "Ofef-export of feature-value pair."
  (declare (ignore feature-types))
  (format nil "\"~(~a~)\"" symbol))


(defmethod export-ofef-special (cxns (mode (eql :constructions-list))  &key feature-types &allow-other-keys)
  "Ofef-export of feature-value pair."
  (declare (ignore feature-types))
  (loop for cxn in cxns
        collect (export-ofef cxn) into ofef-cxns
        finally (return (format nil "{~{~a~^,~%~}}" ofef-cxns))))

(defmethod export-ofef-special (alist (mode (eql :alist))  &key feature-types &allow-other-keys)
  "Ofef-export of feature-value pair."
  (declare (ignore feature-types))
  (loop for (key . value) in alist
        for ofef-key = (export-ofef-special key :eliminate-keyword-colon)
        for ofef-value = (export-ofef value)
        collect (format nil "~a: ~a" ofef-key ofef-value) into ofef-alist
        finally (return (format nil "{~{~a~^,~%~}}" ofef-alist))))

(defmethod export-ofef-special (feature-value-pairs (mode (eql :list-of-feature-value-pairs)) &key feature-types &allow-other-keys)
  "Ofef-export of feature-value pair list."
  (declare (ignore feature-types))
  (loop for (feature . value) in feature-value-pairs
        collect (format nil "~a: ~a" (export-ofef feature) (export-ofef value)) into ofef-pairs
        finally (return (format nil "{~{~a~^,~%~}}" ofef-pairs))))


(defmethod export-ofef-special (contributing-part (mode (eql :contributing-part)) &key feature-types &allow-other-keys)
  "Ofef-export of the contributing pole of a cxn."
  (loop for unit in contributing-part
        collect (format nil "~a" (export-ofef unit :feature-types feature-types)) into ofef-units
        finally (return (format nil "[~{~a~^,~}~%]" ofef-units))))

(defmethod export-ofef-special (conditional-part (mode (eql :conditional-part)) &key feature-types &allow-other-keys)
  "Ofef-export of the contributing pole of a cxn."
  (loop for unit in conditional-part
        collect (format nil "~a" (export-ofef unit :feature-types feature-types)) into ofef-units
        finally (return (format nil "[~{~a~^,~}]" ofef-units))))
 
(defmethod export-ofef-special (unit-structure (mode (eql :unit-structure)) &key feature-types &allow-other-keys)
  "Ofef-export of unit-structure."
  (loop for top-level-feature in unit-structure
        for hashed-feature = (if (eql (first top-level-feature) 'HASH)
                               t nil)
        for top-level-feature-name = (if hashed-feature
                                       (second top-level-feature)
                                       (first top-level-feature))
        for top-level-feature-value = (if hashed-feature
                                       (third top-level-feature)
                                       (second top-level-feature))
        for feature-type = (second (find top-level-feature-name feature-types :key #'first))
        collect (format nil "~a: ~a"
                        (if hashed-feature
                          (export-ofef-special top-level-feature-name :hashed-feature-name)
                          (export-ofef top-level-feature-name))
                        (if feature-type
                          (export-ofef-special top-level-feature-value (make-kw feature-type) :feature-types feature-types)
                          (if (atom top-level-feature-value)
                            (export-ofef top-level-feature-value)
                            (export-ofef-special top-level-feature-value :feature-value-pair :feature-types feature-types)))) into ofef-pairs
        finally (return (format nil "{~%~{~a~^,~%~}~%}" ofef-pairs))))

(defmethod export-ofef-special (feature-value-pair (mode (eql :feature-value-pair)) &key feature-types &allow-other-keys)
  "Ofef-export of FCG feature-value pair."
  (let* ((feature-name (first feature-value-pair))
         (feature-value (second feature-value-pair))
         (feature-type (second (find feature-name feature-types :key #'first))))
    (if feature-type
      (format nil "~a: ~a" (export-ofef feature-name) (export-ofef-special feature-value (make-kw feature-type)))
      (format nil "{~a: ~a}" (export-ofef feature-name) (if (listp feature-value)
                                                        (export-ofef-special feature-value :feature-value-pair :feature-types feature-types)
                                                        (export-ofef feature-value))))))

(defmethod export-ofef-special (feature-name (mode (eql :hashed-feature-name))  &key  &allow-other-keys)
  (format nil "\"#~(~a~)\"" feature-name))

(defmethod export-ofef-special (feature-value (mode (eql :set-of-predicates))  &key  &allow-other-keys)
  (format nil "[~%~{~a~^,~}~%]" (loop for predicate in feature-value
                                      collect (export-ofef predicate) into ofef-predicates
                                      finally (return ofef-predicates))))

(defmethod export-ofef-special (feature-value (mode (eql :sequence-of-predicates))  &key  &allow-other-keys)
  (format nil "[~%~{~a~^,~}~%]" (loop for predicate in feature-value
                                      collect (export-ofef predicate) into ofef-predicates
                                      finally (return ofef-predicates))))

(defmethod export-ofef-special (feature-value (mode (eql :set))  &key  &allow-other-keys)
  (format nil "~a" (export-ofef feature-value))) ;;export list

(defmethod export-ofef-special (feature-value (mode (eql :sequence)) &key  &allow-other-keys)
  (format nil "~a" (export-ofef feature-value))) ;;export list




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

(export-ofef (first (constructions-list  *fcg-constructions*)))

(export-ofef-special (contributing-part (first (constructions-list  *fcg-constructions*))) :contributing-part
                     :feature-types (feature-types *fcg-constructions*))

(export-ofef-special (conditional-part (first (constructions-list  *fcg-constructions*))) :conditional-part
                     :feature-types (feature-types *fcg-constructions*))

(pprint (export-ofef *fcg-constructions*))


|#
