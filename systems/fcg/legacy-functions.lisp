;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :fcg)

;;
;; This file contains functions that are normally NOT used anymore
;; and to which all references should be removed and adapted
;; (december 2016)
;; 
;;

;; This method is still used in a lot of tests, feel free to adapt the tests!!!

(defmethod de-render ((utterance list) (mode (eql :de-render-in-root-mode)) &key &allow-other-keys)
  "De-renders with a root-node and full form constraints."
  (warn "Deprecated Function: two-pole mode (as in FCG before 2015) is not supported anymore")
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
        (push new sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,new) constraints))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole '((root (meaning ()) (sem-cat ())))
		   :right-pole `((root (form ,(cons (cons 'sequence (reverse sequence))
                                                    (append strings constraints)))
                                       (syn-cat ()))))))

(defun make-default-cxn-set ()
  "Build a default construction-set."
  (warn "The function make-default-cxn-set is deprecated and should not be used anymore! Moreover, it is still tailored towards the root-mode from before 2015. Instead, just make an instance of a construction-set and set its configurations")
  (let ((cxn-set (make-instance 'construction-set )))
    ;;; Make sure that the root unit is used instead of the top:
    (set-configuration cxn-set :create-initial-structure-mode :root-mode) 
    (set-configuration cxn-set :de-render-mode :de-render-in-root-mode) 
    (set-configuration cxn-set :render-mode :render-in-root-mode) 
    ;;; Use construction sets:
    (set-configuration cxn-set :cxn-supplier-mode :ordered-by-label) 
    cxn-set))




;; de-render.lisp

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; De-render-with scope
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod de-render ((utterance string) (mode (eql :de-render-with-scope)) &key cxn-inventory &allow-other-keys)
  "splits utterance by space and calls de-render-with-scope with this list."
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t) :de-render-with-scope
             :cxn-inventory cxn-inventory))

(defmethod de-render ((utterance list) (mode (eql :de-render-with-scope)) &key cxn-inventory &allow-other-keys)
  "De-renders with scope"
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((sequence nil) ;; Will become an ordered list of unit-names associated with the strings of the sentence
        (word-boundaries nil) ;; E.g. ((unit-1 0 1) (unit-2 1 2))
        (counter 0)
        (form-predicates (get-configuration cxn-inventory :form-predicates)) ;; Fetches the predicates that will be used (by default: MEETS and PRECEDES and FIRST)
        (form-constraints nil))
    
    ;; 1. Collect a STRING-form-constraint and a boundary for each string, and add them to the right feature-value.
    (dolist (string utterance)
      (let ((unit-name (make-const string nil)))
        (push unit-name sequence)
        (push ;; Here we make the boundary, e.g. (unit-1 0 1)
              `(,unit-name ,counter ,(incf counter)) word-boundaries)
        (push ;; Here we make the string-form-constraint
              `(string ,unit-name ,string) form-constraints)))
    
    ;; 2. Now we add all the relevant word ordering constraints to the form-constraints.
    (setf word-boundaries (reverse word-boundaries)
          form-constraints
          (append (infer-all-constraints-from-boundaries word-boundaries form-predicates nil) form-constraints))
    
    ;; 3. Finally build a transient structure, set its data, and return it.
    (let ((transient-structure
           (make-instance 'coupled-feature-structure
                          :left-pole `((root (meaning ())
                                             (sem-cat ())
                                             (boundaries ,word-boundaries)
                                             (form ,(cons (cons 'sequence (reverse sequence))
                                                          form-constraints))
                                             
                                             (syn-cat ())))
                          :right-pole '((root)))))
      (set-data transient-structure :sequence (reverse sequence))
      transient-structure)))

(export '(get-updating-references
          handle-form-predicate-in-de-render infer-before-constraints
          infer-all-constraints-from-boundaries))

;; By default, it uses the form predicates MEETS, PRECEDES, FIELDS and FIRST
;; -------------------------------------------------------------------------
(defun get-updating-references (&optional node-or-cxn-inventory
                                          (default-form-predicates '(meets precedes fields first)))
  "returns form-predicates from node or cxn-inventory"
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((type (type-of node-or-cxn-inventory)))
    (or (case type
          (null default-form-predicates)
          (cip-node (get-configuration (construction-inventory node-or-cxn-inventory) :form-predicates))
          (coupled-feature-structure (ignore-errors ;; Data slot may not exist
                                       (get-data node-or-cxn-inventory :form-predicates)))
          (t (get-configuration node-or-cxn-inventory :form-predicates)))
        default-form-predicates)))

;; -------------------------------------------------------------------
;; METHOD: handle-form-predicate-in-de-render
;;         > Specializes on form predicates (e.g. MEETS)
;;         > Infers constraints based on boundaries
;; -------------------------------------------------------------------

;; Helper functions
(defun infer-before-constraints (list-of-boundaries predicate test-fn)
  "Infer MEETS or PRECEDES constraints from boundaries."
  ;; First ensure that the boundaries are sorted.
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((sorted-boundaries (sort (copy-list list-of-boundaries) #'< :key #'second)))
    ;; A local function that recursively gets the constraints:
    (labels ((get-all-constraints (lst result)
               (if (null lst)
                 (reverse result)
                 (get-all-constraints (rest lst)
                                      (let* ((first-unit (first lst))
                                             (unit-name (first first-unit))
                                             (outer-bd (third first-unit)))
                                        (dolist (unit-and-boundaries (rest lst) result)
                                          (when (funcall test-fn outer-bd (second unit-and-boundaries))
                                            (push `(,predicate ,unit-name ,(first unit-and-boundaries) ,(make-var 'unit))
                                                  result))))))))
      (get-all-constraints sorted-boundaries nil))))

;; Generic function and its methods
(defgeneric handle-form-predicate-in-de-render (list-of-boundaries predicate &optional unit-structure))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate t) &optional unit-structure)
  ;; To avoid potential symbol-name problems.
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (assert (symbolp predicate))
  (cond
   ((string= predicate 'meets)
    (handle-form-predicate-in-de-render list-of-boundaries 'meets))
   ((string= predicate 'precedes)
    (handle-form-predicate-in-de-render list-of-boundaries 'precedes))
   ((string= predicate 'first)
    (handle-form-predicate-in-de-render list-of-boundaries 'first))
   ((string= predicate 'last)
    (handle-form-predicate-in-de-render list-of-boundaries 'last))
   ((string= predicate 'before)
    (handle-form-predicate-in-de-render list-of-boundaries 'before unit-structure))
   (t
    (progn
      (warn "No applicable handle-form-predicate-in-de-render method for the arguments ~a and ~a" list-of-boundaries predicate)
      nil))))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'meets)) &optional unit-structure)
  (declare (ignore unit-structure))
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (infer-before-constraints list-of-boundaries predicate #'=))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'precedes)) &optional unit-structure)
  (declare (ignore unit-structure))
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (infer-before-constraints list-of-boundaries predicate #'<=))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'fields)) &optional unit-structure)
  ;; The FIELDS feature does not provide enough information to make useful
  ;; inferences in comprehension. So we return nothing instead.
  (declare (ignore list-of-boundaries predicate unit-structure))
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  nil)

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'before)) &optional unit-structure)
  "BEFORE constraints only look at units that have constituents/subunits."
  ;; Return nothing instead because BEFORE is a structural word order constraint.
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let* ((phrasal-units (loop for unit in unit-structure
                              when (or (assoc 'constituents (unit-body unit) :test #'string=)
                                       (assoc 'subunits (unit-body unit) :test #'string=))
                              collect (unit-name unit)))
         (filtered-boundaries (loop for boundary in list-of-boundaries
                                    when (member (first boundary) phrasal-units)
                                    collect boundary)))
    (infer-before-constraints filtered-boundaries predicate #'<=)))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'adjacent)) &optional unit-structure)
  ;; Provides two predicates for every pair of adjacent units.
  ;; E.g. "the mouse" -> ((adjacent the-unit mouse-unit scope) (adjacent mouse-unit the-unit scope))
  (declare (ignore unit-structure))
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((adjacent-before-constraints (infer-before-constraints list-of-boundaries predicate #'=)))
    (loop for form-constraint in adjacent-before-constraints
          append `(,form-constraint
                   ,(list predicate (third form-constraint) (second form-constraint) (make-var 'unit))))))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'first)) &optional unit-structure)
  "Infer FIRST constraints based on SUBUNITS or CONSTITUENTS."
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((constraints nil))
    (dolist (unit unit-structure)
      (let ((subunits (or (unit-feature-value unit 'subunits)
                          (unit-feature-value unit 'constituents))))
        (when subunits
          (let ((first-unit (caar (sort (remove-if #'(lambda(x)
                                                       (not (member (first x) subunits :test #'string=)))
                                                   list-of-boundaries)
                                        #'< :key #'second))))
            (when first-unit
              (push `(,predicate ,first-unit ,(unit-name unit)) constraints))))))
    constraints))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'last)) &optional unit-structure)
  "Get the rightmost subunit."
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let ((constraints nil))
    (dolist (unit unit-structure)
      (let ((subunits (or (unit-feature-value unit 'subunits)
                          (unit-feature-value unit 'constituents))))
        (when subunits
          (let ((last-unit (caar (sort (remove-if #'(lambda(x)
                                                       (not (member (first x) subunits :test #'string=)))
                                                   list-of-boundaries)
                                        #'> :key #'third))))
            (when last-unit
              (push `(,predicate ,last-unit ,(unit-name unit)) constraints))))))
    constraints))

(defun infer-all-constraints-from-boundaries (boundaries form-predicates &optional unit-structure)
  "Given a list of boundaries, infer its form-predicates."
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (let (result)
    (dolist (predicate form-predicates)
      (setf result (append result (handle-form-predicate-in-de-render boundaries predicate unit-structure))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated Methods for backwards compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance t) (mode (eql :de-render-in-one-pole-mode)) &key &allow-other-keys)
  "Deprecated method for backwards compatibility, use de-render-string-meets-precedes now"
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (warn "Please use the :de-render-string-meets-precedes method now. :de-render-in-one-pole-mode is no longer supported.")
  (de-render utterance :de-render-string-meets-precedes))

(defmethod de-render ((utterance t) (mode (eql :de-render-in-one-pole-mode-meets-only))  &key &allow-other-keys)
  "splits utterance by space and calls de render with list instead of string"
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (de-render utterance :de-render-string-meets))

;; ################# ;;
;; Helper functions  ;;
;; ################# ;;

(export '(form-constraints-with-meets))

(defun form-constraints-with-meets (utterance &key (variables nil))
  "Takes a simple list of strings and returns its corresponding string/meets-representation"
  ; the second argument determines whether the string-references will
  ; be constants or variables. variables will in any case be
  ; uninstantiated variables! (i.e. without a "-#id" suffix!) because
  ; the result of this function will likely be passed to (make-cxn)
  ; where you want shared variables that are only then instantiated
  (loop
   for string in utterance
   for variable = (if variables
                    (gensym (concatenate 'string "?" (string-upcase (string-replace string " " "-")) "-"))
                    (make-const (string-replace string " " "-") nil))
   when (length> vars 0)

   collect `(meets ,(car (last vars)) ,variable)
   if (typep string 'string)
   collect `(string ,variable ,string)
   and
   collect variable into vars ;; no meets for gestures
   else
   collect `(gesture ,variable ,string)))

;; ############################################################################
;; Initial structures for root-mode
;; ############################################################################

(defmethod create-initial-structure ((meaning list) (mode t)  &key &allow-other-keys)
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)))
		 :right-pole '((root
				(form nil)
				(syn-cat nil)))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :root-mode))
                                      &key &allow-other-keys)
  (warn "Deprecated functions that will be removed in a next release - adapt or shout loud.")
  (create-initial-structure meaning t))
