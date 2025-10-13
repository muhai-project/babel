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

(export '(footprints meaning sem-cat form syn-cat boundaries before))

;; Default ;;
;;;;;;;;;;;;;

(defmethod de-render ((utterance t) (mode t) &key &allow-other-keys)
  "Default de-render mode: call :de-render-sequence-predicates."
  (de-render utterance :de-render-sequence-predicates))


;; De-render into sequence predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string) (mode (eql :de-render-sequence-predicates)) &key &allow-other-keys)
  "De-renders a string into a set of predicates containing a single sequence predicate."
  (make-instance 'coupled-feature-structure 
                 :left-pole `((root (form ((sequence ,utterance 0 ,(length utterance))))))
                 :right-pole '((root))))


;; De-render into string, meets, precedes and first predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-precedes-first)) &key &allow-other-keys)
  "Splits utterance by space and calls de-render-string-meets-precedes-first with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets-precedes-first))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes-first)) &key &allow-other-keys)
  "De-renders a list of strings into string, meets, precedes and first constraints."
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((unit-name (make-const string nil)))
        (push unit-name sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,unit-name) constraints))
	(push `(string ,unit-name ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (push `(first ,(last-elt sequence)) constraints)
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(cons (cons 'sequence (reverse sequence))
                                                   (append strings constraints)))
                                      (syn-cat ())))
		   :right-pole '((root)))))


;; De-render into string, meets, and precedes predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-precedes)) &key &allow-other-keys)
  "Splits utterance by space and calls de-render-string-meets-precedes with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets-precedes))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes)) &key &allow-other-keys)
  "De-renders a list of strings into string, meets and precedes constraints."
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((unit-name (make-const string nil)))
        (push unit-name sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,unit-name) constraints))
	(push `(string ,unit-name ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(cons (cons 'sequence (reverse sequence))
                                                   (append strings constraints)))
                                      (syn-cat ())))
		   :right-pole '((root)))))


;; De-render into string and meets predicates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets)) &key &allow-other-keys)
  "Splits utterance by space and calls de-render-string-meets with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets)) &key &allow-other-keys)
  "De-renders a list of strings into string and meets."
  (let ((strings nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(append (reverse strings) constraints))
                                      (syn-cat ())))
		   :right-pole '((root)))))


;; De-render raw string ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance list) (mode (eql :de-render-raw)) &key &allow-other-keys)
  "Puts the input strings as such under the form feature in the root."
  (make-instance 'coupled-feature-structure 
                 :left-pole `((root (form ,utterance)))
                 :right-pole '((root))))