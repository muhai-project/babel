(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;;
;; JSON Serialisation of Babel Objects   ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-json))

;; Make-json
;;;;;;;;;;;;;;;;;

(defgeneric make-json (thing &optional objects-processed)
  (:documentation "Return serialised lisp expression for thing, which can be rendered as a json string."))

(defmethod make-json ((thing symbol) &optional objects-processed)
  "Return symbol."
  (values thing objects-processed))

(defmethod make-json ((thing character) &optional objects-processed)
  "Return character as a string."
  (values (string thing) objects-processed))

(defmethod make-json ((thing cons) &optional objects-processed)
  "Return JSON-object"
  (multiple-value-bind (car-json car-objects-processed)
      ;; process car
      (make-json (car thing) objects-processed)
    (multiple-value-bind (cdr-json cdr-objects-processed)
        ;; process cdr
        (make-json (cdr thing) car-objects-processed)
      ;; return (:object (car . cdr))))
      (values (cons car-json cdr-json) cdr-objects-processed))))

(defmethod make-json ((thing string) &optional objects-processed)
  "Return string."
   (values thing objects-processed))

(defmethod make-json ((thing number) &optional objects-processed)
  "Return number."
  (values thing objects-processed))

(defmethod make-json ((thing array) &optional objects-processed)
  "Return array."
  (loop with objects-processed-loop = objects-processed
        for el across thing
        for (json-el new-objects-processed) = (multiple-value-list (make-json el objects-processed-loop))
        do (setf objects-processed-loop new-objects-processed)
        collect json-el into json-elements
        finally (return (values
                         json-elements
                         new-objects-processed))))

(defmethod make-json ((thing function) &optional objects-processed)
  "Return function as a string."
  (values (format nil "~a" thing) objects-processed))

(defmethod make-json ((thing t) &optional objects-processed)
  "Stringify thing."
  (values (format nil "~a" thing) objects-processed))

(defmethod make-json ((thing standard-object) &optional objects-processed)
  "Create a serialised json object for standard object based on class-slots. "
  (if (find thing objects-processed)
    (values "***" objects-processed)
    (loop with slot-objects-processed = objects-processed
          for slot in (closer-mop:class-slots (find-class (type-of thing)))
          collect (multiple-value-bind (jsonified-slot objects-processed-extended)
                      (make-json-clos-slot slot thing slot-objects-processed)
                    (setf slot-objects-processed objects-processed-extended)
                    jsonified-slot)
            into jsonified-slots
          finally (return (values (append (list (cons :class (type-of thing))) jsonified-slots)
                                  slot-objects-processed)))))                                

(defun make-json-clos-slot (slot thing objects-processed)
  (let ((objects-processed-extended (cons thing objects-processed)))
    (values
     #+lispworks (cons (make-json (make-kw (clos:slot-definition-name slot)) objects-processed-extended)
                       (make-json (slot-value thing (clos:slot-definition-name slot)) objects-processed-extended))
     #+sbcl (cons (make-json (make-kw (sb-mop:slot-definition-name slot)) objects-processed-extended)
                  (make-json (slot-value thing (sb-mop:slot-definition-name slot)) objects-processed-extended))
     objects-processed-extended)))