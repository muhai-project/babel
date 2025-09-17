(in-package :slp)

;;-----------------;;
;; predicate utils ;;
;;-----------------;;

(defun compare-elem-to-first-arg (elem predicate)
  "a test function that compares element to the first argument of the predicate"
  (eql
   elem
   (second predicate)))

(defun find-by-fcg-tag (predicates fcg-tag)
  "finds an articulation predicate using its fcg-tag"
  (loop for predicate in predicates
        when (eql (fcg-tag predicate) fcg-tag)
          do (return predicate)))

(defun sort-predicates (predicates)
  "sorts predicates according to type and returns a hash table
   with the types of predicates as keys and the different predicate
   instances as values"
  (loop with sorted-predicates
          = (make-hash-table)
        for predicate in predicates
        for predicate-type
          = (first predicate)
        do
          (push
           predicate
           (gethash
            predicate-type
            sorted-predicates))
        finally
          (return
           sorted-predicates)))

(defun merge-hamnosys (rh-hamnosys lh-hamnosys)
  "merges the sorted hamnosys representations of a right-handed and
left-handed articulation into one two-handed hamnosys representation"
  (let ((rh-handshape (coerce (first rh-hamnosys) 'string))
         (rh-location (if (second rh-hamnosys)
                        (coerce (second rh-hamnosys) 'string)
                        ""))
         (rh-movement (if (third rh-hamnosys)
                        (coerce (third rh-hamnosys) 'string)
                        ""))
         (lh-handshape (coerce (first lh-hamnosys) 'string))
         (lh-location (if (second lh-hamnosys)
                        (coerce (second lh-hamnosys) 'string)
                        ""))
         (lh-movement (if (third lh-hamnosys)
                        (coerce (third lh-hamnosys) 'string)
                        ""))
)
    (format
     nil
     "~a~a~a~a~a~a"
     rh-handshape
     lh-handshape
     rh-location
     lh-location
     rh-movement
     lh-movement)))

(defun sort-by-type (hamnosys)
  "sorts the symbols of a hamnosys string according to their type"
  (loop with handconfig = '()
        with loc = '()
        with movement = '()
        with current-character-type = nil
        with movement-continuation = nil
        for character across hamnosys
        for character-type = (find-hamnosys-character-type character)
        do (if (or (eql character #\) movement-continuation)
             (progn (push character movement)
               (setf movement-continuation t))
             (cond ((or (string= character-type "handshape")
                        (string= character-type "extended-finger-direction")
                        (string= character-type "palm-orientation"))
                    (push character handconfig)
                    (setf current-character-type "handshape"))
                   ((string= character-type "location")
                    (push character loc)
                    (setf current-character-type "location"))
                   ((string= character-type "movement")
                    (push character movement)
                    (setf current-character-type "movement")
                    (setf movement-continuation t))
                   ((string= character-type "modifier")
                    (cond
                     ((string= current-character-type "handshape")
                      (push character handconfig))
                     ((string= current-character-type "location")
                      (push character loc))
                     ((string= current-character-type "movement")
                      (push character movement))))
                   ((string= character-type "various-begin-symbol")
                    (push character movement))
                   ((string= character-type "modifier")
                    (cond
                     ((string= current-character-type "handshape")
                      (push character handconfig))
                     ((string= current-character-type "location")
                      (push character loc))
                     ((string= current-character-type "movement")
                      (push character movement))))
                   ((string= character-type "various-symbol")
                    (cond
                     ((string= current-character-type "handshape")
                      (push character handconfig))
                     ((string= current-character-type "location")
                      (push character loc))
                     ((string= current-character-type "movement")
                      (push character movement))))))
             finally (return
                      `(,(reverse handconfig) ,(reverse loc) ,(reverse movement)))))

;(sort-by-type "")  
    
                   
          
        

(defun sign-table-to-hamnosys-string (sign-table)
  "transforms instance of sign-table into a list of hamnosys strings"
  (loop with output = '()
        with previous-lh-hamnosys = ""
        for x from 0 to (determine-number-of-columns sign-table)
        for right-hand-cell =
          (find-rh-cell-by-column-nr
           (right-hand-cells sign-table)
           x)
        for left-hand-cell =
          (find-lh-cell-by-column-nr
           x
           (left-hand-cells sign-table))
        for rh-hamnosys =
          (hamnosys right-hand-cell)
        for lh-hamnosys =
          (when left-hand-cell
            (hamnosys left-hand-cell))
        for rh-sorted-hamnosys =
          (sort-by-type rh-hamnosys)
        for lh-sorted-hamnosys =
          (when lh-hamnosys
            (sort-by-type lh-hamnosys))
        do (cond
            ((string=
              (find-hamnosys-character-type
               (char
                rh-hamnosys
                0))
              "symmetry")
             (push rh-hamnosys output))
            (left-hand-cell
             (when
                 (string=
                  previous-lh-hamnosys
                  lh-hamnosys)
               (setf (third lh-sorted-hamnosys) nil))
               
             (push
              (merge-hamnosys
               rh-sorted-hamnosys
               lh-sorted-hamnosys)
              output))
            (t
             (push rh-hamnosys output)))
           (setf previous-lh-hamnosys lh-hamnosys)
        finally (return (reverse output))))


;(sign-table-to-hamnosys-string (make-sign-table (predicates *test-utterance-1-predicates*)))
