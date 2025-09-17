(in-package :slp)

;;-------------------;;
;; Create sign-table ;;
;;-------------------;;

;; Define sign-table class. The sign-table contains all information for representing sign predicates as a html table

(defclass sign-table ()
  ((right-hand-predicates
    :documentation "the right-hand predicates observed in the input"
    :initarg :right-hand-predicates
    :accessor right-hand-predicates
    :initform nil
    :type '())
   (two-hand-predicates
    :documentation "the two-hand predicates observed in the input"
    :initarg :two-hand-predicates
    :accessor two-hand-predicates
    :initform nil
    :type '())
   (left-hand-predicates
    :documentation "the left-hand predicates observed in the input"
    :initarg :left-hand-predicates
    :accessor left-hand-predicates
    :initform nil
    :type '())
   (rh-rh-adjacent-predicates
    :documentation "adjacent predicates that establish a relationship between two right-hand fcg-tags"
    :initarg :rh-rh-adjacent-predicates
    :accessor rh-rh-adjacent-predicates
    :initform nil
    :type '())
   (rh-lh-adjacent-predicates
    :documentation "adjacent predicates that establish a relationship between right-hand and left-hand fcg-tags"
    :initarg :rh-lh-adjacent-predicates
    :accessor rh-lh-adjacent-predicates
    :initform nil
    :type '())
   (during-predicates
    :documentation "the during predicates observed in the input"
    :initarg :during-predicates
    :accessor during-predicates
    :initform nil
    :type '())
   (start-coincides-predicates
    :documentation "the start-coincides predicates observed in the input"
    :initarg :start-coincides-predicates
    :accessor start-coincides-predicates
    :initform nil
    :type '())
   (end-coincides-predicates
    :documentation "the end-coincides predicates observed in the input"
    :initarg :end-coincides-predicates
    :accessor end-coincides-predicates
    :initform nil
    :type '())
   (sorted-fcg-tags
    :documentation "a sorted list of the right-hand-fcg-tags in the sign table"
    :initarg :sorted-fcg-tags
    :accessor sorted-fcg-tags
    :initform nil
    :type '())
  (right-hand-cells
    :documentation "the right hand cells of the sign table"
    :initarg :right-hand-cells
    :accessor right-hand-cells
    :initform nil
    :type list)
  (left-hand-cells
    :documentation "the left hand cells of the sign table"
    :initarg :left-hand-cells
    :accessor left-hand-cells
    :initform nil
    :type cons)))

(defclass articulation-predicate ()
  ((articulation-type
    :documentation "the type of the articulation"
    :initarg :articulation-type
    :accessor articulation-type
    :initform nil
    :type symbol)
  (hamnosys
    :documentation "the hamnosys of the articulation"
    :initarg :hamnosys
    :accessor hamnosys
    :initform nil
    :type string)
  (fcg-tag
    :documentation "the fcg-tag of the articulation"
    :initarg :fcg-tag
    :accessor fcg-tag
    :initform nil
    :type symbol)))

(defclass coincides-predicate ()
  ((coincides-type
    :documentation "the type of coinciding relation"
    :initarg :coincides-type
    :accessor coincides-type
    :initform nil
    :type symbol)
  (arg-1
    :documentation "the first argument of the relation"
    :initarg :arg-1
    :accessor arg-1
    :initform nil
    :type symbol)
  (arg-2
    :documentation "the second argument of the relation"
    :initarg :arg-2
    :accessor arg-2
    :initform nil
    :type symbol)))

(defclass right-hand-cell ()
  ((fcg-tag
    :documentation "the fcg-tag of the cell"
    :initarg :fcg-tag
    :accessor fcg-tag
    :initform nil
    :type symbol)
  (hamnosys
    :documentation "the hamnosys of the cell"
    :initarg :hamnosys
    :accessor hamnosys
    :initform nil
    :type string)
  (column-nr
    :documentation "the column-nr of the cell"
    :initarg :column-nr
    :accessor column-nr
    :initform nil
    :type integer)))

(defclass left-hand-cell ()
  ((fcg-tag
    :documentation "the fcg-tag of the cell"
    :initarg :fcg-tag
    :accessor fcg-tag
    :initform nil
    :type symbol)
  (hamnosys
    :documentation "the hamnosys of the cell"
    :initarg :hamnosys
    :accessor hamnosys
    :initform nil
    :type string)
  (start-column-nr
    :documentation "the start-column-nr of the cell"
    :initarg :start-column-nr
    :accessor start-column-nr
    :initform nil
    :type integer)
  (end-column-nr
    :documentation "the end-column-nr of the cell"
    :initarg :end-column-nr
    :accessor end-column-nr
    :initform nil
    :type integer)))

(defun retrieve-column-nr (rh-fcg-tag right-hand-cells)
  "finds the column-nr of the right-hand-cell in right-hand-cells
   that has rh-fcg-tag as fcg-tag"
  (loop for right-hand-cell in right-hand-cells
        when (eql (fcg-tag right-hand-cell)
                  rh-fcg-tag)
          do (return
              (column-nr right-hand-cell))))


(defun find-column-range (left-hand-predicate
                          right-hand-cells
                          coincides-predicates)
  "uses coincides-predicates and right-hand-cells
   to find the column-range of the left-hand-predicate"
  (loop with current-start = nil
        with current-end = nil
        with lh-fcg-tag
          = (fcg-tag
             left-hand-predicate)
        for coincides-predicate in
          coincides-predicates
        for coincides-lh-fcg-tag
          = (arg-2
             coincides-predicate)
        for coincides-rh-fcg-tag
          = (arg-1
             coincides-predicate)
        for column-nr
          = (retrieve-column-nr
             coincides-rh-fcg-tag
             right-hand-cells)
        when (eql coincides-lh-fcg-tag
                  lh-fcg-tag)
          do (unless current-start
               (setf
                current-start
                column-nr))
             (unless current-end
               (setf
                current-end
                column-nr))
             (when (<
                    column-nr
                    current-start)
               (setf
                current-start
                column-nr))
             (when (>
                    column-nr
                    current-end)
               (setf
                current-end
                column-nr))
        finally
          (return
           `(,current-start . ,current-end))))

(defun retrieve-right-hand-cell-info (sign-table)
   "adds all necessary information for the right-hand-cells to the sign-table"
  (loop with column-counter = 0
        with right-hand-predicates
          = (right-hand-predicates
             sign-table)
        with two-hand-predicates
          = (two-hand-predicates
             sign-table)
        for fcg-tag in
          (sorted-fcg-tags sign-table)
        for corresponding-rh-predicate
          = (find-by-fcg-tag
             right-hand-predicates
             fcg-tag)
        for corresponding-two-hand-predicate
          = (find-by-fcg-tag
             two-hand-predicates
             fcg-tag)
        do (cond
            (corresponding-rh-predicate
             (push
              (make-instance
               'right-hand-cell
               :fcg-tag (fcg-tag
                         corresponding-rh-predicate)
               :hamnosys (hamnosys
                          corresponding-rh-predicate)
               :column-nr column-counter)
              (right-hand-cells sign-table)))
            (corresponding-two-hand-predicate
             (progn
               (push
                (make-instance
                 'right-hand-cell
                 :fcg-tag (fcg-tag
                           corresponding-two-hand-predicate)
                 :hamnosys (hamnosys
                            corresponding-two-hand-predicate)
                 :column-nr column-counter)
                (right-hand-cells sign-table))
               (push
                (make-instance
                 'left-hand-cell
                 :fcg-tag (fcg-tag
                           corresponding-two-hand-predicate)
                 :hamnosys (hamnosys
                            corresponding-two-hand-predicate)
                 :start-column-nr column-counter
                 :end-column-nr column-counter)
                (left-hand-cells sign-table))))
            (t
             (push
              (make-instance
               'right-hand-cell
               :fcg-tag fcg-tag
               :hamnosys nil
               :column-nr column-counter)
              (right-hand-cells sign-table))))
        do (incf column-counter)))

  
(defun retrieve-left-hand-cell-info (sign-table)
  "adds all necessary information for the left-hand-cells to the sign-table"
  (loop with right-hand-cells
          = (right-hand-cells
             sign-table)
        with left-hand-predicates
          = (left-hand-predicates
             sign-table)
        with coincides-predicates
          = (append
             (start-coincides-predicates
              sign-table)
             (end-coincides-predicates
              sign-table)
             (during-predicates
              sign-table))
        for left-hand-predicate in
          left-hand-predicates
        for column-range
          = (find-column-range
             left-hand-predicate
             right-hand-cells
             coincides-predicates)
          
        do (push
            (make-instance
             'left-hand-cell
             :fcg-tag (fcg-tag
                       left-hand-predicate)
             :hamnosys (hamnosys
                        left-hand-predicate)
             :start-column-nr (car
                               column-range)
             :end-column-nr (cdr
                             column-range))
           (left-hand-cells sign-table))))

(defun retrieve-arg-2-list (adjacent-predicates)
  "creates and returns a list of all arg-2's in the list of
   adjacent-predicates"
  (loop for adjacent-predicate in
          adjacent-predicates
        collect
          (arg-2
           adjacent-predicate)))

(defun retrieve-lh-arg-2-list (sign-table)
  "creates and returns a list of all arg-2's in the
   adjacent-predicates, during-predicates, and
   end-coincides-predicates of the sign-table"
  (append
   (loop for adjacent-predicate in
           (rh-lh-adjacent-predicates sign-table)
         collect
           (arg-2
            adjacent-predicate))
   (loop for during-predicate in
           (during-predicates sign-table)
         collect
           (arg-1
            during-predicate))
   (loop for end-coincides-predicate in
           (end-coincides-predicates sign-table)
         collect
           (arg-1
            end-coincides-predicate))))


(defun retrieve-right-hand-fcg-tags (right-hand-predicates)
  "creates and returns a list of all right-hand fcg-tags by
   looking them up in the list of right-hand-predicates"
 (loop for predicate in right-hand-predicates
       collect
         (fcg-tag
          predicate)))

(defun set-adjacent-predicates (sign-table adjacent-predicates)
  "splits the list of adjacent-predicates and attaches each list
   to the appropriate slot in sign-table (i.e. rh-rh-adjacent-predicates
   and rh-lh-adjacent-predicates"
  (loop with rh-rh-adjacent-predicates = '()
        with rh-lh-adjacent-predicates = '()
        with right-hand-fcg-tags
          = (retrieve-right-hand-fcg-tags
             (append (right-hand-predicates sign-table)
                     (two-hand-predicates sign-table)))
        for predicate in adjacent-predicates
        do (if (and
                (member
                 (second predicate)
                 right-hand-fcg-tags)
                (member
                 (third predicate)
                 right-hand-fcg-tags))
             (push
              predicate
              rh-rh-adjacent-predicates)
             (push
              predicate
              rh-lh-adjacent-predicates))
        finally (setf (rh-rh-adjacent-predicates sign-table)
                      (make-coincides-predicates rh-rh-adjacent-predicates))
                (setf (rh-lh-adjacent-predicates sign-table)
                      (make-coincides-predicates rh-lh-adjacent-predicates))))

(defun get-first-fcg-tag-candidates (right-hand-predicates arg-2-fcg-tags)
  "get all right-hand fcg-tags that do not immediately follow another
   list contains the first fcg-tag, as well as fcg-tags that occur after
   a gap on the timeline of the right hand"
  (loop for predicate in right-hand-predicates
        when (not
              (member
               (fcg-tag predicate)
               arg-2-fcg-tags))
          collect (fcg-tag predicate)))

(defun complete-chain-and-return-first-fcg-tag (first-fcg-tag-candidates sign-table)
  "adds a dummy fcg-tag and new coincides predicates that link this dummy tag to the existing fcg-tags,
   completing the chain of fcg-tags. Afterwards, it returns the first tag in this chain."
  (loop with first-fcg-tag = nil
        with lh-arg-2-fcg-tags
          = (retrieve-lh-arg-2-list
             sign-table)
        for candidate in
          first-fcg-tag-candidates
        for dummy-var
          = (make-const "dummy")
        do (if (member
                candidate
                lh-arg-2-fcg-tags)
             (progn
               (loop for rh-lh-adjacent-predicate in
                       (rh-lh-adjacent-predicates sign-table)
                     do (cond
                         ((eql
                           (arg-1 rh-lh-adjacent-predicate)
                           candidate)
                          (push
                           (make-instance 'coincides-predicate
                                          :coincides-type 'adjacent
                                          :arg-1 candidate
                                          :arg-2 dummy-var)
                           (rh-rh-adjacent-predicates sign-table)))
                         ((eql
                           (arg-2 rh-lh-adjacent-predicate)
                           candidate)
                          (push
                           (make-instance 'coincides-predicate
                                          :coincides-type 'adjacent
                                          :arg-1 dummy-var
                                          :arg-2 candidate)
                           (rh-rh-adjacent-predicates sign-table)))))
               (loop for during-predicate in (during-predicates sign-table)
                     when (eql
                           (arg-1 during-predicate)
                           candidate)
                       do (loop for rh-lh-adjacent-predicate in
                                  (rh-lh-adjacent-predicates sign-table)
                                when (eql (arg-2 rh-lh-adjacent-predicate)
                                          (arg-2 during-predicate))
                                  do (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'adjacent
                                                     :arg-1 (arg-2 rh-lh-adjacent-predicate)
                                                     :arg-2 dummy-var)
                                      (rh-rh-adjacent-predicates sign-table))
                                     (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'adjacent
                                                     :arg-1 dummy-var
                                                     :arg-2 candidate)
                                      (rh-rh-adjacent-predicates sign-table))
                                     (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'start-coincides
                                                     :arg-1 dummy-var
                                                     :arg-2 (arg-2 rh-lh-adjacent-predicate))
                                      (during-predicates sign-table))))
               (loop for end-coincides-predicate in
                       (end-coincides-predicates sign-table)
                     when (eql
                           (arg-1 end-coincides-predicate)
                           candidate)
                       do (loop for rh-lh-adjacent-predicate in
                                  (rh-lh-adjacent-predicates sign-table)
                                when (eql
                                      (arg-2 rh-lh-adjacent-predicate)
                                      (arg-2 end-coincides-predicate))
                                  do (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'start-coincides
                                                     :arg-1 (arg-1 rh-lh-adjacent-predicate)
                                                     :arg-2 dummy-var)
                                      (rh-rh-adjacent-predicates sign-table))
                                     (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'adjacent
                                                     :arg-1 dummy-var
                                                     :arg-2 candidate)
                                      (rh-rh-adjacent-predicates sign-table))
                                     (push
                                      (make-instance 'coincides-predicate
                                                     :coincides-type 'start-coincides
                                                     :arg-1 dummy-var
                                                     :arg-2 (arg-2 rh-lh-adjacent-predicate))
                                      (end-coincides-predicates sign-table)))))
             (setf first-fcg-tag candidate))
        finally (return first-fcg-tag)))

(defun deterimine-first-fcg-tag-in-chain (sign-table)
  "determines the first fcg-tag in the chain of tags on the
   right hand"
  (let* ((arg-2-fcg-tags
          (retrieve-arg-2-list
           (rh-rh-adjacent-predicates sign-table)))
         (first-fcg-tag-candidates
          (get-first-fcg-tag-candidates
           (append
            (right-hand-predicates sign-table)
            (two-hand-predicates sign-table))
           arg-2-fcg-tags)))
    (if (> (length
            first-fcg-tag-candidates)
           0)
      (complete-chain-and-return-first-fcg-tag
       first-fcg-tag-candidates
       sign-table)
      (first first-fcg-tag-candidates))))

(defun find-adjacent-fcg-tag (rh-rh-adjacent-predicates fcg-tag)
  "finds the tag of the articulation in rh-rh-adjacent-predicates that immediately follows
   fcg-tag"
  (loop for predicate in rh-rh-adjacent-predicates
        when (eql
              (arg-1 predicate)
              fcg-tag)
          return
            (arg-2
             predicate)))

(defun set-sorted-fcg-tags (sign-table)
  "extracts all fcg-tags for the right hand from the sign table
   and sorts them. The sorted list is set as value of sorted-fcg-tags."
   (loop with first-fcg-tag-in-chain
           = (deterimine-first-fcg-tag-in-chain sign-table)
         with sorted-fcg-tags
           = (list first-fcg-tag-in-chain)
         with previous-tag
           = first-fcg-tag-in-chain
         while previous-tag
         for adjacent-tag
           = (find-adjacent-fcg-tag
              (rh-rh-adjacent-predicates sign-table)
              previous-tag)
         do (setf previous-tag nil)
         when adjacent-tag
           do (push adjacent-tag sorted-fcg-tags)
              (setf previous-tag adjacent-tag)
           finally
           (setf (sorted-fcg-tags sign-table)
                 (reverse sorted-fcg-tags))))

(defun make-articulation-predicates (articulation-predicates)
  "transforms list of articulation-predicates into list containing
   instances of articulation-predicate class"
  (loop for predicate in articulation-predicates
        collect
          (make-instance 'articulation-predicate
                         :articulation-type (first predicate)
                         :hamnosys (third predicate)
                         :fcg-tag (second predicate))))

(defun make-coincides-predicates (coincides-predicates)
  "transforms list of articulation-predicates into list containing
   instances of coincides-predicate class"
  (loop for predicate in coincides-predicates
        collect
          (make-instance 'coincides-predicate
                         :coincides-type (first predicate)
                         :arg-1 (second predicate)
                         :arg-2 (third predicate))))
          
(defun make-sign-table (predicates)
  "makes an instance of sign-table and adds all necessary information
   to it"
  (let* ((sorted-predicates
          (sort-predicates predicates))
         (sign-table
          (make-instance 'sign-table
                         :right-hand-predicates
                         (make-articulation-predicates
                          (gethash
                           'right-hand-articulation
                           sorted-predicates))
                         :left-hand-predicates
                         (make-articulation-predicates
                          (gethash
                           'left-hand-articulation
                           sorted-predicates))
                         :two-hand-predicates
                         (make-articulation-predicates
                          (gethash
                           'two-hand-articulation
                           sorted-predicates))
                         :during-predicates
                         (make-coincides-predicates
                          (gethash
                           'during
                           sorted-predicates))
                         :start-coincides-predicates
                         (make-coincides-predicates
                          (gethash
                           'start-coincides
                           sorted-predicates))
                         :end-coincides-predicates
                         (make-coincides-predicates
                          (gethash
                           'end-coincides
                           sorted-predicates)))))
    (set-adjacent-predicates
     sign-table
     (gethash
      'adjacent
      sorted-predicates))
    (set-sorted-fcg-tags sign-table)
    (retrieve-right-hand-cell-info sign-table)
    (retrieve-left-hand-cell-info sign-table)
    sign-table))


(defun find-lh-cell-by-start-column-nr (left-hand-cells start-column-nr)
  "finds the left-hand-cell in sign-table that
   starts with start-column-nr"
  (loop for left-hand-cell in left-hand-cells
        for left-hand-cell-start
          = (start-column-nr left-hand-cell)
        when (eql
              left-hand-cell-start
              start-column-nr)
          do (return left-hand-cell)))

(defun find-rh-cell-by-column-nr (right-hand-cells column-nr)
  "find the right hand expression in sign-table
   that has columnn-nr as column-nr"
  (loop for right-hand-cell in right-hand-cells
        for right-hand-cell-column-nr
          = (column-nr right-hand-cell)
        when (eql
              column-nr
              right-hand-cell-column-nr)
          do (return right-hand-cell)))


(defun determine-number-of-columns (sign-table)
  "determines the number of columns that are needed in the sign-table"
  (-
   (length (right-hand-cells sign-table))
   1 ))