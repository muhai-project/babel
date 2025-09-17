(in-package :slp)

;;------------------------------------------------------------------;;
;; code for transforming geo-prolog strings into predicate notation ;;
;;------------------------------------------------------------------;;

(defun get-all-vars (geo-prolog-string)
  (loop for raw-el in (remove "" (cl-ppcre::split "[,\(\)]" geo-prolog-string) :test #'equal)
        for el = (string-trim " " raw-el)
        when (upper-case-p (first (coerce el 'list)))
        collect (variablify el)))      

(defun geo-prolog-to-predicates (geo-prolog-string)
  "convert geo-prolog to predicate notation, see https://www.cs.utexas.edu/~ml/wasp/geo-funql.html for specification"
  (loop with stack = (list (geo-prolog-to-polish-notation geo-prolog-string))
        with var-list = (get-all-vars geo-prolog-string)
        with program-level = (first (push (get-next-var-from-var-list var-list) var-list))
        
        while stack     
        for part = (pop stack)
        append (loop with next-level = program-level
                     for predicate in part
                     for term = (when (equal (type-of predicate) 'cons)
                                  (first predicate))
                     for args = (when (equal (type-of predicate) 'cons)
                                  (rest predicate))
                     for vars = (append (list program-level)
                                        (loop for el in args
                                              if (or (numberp el)
                                                     (variable-p el))
                                              collect el
                                              else
                                              collect (setf next-level (get-next-var next-level)))) ;; increment level here
                     for cand-emb-preds = (loop for el in args
                                                unless (or (numberp el)
                                                           (variable-p el))
                                                collect el)
                     for accum-val = (list (append (list term) vars))
                     do (cond (;; handle double embedding in list
                               (and (equal term '_not)
                                    (equal (type-of (car cand-emb-preds)) 'cons)
                                    (equal (type-of (caar cand-emb-preds)) 'cons)
                                    (equal (type-of (caaar cand-emb-preds)) 'symbol)
                                    (member (caaar cand-emb-preds) '(const)))
                               ;; todo: handle recursively by pushing to front of stack instead - requires solution for level vars which are now correct because of BF queuing
                               
                               
                               ;; create vars for all embedded predicates
                               (setf accum-val (append accum-val (list (append (list (caaar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdaar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                               ;; todo: make a function out of the above to avoid duplication, loop until atom is reached
                               (setf cand-emb-preds (cddaar cand-emb-preds))
                                                         
                               (setf accum-val (append accum-val (list (append (list (caar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                                                                       
                                                         
                               ;; link all embedded predicates with their new level vars                
                               (setf accum-val (append accum-val (loop for var in (rest (rest (last-elt accum-val)))
                                                       for symb in (remove-if #'variable-p (cdar cand-emb-preds))
                                                       collect (list symb var)))))

                         (;; handle cityid, stateid, countryid, placeid, riverid, river
                               (and (equal (type-of (car cand-emb-preds)) 'cons)
                                    (equal (type-of (caar cand-emb-preds)) 'symbol)
                                    (member (caar cand-emb-preds) '(cityid stateid countryid placeid riverid river const)))
                               ;; create vars for all embedded predicates
                               (setf accum-val (append accum-val (list (append (list (caar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                                                                       
                                                         
                               ;; link all embedded predicates with their new level vars                
                               (setf accum-val (append accum-val (loop for var in (rest (rest (last-elt accum-val)))
                                                       for symb in (remove-if #'variable-p (cdar cand-emb-preds))
                                                       collect (list symb var)))))                               
                              (;; predicate is a constant e.g. 'new_york
                               (equal (type-of predicate) 'symbol)
                               (setf accum-val (list (list predicate program-level)))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list))))
                              
                              ;; term is actually a list, continue with the list in next iteration   
                              ((equal (type-of term) 'cons)
                               (setf accum-val nil)
                  
                               (setf stack (append stack (append (list term) cand-emb-preds))))
                              ;; rest is not preceded by round brackets, add them
                              ((and cand-emb-preds
                                    (or
                                     (equal (type-of (car cand-emb-preds)) 'symbol) ;(new_york)
                                     (equal (type-of (caar cand-emb-preds)) 'symbol))) ;((cityid or ((fewest
                               (setf stack (append stack (list cand-emb-preds))))
                              
                              ;; term is an actual term, collect the result and continue with the embedded predicates
                              (t
                               (setf stack (append stack cand-emb-preds))))

                     when accum-val
                     append accum-val
                     finally (when (and accum-val
                                        (not (equal (type-of predicate) 'symbol)))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list)))))))

(defun get-next-var-from-var-list (var-list)
  (get-next-var (first (sort var-list #'> :key #'(lambda (x) (position (symbol-name x) gl::+placeholder-vars+ :test #'equal))))))

(defun get-next-var (last-var)
  (intern (nth (+ 1 (position (symbol-name last-var) gl::+placeholder-vars+ :test #'equal)) gl::+placeholder-vars+)))

(defun geo-prolog-to-polish-notation (geo-prolog-string)
  (let* ((geo-prolog-string (cl-ppcre:regex-replace-all ", " geo-prolog-string ","))
         (geo-prolog-string (cl-ppcre:regex-replace-all " " geo-prolog-string "_"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([a-z]+_?[a-z]+)[\(]" geo-prolog-string "\(\\1 "))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([A-Z])" geo-prolog-string "?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all ",(_)" geo-prolog-string ",?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "'(.*?)'" geo-prolog-string "\\1"))
         (geo-prolog-string (utils::string-replace geo-prolog-string "," " "))
         (geo-prolog-string (utils::string-replace geo-prolog-string "(not" "(_not"))
         (geo-prolog-string (string-append "("geo-prolog-string ")")))
    (read-from-string geo-prolog-string)))

(defun strip-superfluous-brackets (predicates)
  (cond ((and
          (= (length predicates) 1)
          (equal (type-of (first predicates)) 'list)
          (= (length (first predicates)) 1))
         (first (first predicates)))
        ((= (length predicates) 1)
         (first predicates))
        (t
         predicates)))

(defun predicates-to-geo-prolog (predicates)
  "go through all predicates in reverse, and keep substituting subprograms until no levels are left"
  (loop with stack = (reverse (copy-list predicates))      
        while stack     
        for predicate = (pop stack)
        for term = (first predicate)
        for nxt-lvl-vars = (cond ((member term '(count sum))
                                  (list (fourth predicate)))
                                 ((equal term 'fewest)
                                  (list (fifth predicate)))
                                 ((and (equal term 'cityid)
                                       (equal (fourth predicate) '?_))
                                  (list (third predicate)))
                                 ((equal term 'cityid)
                                  (list (third predicate) (fourth predicate)))
                                 (t
                                  (last predicate)))
        ;; find all embeddable predicates for a certain level
        for embedded-preds = (loop for nxt-lvl-var in nxt-lvl-vars
                                   for em-pred = (find-all nxt-lvl-var (remove predicate predicates) :key #'second :test #'equal)
                                   when em-pred
                                   append em-pred)
        ;; remove the lvl variable from the predicates
        for stripped-embedded-preds-list = (if (= (length nxt-lvl-vars) 1)
                                             ;; there is only one var, but maybe multiple predicates
                                             (mapcar #'(lambda (predicate) (strip-superfluous-brackets (remove (first nxt-lvl-vars) predicate))) embedded-preds)
                                             ;; there is a corresponding var for each predicate (cityid ?x ?y) (new_york ?x) (ny ?y)
                                             (loop for rem-preds in (copy-object embedded-preds)
                                                   for nxt-lvl-var in nxt-lvl-vars
                                                   do (delete nxt-lvl-var rem-preds)
                                                   collect (strip-superfluous-brackets (list rem-preds))))
        ;; throw out the embedded predicates from remaining predicates
        for remaining-preds = (loop for pred in predicates
                                    unless (member pred embedded-preds)
                                    collect pred)
        when embedded-preds
        ;; substitute the level var with the embedded predicates
        do (if (= (length nxt-lvl-vars) 1)
             ;; there is only one var
             (nsubstitute (if (= 1 (length stripped-embedded-preds-list))
                            ;; there is only one predicate, remove its surrounding brackets
                            (first stripped-embedded-preds-list)
                            ;; there are multiple embedded predicates, pass the full list
                            stripped-embedded-preds-list)
                          (first nxt-lvl-vars) predicate)
             ;; there are multiple vars and corresponding predicates, pair them
             (loop for nxt-lvl-var in nxt-lvl-vars
                   for stripped-embedded-preds in stripped-embedded-preds-list
                   do (nsubstitute stripped-embedded-preds nxt-lvl-var predicate)))
        (setf predicates remaining-preds)
        ;; remove the level var for answer
        finally (return (remove (second (first predicates)) (first predicates)))))

;(geo-prolog-to-predicates "answer(A,(river(A),loc(A,B),const(B,stateid(arkansas))))")
;(predicates-to-geo-prolog '((ANSWER ?C ?A ?D) (RIVER ?D ?A) (LOC ?D ?A ?B) (CONST ?D ?B ?E) (STATEID ?E ?F) (ARKANSAS ?F)))
