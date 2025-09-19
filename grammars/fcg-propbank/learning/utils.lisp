(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacy Benepar compatible annotation     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod spacy-benepar-compatible-annotation ((sentence fcg-propbank-sentence) role-set &key (selected-role-types 'all))
  "Returns T if all selected role types (all, core-only, argm-only) or a selected role type correspond to a constituent in the spacy-benepar tree."
  (let ((syntactic-analysis (remove '(nil) (left-pole-structure (initial-transient-structure sentence)) :test #'equalp))
        (selected-roles (loop for frame in (propbank-frames sentence)
                              if (equalp (frame-name frame) role-set)
                              append (loop for frame-role in (frame-roles frame)
                                           if (case selected-role-types
                                                (all t)
                                                (core-only (unless (search "ARGM" (role-type frame-role))
                                                             t))
                                                (argm-only (when (search "ARGM" (role-type frame-role))
                                                             t)))
                                           collect frame-role))))
    
    (loop for role in selected-roles
          for first-index-for-role = (first (indices role))
          for last-index-for-role = (last-elt (indices role))
          always (loop for unit in syntactic-analysis
                       for unit-start = (first (unit-feature-value unit 'span))
                       for unit-end = (second (unit-feature-value unit 'span))
                       thereis (and (= first-index-for-role unit-start)
                                    (= last-index-for-role (- unit-end 1))
                                    (equalp (role-string role) (unit-feature-value unit 'string)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions to create new constructions  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric make-cxn-schema (core-units-with-role cxn-units-with-role mode &key &allow-other-keys)
  (:documentation "Makes an abstract cxn schema"))


(defmethod make-cxn-schema (core-units-with-role cxn-units-with-role 
                                                 (mode (eql :core-roles))
                                                 &key 
                                                 &allow-other-keys)
  (declare (ignore mode))
  
  (loop for (role . nil) in core-units-with-role
        for cxn-unit in cxn-units-with-role
        collect (cons (intern (role-type role))
                      (if (find 'v (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))
                        (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))
                        (if (find 'dependency-label (cddr cxn-unit) :key #'feature-name)
                          (list (feature-value (find 'dependency-label (cddr cxn-unit) :key #'feature-name)))
                          (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))))))


(defmethod make-cxn-schema (units-with-role cxn-units-with-role 
                                                 (mode (eql :argm-leaf))
                                                 &key lemma
                                                 &allow-other-keys)

  (declare (ignore mode))
  (assert lemma)
  
  (loop for (role . nil) in units-with-role
      for cxn-unit in cxn-units-with-role
                       collect (cons (intern (role-type role))
                                     (if (equal (feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)) lemma)
                                       lemma
                                       (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))))


(defmethod make-cxn-schema (units-with-role cxn-units-with-role 
                                                 (mode (eql :argm-phrase))
                                                 &key phrase
                                                 &allow-other-keys)

  (declare (ignore mode))
  (assert phrase)
  
  (loop for (role . nil) in units-with-role
      for cxn-unit in cxn-units-with-role
                       collect (cons (intern (role-type role))
                                     (if (string= (role-type role) "V")
                                       (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))
                                       phrase
                                       ))))




(defmethod make-cxn-schema (core-units-with-role cxn-units-with-role
                                                 (mode (eql :argm-pp))
                                                 &key cxn-preposition-units)
  (loop with pp-unit-number = 0
        for (role . unit) in core-units-with-role
        for cxn-unit in cxn-units-with-role
        collect (cons (intern (role-type role))
                      (cond
                       ;; unit is a pp
                       ((find 'pp (unit-feature-value (unit-body unit) 'syn-class))
                        (incf pp-unit-number)
                        (if (= 1 (length (nth1 pp-unit-number cxn-preposition-units)))
                          (intern (format nil "~{~a~^-~}(~a)" (unit-feature-value unit 'syn-class)
                                          (second (find 'lemma
                                                        (nthcdr 2 (first (nth1 pp-unit-number cxn-preposition-units)))
                                                        :key #'feature-name))))
                          (intern (format nil "~{~a~^-~}(cc-~a)" (unit-feature-value unit 'syn-class)
                                          (second (find 'lemma
                                                        (nthcdr 2 (third (nth1 pp-unit-number cxn-preposition-units)))))))))
                       
                       ;; Unit contains a lemma
                       ((feature-value (find 'lemma (unit-body cxn-unit) :key #'feature-name)))
                       ;; Unit contains a phrase-type
                       ((feature-value (find 'syn-class (unit-body cxn-unit) :key #'feature-name)))))))



(defmethod make-cxn-schema (core-units-with-role cxn-units-with-role
                                                 (mode (eql :argm-sbar))
                                                 &key cxn-s-bar-units)
  (loop with s-bar-unit-number = 0
        for (role . unit) in core-units-with-role
        for cxn-unit in cxn-units-with-role
        collect (cons (intern (role-type role))
                      (cond
                       ;; unit is an s-bar
                       ((or (find 'sbar (unit-feature-value (unit-body unit) 'syn-class))
                            (find 's (unit-feature-value (unit-body unit) 'syn-class)))
                        (incf s-bar-unit-number)
                        (intern (format nil "~{~a~^-~}(~a)" (unit-feature-value unit 'syn-class)
                                        (or (second (find 'lemma
                                                          (nthcdr 2 (nth1 s-bar-unit-number cxn-s-bar-units))
                                                          :key #'feature-name))
                                            (second (find 'string
                                                          (nthcdr 2 (nth1 s-bar-unit-number cxn-s-bar-units))
                                                          :key #'feature-name))))))
                       ;; Unit contains a lemma
                       ((feature-value (find 'lemma (unit-body cxn-unit) :key #'feature-name)))
                       ;; Unit contains a phrase-type
                       ((feature-value (find 'syn-class (unit-body cxn-unit) :key #'feature-name)))))))



(defun make-gram-category (units-with-role &optional lemma)
  "Creates a unique grammatical category based on units-with-role."
  (if lemma
    (intern (symbol-name (make-const
                          (format nil "~{~a~^+~}"
                                  (loop for (r . u) in units-with-role
                                        if (search "ARGM" (role-type r))
                                          collect (format nil "~a(~{~a~^-~}:~a)"
                                                          (role-type r)
                                                          (feature-value (find 'syn-class (unit-body u)
                                                                               :key #'feature-name))
                                                          lemma)
                                        else collect
                                            (format nil "~a~a"
                                                    (role-type r)
                                                    (if (and (not (string= "V" (role-type r)))
                                                             (find 'dependency-label (unit-body u) :key #'feature-name))
                                                      (list (feature-value (find 'dependency-label (unit-body u) :key #'feature-name)))
                                                      (feature-value (find 'syn-class (unit-body u) :key #'feature-name)))))))))
    (intern (symbol-name (make-const
                          (format nil "~{~a~^+~}"
                                  (loop for (r . u) in units-with-role
                                        collect (format nil "~a~a"
                                                        (role-type r)
                                                        (if (and (not (string= "V" (role-type r)))
                                                                 (find 'dependency-label (unit-body u) :key #'feature-name))
                                                          (list (feature-value (find 'dependency-label (unit-body u) :key #'feature-name)))
                                                          (feature-value (find 'syn-class (unit-body u) :key #'feature-name)))))))))))


(defun make-subclause-word-unit (unit-with-role unit-structure)
  (let* ((sbar-unit (cdr unit-with-role))
         (subclause-word-in-ts
          (loop for unit in unit-structure
                when (and (find (list 'node-type 'leaf) (unit-body unit) :test #'equalp)
                         ; (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name sbar-unit))
                          (intersection '(in aux) (unit-feature-value unit 'syn-class)))
                return unit)))

    (if subclause-word-in-ts
       `(,(variablify (unit-name subclause-word-in-ts))
         --
         (parent ,(variablify (unit-name sbar-unit)))
         (syn-class ,(cadr (find 'syn-class (unit-body subclause-word-in-ts) :key #'feature-name)))
         (lemma ,(cadr (find 'lemma (unit-body subclause-word-in-ts) :key #'feature-name))))
      (let ((phrasal-sbar-subunit-in-ts
             (loop for unit in unit-structure
                   when (and (find (list 'node-type 'phrase) (unit-body unit) :test #'equalp)
                             (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name sbar-unit)))
                   return unit)))
        (when phrasal-sbar-subunit-in-ts
           `(,(variablify (unit-name phrasal-sbar-subunit-in-ts))
             --
             (parent ,(variablify (unit-name sbar-unit)))
             (syn-class ,(cadr (find 'syn-class (unit-body phrasal-sbar-subunit-in-ts) :key #'feature-name)))
             (string ,(cadr (find 'string (unit-body phrasal-sbar-subunit-in-ts) :key #'feature-name)))))))))

(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun truncate-frame-name (frame-name)
  "Remove reference to word sense from frame name"
  (cond ((stringp frame-name)
         (first (split-string frame-name ".")))
        ((symbolp frame-name)
         (intern (first (split-string (symbol-name frame-name) "."))))))

;(truncate-frame-name 'believe.01)

(defun make-propbank-contributing-unit (units-with-role gold-frame gram-category footprint &key (include-gram-category? t))
  "Make a contributing unit based on a gold-frame and units-with-role."
  (let* ((v-unit (cdr (assoc "V" units-with-role :key #'role-type :test #'equalp)))
         (v-unit-name (variablify (unit-name v-unit)))
         (meaning (loop for r in (frame-roles gold-frame)
                        if (string= (role-type r) "V")
                        collect `(frame ?roleset ,v-unit-name)
                        else
                        if (and (find (role-type r) units-with-role :key #'(lambda (unit-with-role)
                                                                     (role-type (car unit-with-role))) :test #'equalp)
                                (unit-name (cdr (assoc r units-with-role))))
                        collect `(frame-element ,(intern (upcase (role-type r))) ,v-unit-name
                                                ,(variablify (unit-name (cdr (assoc r units-with-role))))))))
    `(,v-unit-name
      (frame-evoking +)
      (footprints (,footprint))
      ,@(when include-gram-category? `((gram-category ,gram-category)))
      (frame ?roleset)
      (meaning ,meaning))))


(defun make-propbank-conditional-unit-with-role (unit-with-role category footprint &key (lemma nil) (string nil) (frame-evoking nil))
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                   (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name)))))
         (syn-class (find 'syn-class (unit-body unit) :key #'feature-name))
         (dependency-label ;(when (find 'rb (feature-value syn-class))
          (find 'dependency-label (unit-body unit) :key #'feature-name)))
    
    ;;a FEE unit also has the features lemma and footprints
    (if (equalp "V" (role-type (car unit-with-role)))
      `(,unit-name
        --
        (parent ,parent)
        ,syn-class
        (footprints (NOT ,footprint))
        ,@(when frame-evoking
            '((frame-evoking +)))
        ,@(when category
            `((lex-category ,category))))
      `(,unit-name
        --
        (parent ,parent)
        ,@(if dependency-label
            `(,dependency-label)
            `(,syn-class))
        ,@(when lemma
            `((lemma ,lemma)))
        ,@(when string
            `((string ,string)))))))


(defun make-preposition-unit (unit-with-role unit-structure)
  (let* ((pp-unit (cdr unit-with-role))
         (preposition-unit-in-ts (loop for unit in unit-structure
                                       when (and (or (find 'in (feature-value (find 'syn-class (unit-body unit) :key #'feature-name)):test #'equal)
                                                     (equal 'prep (feature-value (find 'dependency-label (unit-body unit) :key #'feature-name))))
                                                 (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                       return unit)))

    (cond (preposition-unit-in-ts
           (let ((lemma (cadr (find 'lemma (unit-body preposition-unit-in-ts) :key #'feature-name))))
             (assert lemma)
             (list
              `(,(variablify (unit-name preposition-unit-in-ts))
                --
                (parent ,(variablify (unit-name pp-unit)))
                (lemma ,lemma)))))
          (t ;; no prep child of pp
           (let* ((coordination-unit (loop for unit in unit-structure
                                           when (and (find 'cc (feature-value (find 'syn-class (unit-body unit) :key #'feature-name)):test #'equal)
                                                     (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                           return unit))
                  (sub-pp-unit (loop for unit in unit-structure
                                     when (and (find 'pp (feature-value (find 'syn-class (unit-body unit) :key #'feature-name)):test #'equal)
                                               (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                     return unit))
                  (prep-unit (loop for unit in unit-structure
                                   when (and (or (find 'in (feature-value (find 'syn-class (unit-body unit) :key #'feature-name)):test #'equal)
                                                 (equal 'prep (feature-value (find 'dependency-label (unit-body unit) :key #'feature-name))))
                                             (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name sub-pp-unit)))
                                   return unit)))
             (if prep-unit
               (let ((lemma (cadr (find 'lemma (unit-body prep-unit) :key #'feature-name))))
                 (list
                  `(,(variablify (unit-name coordination-unit))
                    --
                    (parent ,(variablify (unit-name pp-unit)))
                    (syn-class (cc)))
                  `(,(variablify (unit-name sub-pp-unit))
                    --
                    (parent ,(variablify (unit-name pp-unit)))
                    (syn-class (pp)))
                  `(,(variablify (unit-name prep-unit))
                    --
                    (parent ,(variablify (unit-name sub-pp-unit)))
                    (lemma ,lemma))))
               (let* ((first-contituent-unit-name (first (unit-feature-value (unit-body pp-unit) 'constituents)))
                      (first-contituent-unit (loop for unit in unit-structure
                                                   when (equal (unit-name unit) first-contituent-unit-name)
                                                   return unit))
                      (lemma (cadr (find 'lemma (unit-body first-contituent-unit) :key #'feature-name)))
                      (string (unless lemma (cadr (find 'string (unit-body first-contituent-unit) :key #'feature-name)))))
                 
                 (assert (or lemma string))
                                                  
                 (list
                  `(,(variablify first-contituent-unit-name)
                    --
                    (parent ,(variablify (unit-name pp-unit)))
                    ,@(when lemma `((lemma ,lemma)))
                    ,@(when string `((string ,string))))))))))))

             
(defun make-propbank-conditional-units-without-role (units-with-role cxn-units-with-role unit-structure)
  "Makes conditional units that are needed in a propbank cxn to encode
the paths in the syntactic tree between units that function as slot
fillers (arg0, arg1) and the frame-evoking element unit."
  (remove-duplicates
   (loop with fee-unit = (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                       units-with-role))
         for unit-with-role in (remove fee-unit units-with-role :test #'equal) ;;discard the frame-evoking element (FEE) unit
         for path = (find-path-in-syntactic-tree (cdr unit-with-role) fee-unit unit-structure) ;;find path between a unit in the transient structure and the FEE unit
         append (progn (assert path)
                  (loop for unit-name in path
                        for unit = (find unit-name unit-structure :key #'unit-name)
                        for parent = (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                                       (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                        unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name) ;;check that the unit is not a frame-element
                        collect `(,(variablify unit-name)
                                  --
                                  ,@(when parent
                                      `((parent ,parent)))
                                  ,@(when (has-siblings? unit-with-role units-with-role)
                                      `((word-order ,(make-form-constraints-for-children-with-role-and-same-type unit cxn-units-with-role))))
                                  ,(find 'syn-class (unit-body unit) :key #'feature-name)
                                  ,@(when (find 'passive (unit-body unit) :key #'feature-name)
                                      `(,(find 'passive (unit-body unit) :key #'feature-name)))))))
   :key #'unit-name))

(defun has-siblings? (unit other-units)
  "Checks whether a unit shares its parent with a unit from the other-units list."
  (let ((possible-siblings (remove-if #'(lambda(u) (or (string= (role-type (car u)) "V")
                                                       (string= (role-type (car u))
                                                                (role-type (car unit)))))
                                      other-units)))

    (assert (null (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                           possible-siblings)))
  
    (loop with parent-unit-name = (cadr (find 'parent (unit-body unit) :key #'feature-name))
          for other-unit in possible-siblings
          when (equal (cadr (find 'parent (unit-body other-unit) :key #'feature-name))
                      parent-unit-name)
          do (return t))))

(defun make-form-constraints-for-children-with-role-and-same-type (unit cxn-units-with-role)
  (let* ((children (loop for role-unit in cxn-units-with-role
                         when (equalp (subseq (symbol-name (feature-value (find 'parent (cddr role-unit) :key #'first :test #'equalp))) 1)
                                      (symbol-name (unit-name unit)))
                         collect role-unit))
         (children-per-type (mapcar #'rest
                                    (group-by children #'(lambda (unit)
                                                           (if (find '(node-type leaf) (cddr unit) :test #'equal)
                                                             (feature-value (find 'lex-class (cddr unit) :key #'first :test #'equalp))
                                                             (feature-value (find 'phrase-type (cddr unit) :key #'first :test #'equalp))))
                                              :test #'equalp )))
         (form-constraints-in-unit (feature-value (find 'word-order (unit-body unit) :key #'first :test #'equalp))))

    (loop with fcs-to-keep = nil
          for fc in form-constraints-in-unit
          for first-unit-name = (variablify (second fc))
          for second-unit-name = (variablify (third fc))
          do (let ((fc-to-keep (loop for group in children-per-type
                                  if (and (find first-unit-name group :key #'unit-name :test #'string=)
                                          (find second-unit-name group :key #'unit-name :test #'string=))
                                  collect (list (first fc) first-unit-name second-unit-name))))
               (when fc-to-keep 
                 (setf fcs-to-keep (append fcs-to-keep fc-to-keep))))
          
          finally (return fcs-to-keep))))

(defun find-path-in-syntactic-tree (unit v-unit unit-structure)
  "A search process that finds a path between two units in a transient
structure. The path is returned as a list of unit names, ordered from
start to end(v-unit)"
  (let ((queue (list `((:unit-name . ,(unit-name unit))
                       (:parent . ,(cadr (find 'parent (unit-body unit) :key #'feature-name)))
                       (:constituents .  ,(cadr (find 'constituents (unit-body unit) :key #'feature-name)))
                       (:path . ((,(unit-name unit) . :initial)))))))

    (loop while queue
          for state = (pop queue)
          if (equal (cdr (assoc :unit-name state)) (unit-name v-unit)) ;;solution found
          return (mapcar #'car (cdr (assoc :path state)))
          else do (setf queue (append queue (make-constituent-and-parent-states state unit-structure))))))

(defun make-constituent-and-parent-states (state unit-structure)
  "Creates new states for parent and constituents of current state"
  (let ((new-states nil))
    ;;First create states based on the constituents of the current state
    (loop for constituent-name in (cdr (assoc :constituents state)) 
          for constituent-unit = (find constituent-name unit-structure :key #'unit-name)
          for new-state = `((:unit-name . ,constituent-name)
                            (:parent . ,(cadr (find 'parent (unit-body constituent-unit) :key #'feature-name)))
                            (:constituents . ,(cadr (find 'constituents (unit-body constituent-unit) :key #'feature-name)))
                            (:path . ,(append (cdr (assoc :path state)) (list (cons constituent-name :constituent)))))
          unless (find constituent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          do (push new-state new-states))

    ;;Now create a state for the parent of the current state
    (when (assoc :parent state)
      (let* ((parent-name (cdr (assoc :parent state)))
             (parent-unit (find parent-name unit-structure :key #'unit-name))
             (new-state `((:unit-name . ,parent-name)
                          (:parent . ,(cadr (find 'parent (unit-body parent-unit) :key #'feature-name)))
                          (:constituents . ,(cadr (find 'constituents (unit-body parent-unit) :key #'feature-name)))
                          (:path . ,(append (cdr (assoc :path state)) (list (cons parent-name :parent)))))))
        (unless (find parent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          (push new-state new-states))))
    new-states))

(defmethod all-rolesets ((sentence fcg-propbank-sentence))
  "Returns all propbank frames with which a sentence was annotated."
  (remove-duplicates (mapcar #'frame-name (propbank-frames sentence)) :test #'equalp))


(defmethod ts-unit-structure ((sentence fcg-propbank-sentence) (cxn-inventory fcg-construction-set))
  "Returns the unit structure based on the syntactic analysis."
  (left-pole-structure (initial-transient-structure sentence))) 

(defun units-with-role (ts-unit-structure gold-frame)
  "Returns (cons . units) for each unit in the unit-structure that is an elements of the frame."
  (loop for role in (frame-roles gold-frame) ;;find all units that correspond to annotated frame elements
        for role-start = (first (indices role))
        for role-end = (+ (last-elt (indices role)) 1)
        for unit = (find-unit-by-span ts-unit-structure (list role-start role-end))
        collect (cons role unit)))

(defun v-lemma (units-with-role)
  "Returns the lemma of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return (or (feature-value (find 'lemma (unit-body unit) :key #'feature-name))
                   (feature-value (find 'string (unit-body unit) :key #'feature-name)))))

(defun v-unit (units-with-role)
  "Returns unit of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return unit))

(defun v-unit-with-role (units-with-role)
  "Returns the unit with role of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return (cons role unit)))

(defun v-syn-class (units-with-role)
  "Returns the syn-class of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return (feature-value (find 'syn-class (unit-body unit) :key #'feature-name))))

(defun syn-classes (cxn-units)
  (mapcar #'(lambda (unit)
              (second (find 'syn-class (cddr unit) :key #'first)))
          cxn-units))

(defun lemmas (cxn-units)
  (mapcar #'(lambda (unit)
              (second (find 'lemma (cddr unit) :key #'first)))
          cxn-units))


(defun argm-lemma (unit-with-role)
  "Returns the lemma of the unit."
  (feature-value (find 'lemma (unit-body (cdr unit-with-role)) :key #'feature-name)))


(defun find-equivalent-cxn (schema syn-classes cxn-inventory &key (hash-key nil))
  "Returns true if an equivalent-cxn is already present in the cxn-inventory."
  (loop for cxn in (gethash hash-key (constructions-hash-table cxn-inventory))
        when (and (equal (attr-val cxn :schema) schema)
                  (equalp syn-classes
                          (mapcar #'(lambda (unit)
                                      (second (find 'syn-class (comprehension-lock unit) :key #'first)))
                                  (conditional-part cxn))))
        return cxn))