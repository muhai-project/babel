(in-package :propbank-english)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Learning constructions based on Propbank annotated corpora.  ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-propbank-grammar (list-of-propbank-sentences &key
                                                          (selected-rolesets nil) 
                                                          (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                          (fcg-configuration nil))
  "Learns a Propbank Grammar."
  (let ((cxn-inventory (eval `(def-fcg-constructions-with-type-hierarchy propbank-learned-english
                                :fcg-configurations ,fcg-configuration
                                :visualization-configurations ((:show-constructional-dependencies . nil)
                                                               (:show-categorial-network . nil))
                                :hierarchy-features (constituents dependents)
                                :feature-types ((constituents sequence)
                                                (dependents sequence)
                                                (span sequence)
                                                (syn-class set)
                                                (args set-of-predicates)
                                                (word-order set-of-predicates)
                                                (meaning set-of-predicates)
                                                (footprints set))
                                :cxn-inventory ,cxn-inventory
                                :hashed t))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          for rolesets = (if selected-rolesets
                           (intersection selected-rolesets (all-rolesets sentence) :test #'equalp)
                           (all-rolesets sentence))
          do
          (when (= 0 (mod sentence-number 100))
            (format t "~%---> Sentence ~a." sentence-number))
          (loop for roleset in rolesets
                if (spacy-benepar-compatible-annotation sentence roleset)
                do
                (loop for mode in (get-configuration cxn-inventory :learning-modes)
                      do
                      (learn-from-propbank-annotation sentence roleset cxn-inventory mode)))
          finally
          return cxn-inventory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning constructions from an annotated frame instance. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory mode))

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :core-roles)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :core-roles)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (core-units-with-role (remove-if #'(lambda (unit-with-role)
                                              (search "ARGM" (role-type (car unit-with-role))))
                                          (units-with-role ts-unit-structure gold-frame)))
         (lex-category (add-lexical-cxn gold-frame (v-unit core-units-with-role) cxn-inventory propbank-sentence))
         (gram-category (when lex-category
                          (add-grammatical-cxn gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category))))
    (when gram-category
      (add-word-sense-cxn gold-frame (v-unit core-units-with-role) cxn-inventory propbank-sentence lex-category gram-category))
    
    ))

(defun add-lexical-cxn (gold-frame v-unit cxn-inventory propbank-sentence)
  "Creates a new lexical construction if necessary, otherwise increments frequency of existing cxn."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
         (lex-category (make-id (format nil "~a~a" (truncate-frame-name (frame-name gold-frame)) syn-class)))
         (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class))))
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))
    (if equivalent-cxn
      ;; if cxn already exists: increment frequency
      (progn
        (incf (attr-val equivalent-cxn :frequency))
        (attr-val equivalent-cxn :lex-category))
      ;; Else make new cxn
      (when lemma
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?lex-unit
                         (footprints (lex))
                         (lex-category ,lex-category))
                        <-
                        (?lex-unit
                         --
                         (footprints (NOT lex))
                         (lemma ,lemma)
                         (syn-class ,syn-class)))
                       :attributes (:lemma ,lemma
                                    :lex-category ,lex-category
                                    :score 1
                                    :label lexical-cxn
                                    :frequency 1
                                    :utterance ,(sentence-string propbank-sentence))
                       :cxn-inventory ,cxn-inventory))
        (add-category lex-category (get-type-hierarchy cxn-inventory))
        lex-category))))


(defmethod add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category)
  "Learns a construction capturing all core roles."
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (pp-units-with-role (remove-if-not #'(lambda (unit-w-role)
                                                (find 'pp (unit-feature-value (cdr unit-w-role) 'syn-class)))
                                            core-units-with-role))
         (s-bar-units-with-role  (remove-if-not #'(lambda (unit-w-role)
                                                    (find 'sbar (unit-feature-value (cdr unit-w-role) 'syn-class)))
                                                core-units-with-role))
         (gram-category (make-const (format nil "~{~a~^-~}" (loop for (r . u) in core-units-with-role
                                                                   collect (format nil "~a~a"
                                                                                   (role-type r)
                                                                                   (feature-value (find 'syn-class (unit-body u)
                                                                                                        :key #'feature-name)))))))

         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect
                                    (make-propbank-conditional-unit-with-role unit gram-category)))
         
        
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame gram-category))

         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                                 cxn-units-with-role ts-unit-structure))
         (cxn-preposition-units (loop for pp-unit in pp-units-with-role
                                      collect (make-preposition-unit pp-unit ts-unit-structure)))
         (cxn-s-bar-units  (loop for s-bar-unit in s-bar-units-with-role
                                 collect (make-subclause-word-unit s-bar-unit ts-unit-structure)))
         (cxn-name  (make-cxn-name core-units-with-role cxn-units-with-role cxn-units-without-role cxn-preposition-units cxn-s-bar-units))
         (cxn-preposition-units-flat  (loop for unit in cxn-preposition-units append unit))
         (cxn-s-bar-units-flat (loop for unit in cxn-s-bar-units append unit))
         (schema (loop with pp-unit-number = 1
                       with s-bar-unit-number = 1
                       for (role . unit) in core-units-with-role
                       for cxn-unit in cxn-units-with-role
                       collect (cons (intern (role-type role))
                                     (cond
                                      ;; unit is a pp
                                      ((find 'pp (unit-feature-value (unit-body unit) 'syn-class))
                                       (incf pp-unit-number)
                                       (if (= 1 (length (nth1 pp-unit-number cxn-preposition-units)))
                                         (intern (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class )
                                                         (second (find 'lemma
                                                                       (nthcdr 2 (first (nth1 pp-unit-number cxn-preposition-units)))
                                                                       :key #'feature-name))))
                                         (intern (format nil "~{~a~}(cc-~a)" (unit-feature-value unit 'syn-class )
                                                         (second (find 'lemma
                                                                       (nthcdr 2 (third (nth1 pp-unit-number cxn-preposition-units)))
                                                                       :key #'feature-name))))))
                                      ;; unit is an s-bar
                                      ((find 'sbar (unit-feature-value (unit-body unit) 'syn-class))
                                       (incf s-bar-unit-number)
                                       (if (= 1 (length (nth1 s-bar-unit-number cxn-s-bar-units)))
                                         (intern (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class)
                                                         (second (find 'lemma
                                                                       (nthcdr 2 (first (nth1 s-bar-unit-number cxn-s-bar-units)))
                                                                       :key #'feature-name))))))
                                      ;; unit contains a lemma
                                      ((feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)))
                                      ;; unit contains a phrase-type
                                      ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role
                                                                   cxn-preposition-units-flat
                                                                   cxn-s-bar-units-flat))
                                              cxn-inventory)))
                         
    
    (if equivalent-cxn
      (progn
        (add-link lex-category
                  (attr-val equivalent-cxn :gram-category) (get-type-hierarchy cxn-inventory))
        (incf (attr-val equivalent-cxn :frequency))
        (attr-val equivalent-cxn :gram-category))
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        ;;create a new construction and add it to the cxn-inventory
        (add-category gram-category (get-type-hierarchy cxn-inventory))
        (add-link lex-category gram-category (get-type-hierarchy cxn-inventory))
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  ,@cxn-preposition-units-flat
                                  ,@cxn-s-bar-units-flat)
                            :disable-automatic-footprints t
                            :attributes (:lemma nil
                                              :score ,(length cxn-units-with-role)
                                              :label argument-structure-cxn
                                              :frequency 1
                                              :gram-category ,gram-category
                                              :utterance ,(sentence-string propbank-sentence)
                                              :schema ,schema)
                                 :cxn-inventory ,cxn-inventory))
        gram-category))))


(defun add-word-sense-cxn (gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category)
  "Creates a new lexical construction if necessary, otherwise increments frequency of existing cxn."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (cxn-name (intern (upcase (format nil "~a-cxn" (frame-name gold-frame)))))
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name))
         (sense-category (make-id (frame-name gold-frame))))
    (if equivalent-cxn
      ;; if cxn already exists: increment frequency
      (progn
        (add-link gram-category (attr-val equivalent-cxn :sense-category) (get-type-hierarchy cxn-inventory))
        (add-link (attr-val equivalent-cxn :sense-category) lex-category (get-type-hierarchy cxn-inventory))
        (incf (attr-val equivalent-cxn :frequency))
        (attr-val equivalent-cxn :sense-category))
      ;; Else make new cxn
      (when lemma
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?lex-unit
                         (footprints (ws)))<-
                        (?lex-unit
                         --
                         (lemma ,lemma)
                         (gram-category ,sense-category)
                         (lex-category ,sense-category)
                         (frame ,(frame-name gold-frame))
                         (footprints (NOT ws))))
                       :disable-automatic-footprints t
                       :attributes (:lemma ,lemma
                                    :sense-category ,sense-category
                                    :score 1
                                    :label word-sense-cxn
                                    :frequency 1
                                    :utterance ,(sentence-string propbank-sentence))
                       :cxn-inventory ,cxn-inventory))
        (add-category sense-category (get-type-hierarchy cxn-inventory))
        (add-link gram-category sense-category (get-type-hierarchy cxn-inventory))
        (add-link sense-category lex-category (get-type-hierarchy cxn-inventory))
        sense-category))))




(defun syn-classes (cxn-units)
  (mapcar #'(lambda (unit)
              (second (find 'syn-class (cddr unit) :key #'first)))
          cxn-units))

(defun lemmas (cxn-units)
  (mapcar #'(lambda (unit)
              (second (find 'lemma (cddr unit) :key #'first)))
          cxn-units))

















(defun make-subclause-word-unit (unit-with-role unit-structure)
  (let* ((sbar-unit (cdr unit-with-role))
         (subclause-word-in-ts (loop for unit in unit-structure
                                     when (and (find (list 'node-type 'leaf) (unit-body unit) :test #'equalp)
                                               (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name sbar-unit)))
                                     return unit)))

         (cond (subclause-word-in-ts
                (list
                 `(,(variablify (unit-name subclause-word-in-ts))
                   --
                   (parent ,(variablify (unit-name sbar-unit)))
                   (lemma ,(cadr (find 'lemma (unit-body subclause-word-in-ts) :key #'feature-name))))))
               (t 
                  ;(break)
                  nil))))



(defun make-cxn-name (ts-units-with-role cxn-units-with-role cxn-units-without-role preposition-units s-bar-units)
  (loop with pp-unit-number = 0
        with s-bar-unit-number = 0
        for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        collect (format nil "~a:~a"
                        (role-type role)
                        (cond
                         ;; unit is a pp
                         ((find 'pp (unit-feature-value (unit-body unit) 'syn-class))
                          (incf pp-unit-number)
                          (if (= 1 (length (nth1 pp-unit-number preposition-units)))
                            (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class )
                                    (second (find 'lemma
                                                  (nthcdr 2 (first (nth1 pp-unit-number preposition-units)))
                                                  :key #'feature-name)))
                            (format nil "~{~a~}(cc-~a)" (unit-feature-value unit 'syn-class )
                                    (second (find 'lemma
                                                  (nthcdr 2 (third (nth1 pp-unit-number preposition-units)))
                                                  :key #'feature-name)))))
                         ;; unit is an s-bar
                         ((find 'sbar (unit-feature-value (unit-body unit) 'syn-class))
                          (incf s-bar-unit-number)
                          (if (= 1 (length (nth1 s-bar-unit-number s-bar-units)))
                            (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class)
                                    (second (find 'lemma
                                                  (nthcdr 2 (first (nth1 s-bar-unit-number s-bar-units)))
                                                  :key #'feature-name)))))
                         ;; unit contains a lemma
                         ((feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)))
                         ;; unit contains a phrase-type
                         ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))
                          (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))))
        into roles
        finally return (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role))))))

  
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

  
(defun make-propbank-contributing-unit (units-with-role gold-frame gram-category)
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
      (footprints (fee))
      (gram-category ,gram-category)
      (frame ?roleset)
      (meaning ,meaning))))

(defun make-propbank-conditional-unit-with-role (unit-with-role category)
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                   (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name)))))
         (syn-class (find 'syn-class (unit-body unit) :key #'feature-name)))
    ;;a FEE unit also has the features lemma and footprints
    (if (equalp "V" (role-type (car unit-with-role)))
      `(,unit-name
        --
        (parent ,parent)
        ,syn-class
        (footprints (NOT fee))
        (lex-category ,category))
      `(,unit-name
        --
        (parent ,parent)
        ,syn-class))))




(defun make-preposition-unit (unit-with-role unit-structure)
  (let* ((pp-unit (cdr unit-with-role))
         (preposition-unit-in-ts (loop for unit in unit-structure
                                       when (and (or (find 'in (feature-value (find 'syn-class (unit-body unit) :key #'feature-name)):test #'equal)
                                                     (equal 'prep (feature-value (find 'dependency-label (unit-body unit) :key #'feature-name))))
                                                 (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                       return unit)))

    (cond (preposition-unit-in-ts
           (list
            `(,(variablify (unit-name preposition-unit-in-ts))
              --
              (parent ,(variablify (unit-name pp-unit)))
              (lemma ,(cadr (find 'lemma (unit-body preposition-unit-in-ts) :key #'feature-name))))))
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
             (when prep-unit
               (list
                `(,(variablify (unit-name coordination-unit))
                  --
                  (parent ,(variablify (unit-name pp-unit)))
                  (syn-class (cc)))
                `(,(variablify (unit-name sub-pp-unit))
                  --
                  (parent ,(variablify (unit-name coordination-unit)))
                  (syn-class (pp)))
                `(,(variablify (unit-name prep-unit))
                  --
                  (parent ,(variablify (unit-name sub-pp-unit)))
                  (lemma ,(cadr (find 'lemma (unit-body prep-unit) :key #'feature-name)))))))))))

             
(defun make-propbank-conditional-units-without-role (units-with-role cxn-units-with-role unit-structure)
  "Makes conditional units that are needed in a propbank cxn to encode
the paths in the syntactic tree between units that function as slot
fillers (arg0, arg1) and the frame-evoking element unit."
  (remove-duplicates
   (loop with fee-unit = (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                       units-with-role))
         for unit-with-role in (remove fee-unit units-with-role :test #'equal) ;;discard the frame-evoking element (FEE) unit
         for path = (find-path-in-syntactic-tree (cdr unit-with-role) fee-unit unit-structure) ;;find path between a unit in the transient structure and the FEE unit

         append (loop for unit-name in path
                      for unit = (find unit-name unit-structure :key #'unit-name)
                      for parent = (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                                     (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                      for form-constraints-for-children-with-role-and-same-type = (make-form-constraints-for-children-with-role-and-same-type unit cxn-units-with-role)

                      unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name) ;;check that the unit is not a frame-element
                      collect `(,(variablify unit-name)
                                --
                                ,@(when parent
                                    `((parent ,parent)))
                                ,@(when form-constraints-for-children-with-role-and-same-type
                                   `((word-order ,form-constraints-for-children-with-role-and-same-type)))
                                ,(find 'syn-class (unit-body unit) :key #'feature-name))))
   :key #'unit-name))


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
          
          finally return fcs-to-keep)))


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

(defmethod all-rolesets ((sentence conll-sentence))
  "Returns all propbank frames with which a sentence was annotated."
  (mapcar #'frame-name (propbank-frames sentence)))


(defmethod ts-unit-structure ((sentence conll-sentence) (cxn-inventory fcg-construction-set))
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
        return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))

(defun v-unit (units-with-role)
  "Returns the lemma of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return unit))

(defun v-syn-class (units-with-role)
  "Returns the syn-class of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return (feature-value (find 'syn-class (unit-body unit) :key #'feature-name))))


(defun argm-lemma (unit-with-role)
  "Returns the lemma of the unit."
  (feature-value (find 'lemma (unit-body (cdr unit-with-role)) :key #'feature-name)))


(defun find-equivalent-cxn (schema syn-classes cxn-inventory )
  "Returns true if an equivalent-cxn is already present in the cxn-inventory."
  (loop for cxn in (gethash nil (constructions-hash-table cxn-inventory))
        when (and (equal (attr-val cxn :schema) schema)
                  (equalp syn-classes
                          (mapcar #'(lambda (unit)
                                      (second (find 'syn-class (comprehension-lock unit) :key #'first)))
                                  (conditional-part cxn))))
        return cxn))

