(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Learning constructions based on Propbank annotated corpora.  ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions propbank-learned
  :visualization-configurations ((:show-constructional-dependencies . t)
                                 (:show-categorial-network . nil)
                                 (:hide-attributes . t)
                                 (:hide-features . nil))
  :fcg-configurations ((:node-tests :check-double-role-assignment)
                       (:parse-goal-tests :no-valid-children)
                       (:max-nr-of-nodes . 200)
                       (:de-render-mode . :de-render-constituents-dependents)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:search-algorithm . :best-first)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:heuristics :edge-weight :nr-of-roles-integrated)
                       (:cxn-supplier-mode . :hashed-categorial-network)
                       (:sort-cxns-before-application . nil)
                       (:node-expansion-mode . :full-expansion)
                       (:hash-mode . :hash-lemma))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents sequence)
                  (dependents sequence)
                  (span sequence)
                  (syn-class set)
                  (args set-of-predicates)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :hashed t)

(defun learn-propbank-grammar (list-of-fcg-propbank-sentences &key
                                                              (selected-rolesets nil)
                                                              (excluded-rolesets nil)
                                                              (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                              (fcg-configuration nil))
  
  "Learns a PropBank grammar based on a corpus of PropBank-annotated sentences."
  ;; Initializing the inventory
  (let ((cxn-inventory (eval `(setf ,cxn-inventory (make-propbank-learned-cxns)))))
    (set-configurations (configuration cxn-inventory) fcg-configuration)
    (set-data (blackboard cxn-inventory) :training-corpus-size 0)
    
    ;; Training the grammar
    (loop for fcg-propbank-sentence in list-of-fcg-propbank-sentences
          for sentence-number from 1
          for training-corpus-size = (get-data (blackboard cxn-inventory) :training-corpus-size)
          for annotated-rolesets = (mapcar #'frame-name (propbank-frames fcg-propbank-sentence))
          for rolesets = (cond (selected-rolesets
                                (intersection selected-rolesets annotated-rolesets :test #'equalp))
                               (excluded-rolesets
                                (loop for roleset in annotated-rolesets
                                      unless (find roleset excluded-rolesets :test #'equalp)
                                        collect roleset))
                               (t
                                annotated-rolesets))
          do
            (when (= 0 (mod sentence-number 100))
              (format t "~%---> Sentence ~a." sentence-number))
            (when rolesets
              (set-data (blackboard cxn-inventory) :training-corpus-size (incf training-corpus-size)))
            (loop for roleset in rolesets
                  do 
                    (loop for mode in (get-configuration cxn-inventory :learning-modes)
                          do
                            (learn-from-propbank-annotation fcg-propbank-sentence roleset cxn-inventory mode)))
          finally
            (return cxn-inventory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning constructions from an annotated frame instance. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric learn-from-propbank-annotation (fcg-propbank-sentence roleset cxn-inventory mode)
  (:documentation "Learns constructions and categories from a single PropBank-annotated sentence."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core roles.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod learn-from-propbank-annotation (fcg-propbank-sentence roleset cxn-inventory (mode (eql :core-roles)))
  "Checks for every gold frame that has been annotated in the
propbank-sentence whether there is a spacy-benepar compatible
annotation (i.e. every argument corresponds to a constituent) and then
learns constructions and categories for that frame."
  (loop with gold-frames = (find-all roleset (propbank-frames fcg-propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation fcg-propbank-sentence roleset :selected-role-types 'core-only)
        do (learn-constructions-for-gold-frame-instance fcg-propbank-sentence gold-frame cxn-inventory mode)))

(defmethod learn-constructions-for-gold-frame-instance (fcg-propbank-sentence gold-frame cxn-inventory (mode (eql :core-roles)))
  "Learns lexical, argument structure and word sense constructions
along with the categories that connect them for all core roles that
have been annotated for the given gold-frame. "
  (let* ((ts-unit-structure (ts-unit-structure fcg-propbank-sentence cxn-inventory))
         (core-units-with-role (remove-if #'(lambda (unit-with-role)
                                              (search "ARGM" (role-type (car unit-with-role))))
                                          (units-with-role ts-unit-structure gold-frame))))
  
      (let* ((fe-category (add-lexical-cxn gold-frame (v-unit core-units-with-role) cxn-inventory fcg-propbank-sentence))
             (argst-category (when fe-category
                              (add-grammatical-cxn gold-frame core-units-with-role cxn-inventory fcg-propbank-sentence fe-category))))
        (when argst-category
          (add-word-sense-cxn gold-frame (v-unit core-units-with-role) cxn-inventory fcg-propbank-sentence fe-category argst-category)))))


(defun find-lexical-cxn (v-unit cxn-inventory)
  "Finds a lexical construction based on a v-unit."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
         (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class)) :fcg-propbank)))
    (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))

(defun add-lexical-cxn (gold-frame v-unit cxn-inventory propbank-sentence)
  "Creates a new lexical construction if necessary, otherwise
increments frequency of existing cxn. Also adds a new lexical category
to the categorial network. Returns the lexical category."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
         (fe-category (intern (symbol-name (make-id (format nil "~a~a" (truncate-frame-name (frame-name gold-frame)) syn-class)))
                               :fcg-propbank))
         (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class)) :fcg-propbank))
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))
    (if equivalent-cxn
      ;; If cxn already exists: increment frequency
      (progn
        (incf (attr-val equivalent-cxn :score))
        (attr-val equivalent-cxn :fe-category))
      ;; Else make new cxn
      (when lemma
        (if (equalp syn-class '(vp))
          (let ((lex-lemma (intern (subseq (symbol-name lemma) 0 (search "-" (symbol-name lemma))) :fcg-propbank)))
            (eval
             `(def-fcg-cxn ,cxn-name
                           ((?phrasal-unit
                             (footprints (lex))
                             (fe-category ,fe-category))
                            (?lex-unit
                             (footprints (lex)))
                            <-
                            (?phrasal-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lemma)
                             (syn-class ,syn-class))
                            (?lex-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lex-lemma)
                             (parent ?phrasal-unit)))
                           :attributes (:lemma ,lemma
                                        :fe-category ,fe-category
                                        :label lexical-cxn
                                        :score 1)
                           :description ,(sentence-string propbank-sentence)
                           :disable-automatic-footprints t
                           :cxn-inventory ,cxn-inventory)))
            (eval
             `(def-fcg-cxn ,cxn-name
                           ((?lex-unit
                             (footprints (lex))
                             (fe-category ,fe-category))
                            <-
                            (?lex-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lemma)
                             (syn-class ,syn-class)))
                           :attributes (:lemma ,lemma
                                        :fe-category ,fe-category
                                        :label lexical-cxn
                                        :score 1)
                           :description ,(sentence-string propbank-sentence)
                           :disable-automatic-footprints t
                           :cxn-inventory ,cxn-inventory)))
        (add-category fe-category cxn-inventory :recompute-transitive-closure nil)
          fe-category))))



(defun add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence fe-category)
  "Learns a grammatical construction capturing all core roles and adds
a grammatical category to the categorial network. Returns the
grammatical category."
  
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (argst-category (make-argst-category core-units-with-role))
         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect (make-propbank-conditional-unit-with-role unit argst-category 'fee)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                               cxn-units-with-role ts-unit-structure))
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame argst-category 'fee))
         (schema (make-cxn-schema core-units-with-role cxn-units-with-role :core-roles))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" argst-category (length cxn-units-without-role))) :fcg-propbank))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        ;;2) Check if there was already a link in the categorial network between the fe-category and the argst-category:
        (if (link-exists-p fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type nil
                      :recompute-transitive-closure nil)
            (add-link fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type 'lex-gram
                      :recompute-transitive-closure nil)))
        ;;3) Return argst-category
        (attr-val equivalent-cxn :argst-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category argst-category cxn-inventory :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role
                             )
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma nil
                                         :label argument-structure-cxn
                                         :score 1
                                         :argst-category ,argst-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        argst-category))))


(defun add-word-sense-cxn (gold-frame v-unit cxn-inventory propbank-sentence fe-category argst-category)
  "Creates a new word sense construction if necessary, otherwise
increments frequency of existing cxn. Adds a new sense category to the
categorial network and returns it."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (feature-value (find 'string (unit-body v-unit) :key #'feature-name))))
         (cxn-name (intern (upcase (format nil "~a(~a)-cxn" (frame-name gold-frame) lemma)) :fcg-propbank))
         
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key (if (stringp lemma)
                                                                      (intern (upcase lemma) :fcg-propbank)
                                                                      lemma) :key #'name))
         (roleset-category (intern (symbol-name (frame-name gold-frame)) :fcg-propbank)))
    
    (if equivalent-cxn
      
      ;; If word sense cxn already exists
      ;;---------------------------------
      (progn
        (incf (attr-val equivalent-cxn :score))
        
        ;; edge between argst-category and roleset-category
        (if (link-exists-p argst-category (attr-val equivalent-cxn :roleset-category) cxn-inventory)
          ;;connection between gram and sense category exists: increase edge weight
          (progn
            (incf-link-weight argst-category (attr-val equivalent-cxn :roleset-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight argst-category (attr-val equivalent-cxn :roleset-category) cxn-inventory :delta 1.0 :link-type 'gram-sense))
          ;;add new link
          (progn
            (add-link argst-category
                      (attr-val equivalent-cxn :roleset-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link argst-category
                      (attr-val equivalent-cxn :roleset-category) cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)))
        
        ;; edge between fe-category and roleset-category
        (if (link-exists-p fe-category (attr-val equivalent-cxn :roleset-category) cxn-inventory)
          (progn
            (incf-link-weight fe-category (attr-val equivalent-cxn :roleset-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight fe-category (attr-val equivalent-cxn :roleset-category) cxn-inventory :delta 1.0 :link-type 'lex-sense))
          (progn
            (add-link fe-category
                      (attr-val equivalent-cxn :roleset-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link fe-category
                      (attr-val equivalent-cxn :roleset-category) cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)))

        (attr-val equivalent-cxn :roleset-category))
      
      ;; Else make new cxn
      ;;-------------------
      (progn (assert lemma)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?lex-unit
                         (footprints (ws)))
                        <-
                        (?lex-unit
                         --
                         ,@(if (stringp lemma)
                             `((string ,lemma))
                             `((lemma ,lemma)))
                         (argst-category ,roleset-category)
                         (fe-category ,roleset-category)
                         (frame ,(intern (upcase (frame-name gold-frame)) :fcg-propbank))
                         (footprints (NOT ws))))
                       :disable-automatic-footprints t
                       :attributes (:lemma ,(if (stringp lemma)
                                              (intern (upcase lemma) :fcg-propbank)
                                              lemma)
                                    :roleset-category ,roleset-category
                                    :label word-sense-cxn
                                    :score 1)
                       :description ,(sentence-string propbank-sentence)
                       :cxn-inventory ,cxn-inventory))
        
        (add-category roleset-category cxn-inventory :recompute-transitive-closure nil)
        (add-link argst-category roleset-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link argst-category roleset-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)
        (add-link fe-category roleset-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link fe-category roleset-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)
        
        roleset-category))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM single word      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-leaf)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-leaf)))
  "Learns a new construction for every modifier argument in the gold frame that corresponds to a leaf node."
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-leafs (remove-if-not #'(lambda (unit-with-role)
                                      (and (search "ARGM" (role-type (car unit-with-role)))
                                           (equalp (unit-feature-value (cdr unit-with-role) 'node-type) 'leaf)))
                                  units-with-role)))

    (loop with v-unit-with-role = (v-unit-with-role units-with-role)
          for argm-leaf in argm-leafs
          for argm-unit-name = (unit-name (cdr argm-leaf))
          append (loop with v-unit-found? = nil
                       for (role . unit) in units-with-role
                       for unit-name = (unit-name unit)
                       if (string= (role-type role) "V")
                       do (setf v-unit-found? t)
                       else if (equal unit-name argm-unit-name)
                       collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-leaf unit
                                                        (list v-unit-with-role argm-leaf)
                                                        (list argm-leaf v-unit-with-role))))
                                 (add-argm-leaf-cxn gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure))))))
   

(defun add-argm-leaf-cxn (gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure)
  "Learns a construction capturing V + ARGM-argm."
  (let* ((argm-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (argm-lemma (unit-feature-value (unit-body argm-unit) 'lemma))
         (footprint (make-const 'argm))
         (cxn-units-with-role
          (loop for unit-w-role in units-with-role
                if (equal (role-type (car unit-w-role)) "V")
                collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :frame-evoking t)
                else collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :lemma argm-lemma)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame nil footprint :include-argst-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         (cxn-name (make-cxn-name units-with-role cxn-units-with-role cxn-units-without-role :argm-leaf :lemma argm-lemma))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-leaf :lemma argm-lemma))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory
                                              :hash-key argm-lemma)))

    (if equivalent-cxn
      
      ;; argm-leaf cxn already exists, update its frequency
      (incf (attr-val equivalent-cxn :score))
      
      ;; create a argm-leaf cxn
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,argm-lemma
                                         :label argm-leaf-cxn
                                         :score 1)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM PPs              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-pp)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-pp)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-pps (remove-if-not #'(lambda (unit-with-role)
                                      (and (search "ARGM" (role-type (car unit-with-role)))
                                           (find 'pp (unit-feature-value (cdr unit-with-role) 'syn-class))))
                                  units-with-role))
         (v-unit (v-unit units-with-role))
         (lex-cxn (find-lexical-cxn v-unit cxn-inventory))
         (fe-category (when lex-cxn (attr-val lex-cxn :fe-category)))
         (argst-categories
          (when fe-category
            (loop with v-unit-with-role = (v-unit-with-role units-with-role)
                  for argm-pp in argm-pps
                  for pp-unit-name = (unit-name (cdr argm-pp))
                  append (loop with v-unit-found? = nil
                                for (role . unit) in units-with-role
                                for unit-name = (unit-name unit)
                                if (string= (role-type role) "V")
                                do (setf v-unit-found? t)
                                else if (equal unit-name pp-unit-name)
                                collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-pp-unit
                                                                 (list v-unit-with-role argm-pp)
                                                                 (list argm-pp v-unit-with-role))))
                                          (add-pp-cxn gold-frame units-with-role cxn-inventory propbank-sentence fe-category ts-unit-structure)))))))
    
    (loop for argst-category in argst-categories
          for word-sense-cxn = (find-word-sense-cxn gold-frame v-unit cxn-inventory)
          if word-sense-cxn
          do (update-categorial-network fe-category argst-category (attr-val word-sense-cxn :roleset-category) cxn-inventory)
          else do (add-word-sense-cxn gold-frame v-unit cxn-inventory propbank-sentence fe-category argst-category))))




(defun find-word-sense-cxn (gold-frame v-unit cxn-inventory)
  "Find a word sense construction."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (intern (upcase (feature-value (find 'string (unit-body v-unit) :key #'feature-name))) :fcg-propbank)))
         (cxn-name (intern (upcase (format nil "~a-cxn" (frame-name gold-frame))) :fcg-propbank)))
    (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))


(defun update-categorial-network (fe-category argst-category roleset-category cxn-inventory)

  (if (cxn-inventory argst-category roleset-category cxn-inventory)
    ;;connection between gram and sense category exists: increase edge weight
    (progn
      (incf-link-weight argst-category roleset-category cxn-inventory :delta 1.0 :link-type nil)
      (incf-link-weight argst-category roleset-category cxn-inventory :delta 1.0 :link-type 'gram-sense))
    ;;add new link
    (progn
      (add-link argst-category roleset-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
      (add-link argst-category roleset-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)))

  (if (link-exists-p fe-category roleset-category cxn-inventory)
    ;;connection between gram and sense category exists: increase edge weight
    (progn
      (incf-link-weight fe-category roleset-category cxn-inventory :delta 1.0 :link-type nil )
      (incf-link-weight fe-category roleset-category cxn-inventory :delta 1.0 :link-type 'lex-sense))
    ;;add new link
    (progn
      (add-link fe-category roleset-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
      (add-link fe-category roleset-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil))))




(defun add-pp-cxn (gold-frame units-with-role cxn-inventory propbank-sentence fe-category ts-unit-structure)
  "Learns a construction capturing V + ARGM-pp."
  (let* ((pp-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                         (role-type (car unit-w-role))) :test #'search))
         (cxn-preposition-units (make-preposition-unit pp-unit ts-unit-structure)) ;;list with 1 or 3 units
         (preposition-lemma
          (if (= 1 (length cxn-preposition-units))
            (or (second (find 'lemma (nthcdr 2 (first cxn-preposition-units))
                              :key #'feature-name))
                (second (find 'string (nthcdr 2 (first cxn-preposition-units))
                              :key #'feature-name)))
            (second (find 'lemma (nthcdr 2 (third cxn-preposition-units))
                                          :key #'feature-name))))
         (argst-category (make-argst-category units-with-role preposition-lemma))
         (footprint (make-const 'pp))
         
         (cxn-units-with-role (loop for unit in units-with-role
                                     if (equal (role-type (car unit)) "V")
                                     collect (make-propbank-conditional-unit-with-role unit argst-category footprint :frame-evoking t)
                                     else collect (make-propbank-conditional-unit-with-role unit argst-category footprint)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame argst-category footprint :include-argst-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" argst-category (length cxn-units-without-role))) :fcg-propbank))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-pp :cxn-preposition-units (list cxn-preposition-units)))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role
                                                                   cxn-preposition-units))
                                              cxn-inventory
                                              :hash-key preposition-lemma)))

    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        ;;2) Check if there was already a link in the type hierarchy between the fe-category and the argst-category:
        (if (link-exists-p fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :link-type nil)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
          (add-link fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)))
        ;;3) Return argst-category
        (attr-val equivalent-cxn :argst-category))
      
      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;-------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (assert preposition-lemma)
        (add-category argst-category cxn-inventory :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  ,@cxn-preposition-units)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,preposition-lemma
                                         :label argm-phrase-cxn
                                         :score 1
                                         :argst-category ,argst-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        argst-category))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM S-BARs           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-sbar)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-sbar)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-sbars (remove-if-not #'(lambda (unit-with-role)
                                        (and (search "ARGM" (role-type (car unit-with-role)))
                                             (or (find 'sbar (unit-feature-value (cdr unit-with-role) 'syn-class))
                                                 (find 's (unit-feature-value (cdr unit-with-role) 'syn-class)))))
                                  units-with-role))
         (v-unit (v-unit units-with-role))
         (lex-cxn (find-lexical-cxn v-unit cxn-inventory))
         (fe-category (when lex-cxn (attr-val lex-cxn :fe-category)))
         (argst-categories
          (when fe-category
            (remove nil
                    (loop with v-unit-with-role = (v-unit-with-role units-with-role)
                          for argm-sbar in argm-sbars
                          for sbar-unit-name = (unit-name (cdr argm-sbar))
                          append (loop with v-unit-found? = nil
                                       for (role . unit) in units-with-role
                                       for unit-name = (unit-name unit)
                                       if (string= (role-type role) "V")
                                         do (setf v-unit-found? t)
                                       else if (equal unit-name sbar-unit-name)
                                              collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-pp-unit
                                                                               (list v-unit-with-role argm-sbar)
                                                                               (list argm-sbar v-unit-with-role))))
                                                        (add-sbar-cxn gold-frame units-with-role cxn-inventory propbank-sentence fe-category ts-unit-structure))))))))
            
    (loop for argst-category in argst-categories
          do (add-word-sense-cxn gold-frame v-unit cxn-inventory propbank-sentence fe-category argst-category)))) ;;only one cxn, multiple links in th


(defun add-sbar-cxn (gold-frame units-with-role cxn-inventory propbank-sentence fe-category ts-unit-structure)
  "Learns a construction capturing V + ARGM-sbar."
  (let* ((sbar-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (cxn-sbar-unit (make-subclause-word-unit sbar-unit ts-unit-structure))
         (sbar-lemma (second (or (find 'lemma (nthcdr 2 cxn-sbar-unit) :key #'feature-name)
                                 (find 'string (nthcdr 2 cxn-sbar-unit) :key #'feature-name))))
         (argst-category (make-argst-category units-with-role sbar-lemma))
         (footprint (make-const 'sbar))
          ;;1 unit
         (cxn-units-with-role (loop for unit in units-with-role
                                    if (equal (role-type (car unit)) "V")
                                      collect (make-propbank-conditional-unit-with-role unit argst-category footprint :frame-evoking t)
                                    else collect (make-propbank-conditional-unit-with-role unit argst-category footprint)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame argst-category footprint :include-argst-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" argst-category (length cxn-units-without-role))) :fcg-propbank))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-sbar :cxn-s-bar-units (list cxn-sbar-unit)))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role
                                                                   (list cxn-sbar-unit)))
                                              cxn-inventory
                                              :hash-key (if (stringp sbar-lemma)
                                                          (intern (upcase sbar-lemma) :fcg-propbank)
                                                          sbar-lemma))))
    (if equivalent-cxn
      
      ;;Grammatical construction already exists
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :score))
        
        ;;2) Check if there was already a link in the type hierarchy between the fe-category and the argst-category:
        (if (link-exists-p fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :link-type nil)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight fe-category (attr-val equivalent-cxn :argst-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link fe-category
                    (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
          (add-link fe-category
                    (attr-val equivalent-cxn :argst-category) cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)))
        
        ;;3) Return argst-category
        (attr-val equivalent-cxn :argst-category))
      
      ;;Create a new grammatical category for the observed pattern + add category and link to the type hierarchy
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (add-category argst-category cxn-inventory :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link fe-category argst-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)

        (unless (find (unit-name cxn-sbar-unit) cxn-units-with-role :key #'unit-name :test #'equal) ;;check for avoiding duplicate unit names as a consequence of too flat constituency structures
          (eval `(def-fcg-cxn ,cxn-name
                              (,contributing-unit
                               <-
                               ,@cxn-units-with-role
                               ,@cxn-units-without-role
                               ,cxn-sbar-unit)
                              :disable-automatic-footprints t
                              :attributes (:schema ,schema
                                           :lemma ,(if (stringp sbar-lemma)
                                                     (intern (upcase sbar-lemma) :fcg-propbank)
                                                     sbar-lemma)
                                           :label argm-phrase-cxn
                                           :score 1
                                           :argst-category ,argst-category)
                              :description ,(sentence-string propbank-sentence)
                              :cxn-inventory ,cxn-inventory))
          argst-category)))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM phrase with string          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;argm phrases that are not pps or sbar phrases but typically nps or advps

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-phrase-with-string)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-phrase-with-string)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-phrases (remove-if-not #'(lambda (unit-with-role)
                                        (and (search "ARGM" (role-type (car unit-with-role)))
                                             (equalp (unit-feature-value (cdr unit-with-role) 'node-type) 'phrase)
                                             (not (or (find 'sbar (unit-feature-value (cdr unit-with-role) 'syn-class)) ;;no sbar, s or pp phrases!
                                                      (find 's (unit-feature-value (cdr unit-with-role) 'syn-class))
                                                      (find 'pp (unit-feature-value (cdr unit-with-role) 'syn-class))))))
                                  units-with-role)))


    (loop with v-unit-with-role = (v-unit-with-role units-with-role)
          for argm-phrase in argm-phrases
          for argm-unit-name = (unit-name (cdr argm-phrase))
          append (loop with v-unit-found? = nil
                       for (role . unit) in units-with-role
                       for unit-name = (unit-name unit)
                       if (string= (role-type role) "V")
                       do (setf v-unit-found? t)
                       else if (equal unit-name argm-unit-name)
                       collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-unit
                                                        (list v-unit-with-role argm-phrase)
                                                        (list argm-phrase v-unit-with-role))))
                                 (add-argm-phrase-with-string-cxn gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure))))))

    
  
(defun add-argm-phrase-with-string-cxn (gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure)
  "Learns a construction capturing V + ARGM that is a phrase (but not pp/sbar/s). Categorial network not used."
  (let* ((argm-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (argm-string (unit-feature-value (cdr argm-unit) 'string))
         (footprint (make-const 'argm))
         (cxn-units-with-role
          (loop for unit-w-role in units-with-role
                if (equal (role-type (car unit-w-role)) "V")
                collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :frame-evoking t)
                else collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :string argm-string)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame nil footprint :include-argst-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))

         (cxn-name (make-cxn-name units-with-role cxn-units-with-role cxn-units-without-role :argm-phrase :phrase argm-string))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-phrase :phrase argm-string))
         
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory
                                              :hash-key (intern (upcase argm-string) :fcg-propbank))))

    (if equivalent-cxn
      
      ;;argm-leaf cxn already exists, update its frequency
      (incf (attr-val equivalent-cxn :score))
      
      ;;create a argm-leaf cxn
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,(intern (upcase argm-string) :fcg-propbank)
                                         :score ,(length cxn-units-with-role)
                                         :label argm-leaf-cxn
                                         :score 1)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))))))



(defgeneric make-cxn-name (ts-units-with-role 
                           cxn-units-with-role
                           cxn-units-without-role mode &key &allow-other-keys)
  (:documentation "Creates a unique construction name for an argument structure construction"))


(defmethod make-cxn-name ((ts-units-with-role list)
                          (cxn-units-with-role list)
                          (cxn-units-without-role list)
                          (mode (eql :argm-leaf))
                          &key lemma &allow-other-keys)
  "Creates a unique construction name for an argument structure
construction that links a predicate to a modifier through the
constituency path that links them."

  (declare (ignore mode))
  
  (loop for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        collect (format nil "~a(~a)"
                        (role-type role)
                        (if (equal (feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)) lemma)
                          lemma
                          (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))) ))
        into roles
        finally (return (intern (symbol-name (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role)))))))))

(defmethod make-cxn-name ((ts-units-with-role list)
                          (cxn-units-with-role list)
                          (cxn-units-without-role list)
                          (mode (eql :argm-phrase))
                          &key phrase &allow-other-keys)
  "Creates a unique construction name for an argument structure
construction that links a predicate to a modifier through the
constituency path that links them."

  (declare (ignore mode))

  (assert (= (length cxn-units-with-role) 2)) ;;v-unit + single argm-unit with string
  (assert phrase)
  
  (loop for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        if (string= (role-type role) "V")
          collect (format nil "V(~a)" (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))) into roles
        else
          collect (format nil "~a(~a)" (role-type role) phrase) into roles
        finally (return (intern (symbol-name (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role)))))))))
