(in-package :cgl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add Categorial links     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-categorial-links (add-cxns-and-th-links) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (unless (find-data (blackboard (construction-inventory node)) :add-th-links-repair-failed)
    (let ((cxns-and-th-links (create-th-links problem node)))
      (if cxns-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data cxns-and-th-links)
        (progn (set-data (blackboard (construction-inventory node)) :add-th-links-repair-failed t)
          nil)))))

(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-th-links (create-th-links problem node)))
    (when cxns-and-th-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-th-links))))

(defun disable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :th-connected-mode :path-exists)
  (set-configuration cxn-inventory :update-th-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :th-connected-mode :neighbours)
  (set-configuration cxn-inventory :update-th-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t))

(defun filter-by-phrase-type (type cxns)
  "returns all cxns in the list for the given type"
  (loop for cxn in cxns
        for orig-cxn = (get-original-cxn cxn)
        for phrase-type = (phrase-type orig-cxn)
        when (equal phrase-type type)
        collect orig-cxn))

(defun create-new-th-links (lex-classes-lex-cxns lex-classes-item-based-units type-hierarchy)
  "Creates all TH links for matching lexical cxns using their original lex-class."
  (loop for lex-cxn-lex-class in lex-classes-lex-cxns
        for item-slot-lex-class in lex-classes-item-based-units
        unless (neighbouring-categories-p lex-cxn-lex-class item-slot-lex-class type-hierarchy)
        collect (cons lex-cxn-lex-class item-slot-lex-class)))

(defun create-th-links (problem node)
  "Return the TH links and applied cxns from a comprehend with :th-connected-mode :path-exists instead of :neighbours"
  (let* ((utterance (random-elt (get-data problem :utterances)))
         (gold-standard-meaning (random-elt (get-data problem :meanings)))
         (cxn-inventory (construction-inventory node))
         (orig-cxn-set (original-cxn-set cxn-inventory))
         (type-hierarchy (categorial-network cxn-inventory)))
    (disable-meta-layer-configuration cxn-inventory) ;(fcg::unify-atom
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend utterance :cxn-inventory orig-cxn-set :gold-standard-meaning gold-standard-meaning)))
             (meaning-network (first comprehension-result))
             (cip-node (second comprehension-result))
             (cip (third comprehension-result)))
        (enable-meta-layer-configuration cxn-inventory)

        ;;there is a solution with connected links in the TH
        (when (member 'succeeded (statuses cip-node) :test #'string=)
          (let* ((applied-cxns (applied-constructions cip-node))
                 (lex-cxns (sort-cxns-by-form-string (filter-by-phrase-type 'lexical applied-cxns) utterance))
                 (lex-classes-lex-cxns (when lex-cxns
                                         (map 'list #'lex-class-cxn lex-cxns)))
                 (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn
                                                 (get-all-unit-lex-classes item-based-cxn)))
                 (th-links (when (and lex-classes-lex-cxns lex-classes-item-based-units (= (length lex-classes-lex-cxns) (length lex-classes-item-based-units)))
                             (create-new-th-links lex-classes-lex-cxns lex-classes-item-based-units type-hierarchy))))
            (when (filter-by-phrase-type 'holophrase applied-cxns)
                   (format t "Something has gone horribly wrong!"))
            (list applied-cxns th-links)))))))


#|
(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-cxns-and-th-links) (problem problem) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((cxns (first (restart-data fix)))
           (th-links (second (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th-flat-list nil)
           (th (loop for th-link in th-links
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     (setf th-flat-list (append th-flat-list (list th-link)))
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
           (last-node  (initial-node node))
           (applied-nodes (loop for cxn in cxns
                                do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car last-node))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                                   (direction (cip node))
                                                                                                   :configuration (configuration (construction-inventory node))
                                                                                                   :cxn-inventory (construction-inventory node)))))
                                collect last-node)))
      ;; ignore
      ;; Reset type hierarchy
      (set-categorial-network (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of last new node
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-cxns nil) ;; add cxns that aren't applied here! cube + sphere are learned but only cube applied
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to last new node
      (setf (cxn-supplier last-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses last-node))
      (push 'added-by-repair (statuses last-node))
      ;; enqueue only last new node; never backtrack over the first applied construction, we applied them as a block
      (cip-enqueue last-node (cip node) (get-configuration node :queue-mode)))))
|#

