(in-package :fcg) 

;;;;;;;;;;;;;;;
;;           ;;
;; Alignment ;;
;;           ;;
;;;;;;;;;;;;;;;


(defgeneric align (solution-node cip mode))
#|
(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (solution-node-after-learning t)
                  (applied-fixes list)
                  (mode (eql :no-alignment)))
  "No alignment")

(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (solution-node-after-learning t)
                  (applied-fixes list)
                  (mode (eql :punish-non-gold-solutions)))
  "Reward successfully used cxns and categorial links, punish those that led to a non-gold solution."
  (declare (ignore solution-node solution-node-after-learning))
  (notify entrenchment-started)
  (multiple-value-bind (gold-solution-nodes non-gold-solution-nodes)
      (loop for node in (succeeded-nodes cip)
            if (gold-standard-solution-p (car-resulting-cfs (cipn-car node))
                                         (get-data (blackboard (construction-inventory node)) :speech-act)
                                         (direction cip)
                                         (configuration (construction-inventory node)))
              collect node into gold-solutions
            else
              collect node into non-gold-solutions
            finally (return (values gold-solutions non-gold-solutions)))
    (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
           (category-mapping (find-data cip :category-mapping))
           (all-fix-cxns (loop for fix in applied-fixes append (when (slot-exists-p fix 'fix-constructions)
                                                                 (fix-constructions fix))))
           (constructions-to-reward (remove-duplicates (loop for node in gold-solution-nodes
                                                             for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                             append (loop for applied-cxn in applied-cxns
                                                                          for equivalent-cxn = (attr-val applied-cxn :equivalent-cxn)
                                                                          if (and equivalent-cxn
                                                                                  ;; if the applied cxn resulted from the same anti-unification repair, it
                                                                                  ;; is new and should not be rewarded.
                                                                                  (not (eq (au-repair-processor (anti-unification-state
                                                                                                                                (attr-val applied-cxn :fix)))
                                                                                           (au-repair-processor (anti-unification-state
                                                                                                                 (attr-val equivalent-cxn :fix))))))
                                                                            collect equivalent-cxn
                                                                          else
                                                                            unless (find (name applied-cxn) all-fix-cxns :key #'name)
                                                                              collect applied-cxn))))
           (constructions-to-punish (remove-duplicates (loop for node in non-gold-solution-nodes
                                                             for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                             append (loop for applied-cxn in applied-cxns
                                                                          unless (find (name applied-cxn) constructions-to-reward :key #'name)
                                                                            collect applied-cxn))))
           (links-to-reward (remove-duplicates (loop for node in gold-solution-nodes
                                                     append (loop for (cat-1 cat-2 link-type) in (used-categorial-links node)
                                                                  for mapped-link = (list (or (cdr (assoc cat-1 category-mapping))
                                                                                              cat-1)
                                                                                          (or (cdr (assoc cat-2 category-mapping))
                                                                                              cat-2)
                                                                                          link-type)
                                                                  when (and (find (first mapped-link) constructions-to-reward
                                                                                  :test #'member
                                                                                  :key #'(lambda (cxn)
                                                                                           (cons-if (attr-val cxn :cxn-cat)
                                                                                                    (attr-val cxn :slot-cats))))
                                                                            (find (second mapped-link) constructions-to-reward
                                                                                  :test #'member
                                                                                  :key #'(lambda (cxn)
                                                                                           (cons-if (attr-val cxn :cxn-cat)
                                                                                                    (attr-val cxn :slot-cats)))))
                                                                    collect mapped-link))
                                               :test #'equal))
           (links-to-punish (remove-duplicates (loop for node in non-gold-solution-nodes
                                                     append (loop for link in (used-categorial-links node)
                                                                  unless (find link links-to-reward :test #'equal)
                                                                  collect link))
                                               :test #'equal))
                                                                        
           (deleted-cxns nil)
           (deleted-categories nil)
           (deleted-links nil)
           (rewarded-links nil)
           (punished-links nil))
      ;; Reward
      (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
            for cxn in constructions-to-reward
            for new-score = (+ reward-rate
                               (* (- 1 reward-rate)
                                  (attr-val cxn :entrenchment-score)))
            do (setf (attr-val cxn :entrenchment-score) new-score))
      (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
            for (cat-1 cat-2 link-type) in links-to-reward
            for new-score = (+ reward-rate
                               (* (- 1 reward-rate)
                                  (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
            do (push (list cat-1 cat-2 new-score) rewarded-links)
               (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type))
      (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
            for cxn in constructions-to-punish
            for new-score = (* (- 1 inhibition-rate)
                               (attr-val cxn :entrenchment-score))
            do (setf (attr-val cxn :entrenchment-score) new-score)
            when (< (attr-val cxn :entrenchment-score) 0.01)
              do (delete-cxn cxn cxn-inventory)
                 (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                 (push cxn deleted-cxns)
                 (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
      (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
            for (cat-1 cat-2 link-type) in links-to-punish
            for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                          (category-exists-p cat-2 cxn-inventory))
            for new-score = (when categories-exist-p
                              (* (- 1 inhibition-rate)
                               (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
            do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
                 (push (list cat-1 cat-2 new-score) punished-links))
            when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
              do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
                 (push (cons cat-1 cat-2) deleted-links))
      
      (notify entrenchment-finished constructions-to-reward rewarded-links constructions-to-punish punished-links
              deleted-cxns deleted-categories deleted-links))))

(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (solution-node-after-learning t)
                  (applied-fixes list)
                  (mode (eql :punish-non-gold-branches)))
  "Reward successfully used cxns and categorial links, punish cxns and links from other branches."
  (declare (ignore solution-node solution-node-after-learning))
  (notify entrenchment-started)      
  (let* ((gold-solution-nodes (loop for node in (succeeded-nodes cip)
                                    if (gold-standard-solution-p (car-resulting-cfs (cipn-car node))
                                                                 (get-data (blackboard (construction-inventory node)) :speech-act)
                                                                 (direction cip)
                                                                 (configuration (construction-inventory node)))
                                      collect node))
         (gold-solution-cxns (remove-duplicates (mapcar #'original-cxn (mappend #'applied-constructions gold-solution-nodes))))
         (cxn-inventory (original-cxn-set (construction-inventory cip)))
         (category-mapping (find-data cip :category-mapping))
         (all-fix-cxns (loop for fix in applied-fixes append (when (slot-exists-p fix 'fix-constructions)
                                                               (fix-constructions fix))))
         (constructions-to-reward (remove-duplicates (loop for node in gold-solution-nodes
                                                           for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                           append (loop for applied-cxn in applied-cxns
                                                                        for equivalent-cxn = (attr-val applied-cxn :equivalent-cxn)
                                                                        if (and equivalent-cxn
                                                                                ;; if the applied cxn resulted from the same anti-unification repair, it
                                                                                ;; is new and should not be rewarded.
                                                                                (not (eq (au-repair-processor (anti-unification-state
                                                                                                               (attr-val applied-cxn :fix)))
                                                                                         (au-repair-processor (anti-unification-state
                                                                                                               (attr-val equivalent-cxn :fix))))))
                                                                          collect equivalent-cxn
                                                                        else
                                                                          unless (find (name applied-cxn) all-fix-cxns :key #'name)
                                                                            collect applied-cxn))))
         (nodes-to-punish (loop for node in (rest (traverse-depth-first (top-node cip) :collect-fn #'identity)) ;; all nodes except initial node
                                for applied-cxn = (original-cxn (first (applied-constructions node)))
                                unless (find applied-cxn gold-solution-cxns :test #'equivalent-form-meaning-mapping)
                                  collect node))
         (constructions-to-punish (remove-duplicates (loop for node in nodes-to-punish
                                                           for applied-cxn = (original-cxn (first (applied-constructions node)))
                                                           collect (or (attr-val applied-cxn :equivalent-cxn) applied-cxn))))
         (links-to-reward-and-all-solution-links  (multiple-value-list (loop with links-to-reward = nil
                                                                             with all-solution-links = nil
                                                                             for node in gold-solution-nodes
                                                                             do (loop for (cat-1 cat-2 link-type) in (used-categorial-links node)
                                                                                      for mapped-link = (list (or (cdr (assoc cat-1 category-mapping))
                                                                                                                  cat-1)
                                                                                                              (or (cdr (assoc cat-2 category-mapping))
                                                                                                                  cat-2)
                                                                                                              link-type)
                                                                                      do (setf all-solution-links (cons mapped-link all-solution-links))
                                                                                      when (and (find (first mapped-link) constructions-to-reward
                                                                                                      :test #'member
                                                                                                      :key #'(lambda (cxn)
                                                                                                               (cons-if (attr-val cxn :cxn-cat)
                                                                                                                        (attr-val cxn :slot-cats))))
                                                                                                (find (second mapped-link) constructions-to-reward
                                                                                                      :test #'member
                                                                                                      :key #'(lambda (cxn)
                                                                                                               (cons-if (attr-val cxn :cxn-cat)
                                                                                                                        (attr-val cxn :slot-cats)))))
                                                                                        do (setf links-to-reward (cons mapped-link links-to-reward)))
                                                                             finally (return (values (remove-duplicates links-to-reward :test #'equal)
                                                                                                     (remove-duplicates all-solution-links :test #'equal))))))
         (links-to-reward (first links-to-reward-and-all-solution-links))
         (all-solution-links (second links-to-reward-and-all-solution-links))
         (links-to-punish (remove-duplicates (loop for node in nodes-to-punish
                                                   append (loop for (cat-1 cat-2 link-type) in (used-categorial-links node)
                                                                for mapped-link = (list (or (cdr (assoc cat-1 category-mapping))
                                                                                            cat-1)
                                                                                        (or (cdr (assoc cat-2 category-mapping))
                                                                                            cat-2)
                                                                                        link-type)
                                                                unless (find mapped-link all-solution-links :test #'equal)
                                                                  collect mapped-link))
                                             :test #'equal))
                                                                        
         (deleted-cxns nil)
         (deleted-categories nil)
         (deleted-links nil)
         (rewarded-links nil)
         (punished-links nil))
    ;; Reward
    (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
          for cxn in constructions-to-reward
          for new-score = (+ reward-rate
                             (* (- 1 reward-rate)
                                (attr-val cxn :entrenchment-score)))
          do (setf (attr-val cxn :entrenchment-score) new-score))
    (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
          for (cat-1 cat-2 link-type) in links-to-reward
          for new-score = (+ reward-rate
                             (* (- 1 reward-rate)
                                (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
          do (push (list cat-1 cat-2 new-score) rewarded-links)
             (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type))
    (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
          for cxn in constructions-to-punish
          for new-score = (* (- 1 inhibition-rate)
                             (attr-val cxn :entrenchment-score))
          do (setf (attr-val cxn :entrenchment-score) new-score)
          when (< (attr-val cxn :entrenchment-score) 0.01)
            do (delete-cxn cxn cxn-inventory)
               (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
               (push cxn deleted-cxns)
               (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
    (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
          for (cat-1 cat-2 link-type) in links-to-punish
          for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                        (category-exists-p cat-2 cxn-inventory))
          for new-score = (when categories-exist-p
                            (* (- 1 inhibition-rate)
                               (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
          do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
               (push (list cat-1 cat-2 new-score) punished-links))
          when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
            do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
               (push (cons cat-1 cat-2) deleted-links))
      
    (notify entrenchment-finished constructions-to-reward rewarded-links constructions-to-punish punished-links
            deleted-cxns deleted-categories deleted-links)))


|#


(defun gold-solutions-and-other-solutions (cip)
  "Returns as first value all gold solution nodes in cip, as second value all non-gold solutions."
  (loop for node in (succeeded-nodes cip)
        if (gold-standard-solution-p (car-resulting-cfs (cipn-car node))
                                     (get-data (blackboard (construction-inventory node)) :speech-act)
                                     (direction cip)
                                     (configuration (construction-inventory node)))
          collect node into gold-solutions
        else
          collect node into non-gold-solutions
        finally (return (values gold-solutions non-gold-solutions))))

(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (mode (eql :punish-other-solutions)))
  "Reward successfully used cxns and categorial links, punish those of other solutions, separately for
solutions that match the gold standard and those that don't."
  ;; Notify start of entrenchment
  (notify entrenchment-started)

  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (gold-solutions-and-other-solutions (multiple-value-list (gold-solutions-and-other-solutions cip)))
         (all-gold-solutions (first gold-solutions-and-other-solutions))
         (all-non-gold-solutions (second gold-solutions-and-other-solutions))
         (constructions-to-reward nil)
         (gold-constructions-to-punish nil)
         (non-gold-constructions-to-punish nil)
         (gold-links-to-punish nil)
         (non-gold-links-to-punish nil)
         (links-to-reward nil)
         (deleted-cxns nil)
         (deleted-categories nil)
         (deleted-links nil)
         (rewarded-links nil)
         (punished-links nil))

    ;; Best solution = gold solution
    (if (find (created-at solution-node) all-gold-solutions :key #'created-at)
      (let ((gold-solutions-except-best-solution (remove (created-at solution-node) all-gold-solutions :key #'created-at)))
        ;; Rewarding and punishment of constructions
        (setf constructions-to-reward (remove-duplicates (mapcar #'original-cxn (applied-constructions solution-node))))
        (setf gold-constructions-to-punish (remove-duplicates (loop for node in gold-solutions-except-best-solution
                                                                    for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                                    append (loop for applied-cxn in applied-cxns
                                                                                 unless (find (name applied-cxn) constructions-to-reward :key #'name)
                                                                                   collect applied-cxn))))
        (setf non-gold-constructions-to-punish (remove-duplicates (loop for node in all-non-gold-solutions
                                                                        for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                                        append (loop for applied-cxn in applied-cxns
                                                                                     unless (find (name applied-cxn) constructions-to-reward :key #'name)
                                                                                       collect applied-cxn))))
        ;; Rewarding and punishment of links
        (setf links-to-reward (remove-duplicates (used-categorial-links solution-node) :test #'equal))
        (setf gold-links-to-punish (remove-duplicates (loop for node in gold-solutions-except-best-solution
                                                            append (loop for link in (used-categorial-links node)
                                                                         unless (find link links-to-reward :test #'equal)
                                                                           collect link))
                                                      :test #'equal))
        (setf non-gold-links-to-punish (remove-duplicates (loop for node in all-non-gold-solutions
                                                                append (loop for link in (used-categorial-links node)
                                                                             unless (find link links-to-reward :test #'equal)
                                                                               collect link))
                                                          :test #'equal))
        
        ;; Reward
        (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
              for cxn in constructions-to-reward
              for new-score = (+ reward-rate
                                 (* (- 1 reward-rate)
                                    (attr-val cxn :entrenchment-score)))
              do (setf (attr-val cxn :entrenchment-score) new-score))
        (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
              for (cat-1 cat-2 link-type) in links-to-reward
              for new-score = (+ reward-rate
                                 (* (- 1 reward-rate)
                                    (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
              do (push (list cat-1 cat-2 new-score) rewarded-links)
                 (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type))
        ;; Punish other gold
        (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
              for cxn in gold-constructions-to-punish
              for new-score = (* (- 1 inhibition-rate)
                                 (attr-val cxn :entrenchment-score))
              do (setf (attr-val cxn :entrenchment-score) new-score)
              when (< (attr-val cxn :entrenchment-score) 0.01)
                do (delete-cxn cxn cxn-inventory)
                   (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                   (push cxn deleted-cxns)
                   (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
        (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
              for (cat-1 cat-2 link-type) in gold-links-to-punish
              for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                            (category-exists-p cat-2 cxn-inventory))
              for new-score = (when categories-exist-p
                                (* (- 1 inhibition-rate)
                                   (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
              do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
                   (push (list cat-1 cat-2 new-score) punished-links))
              when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
                do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
                   (push (cons cat-1 cat-2) deleted-links))
        ;;; Punish non-gold
        (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
              for cxn in non-gold-constructions-to-punish
              for new-score = (* (- 1 inhibition-rate)
                                 (attr-val cxn :entrenchment-score))
              do (setf (attr-val cxn :entrenchment-score) new-score)
              when (< (attr-val cxn :entrenchment-score) 0.01)
                do (delete-cxn cxn cxn-inventory)
                   (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                   (push cxn deleted-cxns)
                   (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
        (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
              for (cat-1 cat-2 link-type) in non-gold-links-to-punish
              for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                            (category-exists-p cat-2 cxn-inventory))
              for new-score = (when categories-exist-p
                                (* (- 1 inhibition-rate)
                                   (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
              do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
                   (push (list cat-1 cat-2 new-score) punished-links))
              when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
                do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
                   (push (cons cat-1 cat-2) deleted-links)))

        ;; Best solution is not a gold solution (elsewhere)
        ;; Punished used constructions and links
        (progn
          (setf non-gold-constructions-to-punish (remove-duplicates (mapcar #'original-cxn (applied-constructions solution-node))))
          (setf non-gold-links-to-punish (remove-duplicates (used-categorial-links solution-node) :test #'equal))
 
          (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
                for cxn in non-gold-constructions-to-punish
                for new-score = (* (- 1 inhibition-rate)
                                   (attr-val cxn :entrenchment-score))
                do (setf (attr-val cxn :entrenchment-score) new-score)
                when (< (attr-val cxn :entrenchment-score) 0.01)
                  do (delete-cxn cxn cxn-inventory)
                     (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                     (push cxn deleted-cxns)
                     (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
          (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
                for (cat-1 cat-2 link-type) in non-gold-links-to-punish
                for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                              (category-exists-p cat-2 cxn-inventory)
                                              (link-exists-p cat-1 cat-2 cxn-inventory))
                for new-score = (when categories-exist-p
                                  (* (- 1 inhibition-rate)
                                     (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
                do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
                     (push (list cat-1 cat-2 new-score) punished-links))
                when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
                  do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
                     (push (cons cat-1 cat-2) deleted-links))))

    ;; Remove orphan constructions
    (loop for cxn in (constructions-list cxn-inventory)
          do (when (and (eql (type-of cxn) 'filler-cxn)
                        (not (neighbouring-categories (attr-val cxn :cxn-cat) cxn-inventory)))
               (delete-cxn cxn cxn-inventory)
               (remove-category (attr-val cxn :cxn-cat) cxn-inventory :recompute-transitive-closure nil)
               (push cxn deleted-cxns)
               (setf deleted-categories (append (list (attr-val cxn :cxn-cat)) deleted-categories)))
             (when (and (eql (type-of cxn) 'linking-cxn)
                        (or (not (neighbouring-categories (first (attr-val cxn :slot-cats)) cxn-inventory))
                            (not (neighbouring-categories (second (attr-val cxn :slot-cats)) cxn-inventory))
                            (and (attr-val cxn :cxn-cat)
                                 (not (neighbouring-categories (attr-val cxn :cxn-cat) cxn-inventory)))))
               (delete-cxn cxn cxn-inventory)
               (remove-categories (cons (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory :recompute-transitive-closure nil)
               (push cxn deleted-cxns)
               (setf deleted-categories (append (cons (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories))))

    (notify entrenchment-finished constructions-to-reward rewarded-links
            (append gold-constructions-to-punish non-gold-constructions-to-punish)
            punished-links deleted-cxns deleted-categories deleted-links)))

    #|
    (loop for cxn in (remove-if #'(lambda (cxn) (eq 'linking-cxn (type-of cxn))) constructions-to-reward)
          for equivalent-cxns = (find-all cxn (constructions-list cxn-inventory) :test #'other-cxn-w-same-form-and-meaning-p)
          do (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
                   for cxn in equivalent-cxns
                   for new-score = (* (- 1 inhibition-rate)
                                      (attr-val cxn :entrenchment-score))
                   do (setf (attr-val cxn :entrenchment-score) new-score)
                   when (< (attr-val cxn :entrenchment-score) 0.01)
                     do (delete-cxn cxn cxn-inventory)
                        (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                        (push cxn deleted-cxns)
                        (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories))))
          |#

 