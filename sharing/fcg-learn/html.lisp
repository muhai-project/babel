(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; HTML en CSS for web-interface ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixes ;;
;;;;;;;;;;;

(defmethod make-html ((fix holophrastic-fix)
                      &key expand-initially)
  "Visualisation for holophrastic fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "fix-constructions:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cxn in (fix-constructions fix)
                      collect (make-html cxn
                                         :cxn-inventory (cxn-inventory cxn)
                                         :expand-initially expand-initially)))))))
        :expand-initially expand-initially))))


(defmethod make-html ((fix anti-unification-fix)
                      &key expand-initially)
  "Visualisation for anti-unification fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "anti-unification state:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (anti-unification-state fix)))))
           ,@(when (base-cxns fix)
               `(((tr) ((td :class "data-field") "base-cxns:")
                  ((td :class "data")
                   ((td :class "data-field")
                    ,@(loop for cxn in (base-cxns fix)
                            collect (make-html cxn
                                               :cxn-inventory (cxn-inventory cxn)
                                               :expand-initially expand-initially)))))))
           ((tr) ((td :class "data-field") "fix-constructions:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cxn in (fix-constructions fix)
                      collect (make-html cxn
                                         :cxn-inventory (cxn-inventory cxn)
                                         :expand-initially expand-initially)))))
           ((tr) ((td :class "data-field") "fix-categories:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cat in (fix-categories fix)
                      collect (make-html cat)))))
           ((tr) ((td :class "data-field") "fix-links:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for link in (fix-categorial-links fix)
                      collect (make-html link)))))))
        :expand-initially expand-initially))))


(defmethod make-html ((fix categorial-link-fix)
                      &key expand-initially)
  "Visualisation for categorial link fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "fix-links:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for link in (fix-categorial-links fix)
                      collect (make-html link)))))))
        :expand-initially expand-initially))))


(defmethod make-html-construction-title ((construction fcg-learn-cxn))
  "Print title of construction and node with name and entrenchment score."
 `((span) 
   ,(format nil "~(~a~) (~$)" (name construction) (attr-val construction :entrenchment-score))))




;; AU repair states ;;
;;;;;;;;;;;;;;;;;;;;;;


(defmethod make-html ((state au-repair-state)
		      &key 
		      (tree-id (make-id 'au-repair-state))
                      expand-initially)
  ""
  (let* ((node-id-0 (make-id 'au-repair-state))
	 (node-color (if (find state (succeeded-states (au-repair-processor state)))
                       "#1e81b0"
                       "#e28743"))
         (node-id (symb tree-id '- (created-at state)))
         ;;function for the title
         (title-div
          (lambda (bol_color link-parameters)
            `((div :class "cipn-title" 
                   :style ,(mkstr "background-color:" (if bol_color "#A8930D;" node-color ) ";"))
              ((a ,@link-parameters)
               ,(make-html-au-repair-state-title state)))))
         ;;
         (outer-div-with-menu
          (lambda (div-children)
            `((div :class "cipn" :id ,(mkstr node-id))
              ,(make-div-with-menu 
                :div-attributes `(:style ,(mkstr "border:1px solid " node-color))
                :div-children div-children))))
         ;;generates the different versions of the node
         (div
          (lambda ()
            (make-expandable/collapsable-element 
             node-id-0 (make-id)
             ;; the unexpanded version
             (lambda ()
               (funcall 
                outer-div-with-menu
                (list 
                 (funcall 
                  title-div nil
                  (make-expand/collapse-link-parameters node-id-0 t)))))
             
             (lambda ()
               (funcall 
                outer-div-with-menu 
                (list 
                 (funcall 
                  title-div nil
                  (make-expand/collapse-link-parameters node-id-0 nil))
                 `((table)
                   ((tbody)
                    ((tr) 
                       ((td :style ,(mkstr "border-top:1px solid "
                                           node-color))
                        
                         ,@(when (new-cxns state)
                             `(((tr) ((td :class "data-field") "Fix cxns:")
                                ((td :class "data")
                                 ,@(loop for cxn in (new-cxns state)
                                         collect (make-html cxn 
                                                            :cxn-inventory (cxn-inventory cxn)
                                                            :expand-initially expand-initially))))))
                         ,@(when (base-cxn state)
                             `(((tr) ((td :class "data-field") "Base cxn:")
                                ((td :class "data")
                                 ,(make-html (base-cxn state)
                                             :cxn-inventory (cxn-inventory (base-cxn state))
                                             :expand-initially expand-initially)))))
                         ((table :class "two-col")
                         ((tr) ((td :class "data-field") "Fix cxn-inventory:")
                                ((td :class "data")
                                 ,(make-html (fix-cxn-inventory state))))
                         ,@(when (remaining-form-speech-act state)
                             `(((tr) ((td :class "data-field") "Remaining form:")
                                ((td :class "data")
                                 ,(predicate-network->svg (remaining-form-speech-act state))))))
                         ,@(when (remaining-meaning-speech-act state)
                             `(((tr) ((td :class "data-field") "Remaining meaning:")
                                ((td :class "data")
                                 ,(predicate-network->svg (remaining-meaning-speech-act state))))))))))))))))) 
         (tree
          (lambda ()
            (draw-node-with-children 
             (funcall div)
             (reverse 
              (loop for child in (children state)
                    collect (make-html 
                             child 
                             :tree-id tree-id )))
             :line-width "1px" :style "solid" :color "#888" :width "10px"))))
    
    (store-wi-object state node-id)
    
    (funcall tree)))

;; (add-element (make-html *test-state*))



(defgeneric make-html-au-repair-state-title (au-repair-state))

(defmethod make-html-au-repair-state-title ((state au-repair-state))
  ""
  `((span)
     ,(cond ((base-cxn state)
             `((span) "learning from "
               ((i) ,(format nil "~(~a~) (cf: ~a cm: ~a)"
                             (name (base-cxn state))
                             (cost (au-result-form state))
                             (cost (au-result-meaning state))))))
            ((eql (type-of (first (new-cxns state))) 'holophrastic-cxn)
             "new holophrastic cxn")
            ((eql (type-of (first (new-cxns state))) 'filler-cxn)
             "new filler cxn")
            (t
             "initial"))))
