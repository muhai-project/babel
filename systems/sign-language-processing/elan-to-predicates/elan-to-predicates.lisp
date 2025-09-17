(in-package :slp)

(defun elan->predicates (annotation-document-xmls)
  "transforms annotation-document-xmls to a set of predicates that represent signs using hamnosys-strings"
  (let* ((time-points
          (retrieve-time-points annotation-document-xmls))
         (dominant-intervals '())
         (non-dominant-intervals '())
         (alignments '()))
    
    ;; reset the id-counters of fcg to avoid large numerical tails after a few iterations
    (reset-id-counters)

    ;; loop over all child-tags of the annotation-document-xmls
    (loop for child in (xmls:node-children annotation-document-xmls)
          ;; if the child-tag has a tier-id, we find it
          for tier-id =
            ;; loop over all attribes of the child-tag
            (loop for attribute in (xmls:node-attrs child)
                  ;; if the attribute is named TIER_ID, stop the loop and return value of attribute
                  when (string=
                        (first attribute)
                        "TIER_ID")
                    do (return
                        (read-from-string
                         (upcase
                          (replace-spaces
                           (second attribute))))))
          
          ;; check whether the extracted tier-id is one we are interested in (a tier with glosses or hamnosys)
          when (member tier-id
                       (list 'LH-GLOSSES
                             'RH-GLOSSES
                             'LH-HAMNOSYS
                             'RH-HAMNOSYS))
            do (cond
                
                ;;--------------------------------------------------------;;
                ;; Condition A: the tier is the dominant hand gloss tier  ;;
                ;;--------------------------------------------------------;;
                ;; if the tier is the dominant hand gloss tier, we use it to extract intervals
                ;; we set dominant-intervals to the list of extracted intervals
                ((eql tier-id 'RH-GLOSSES)
                 (setf dominant-intervals
                       (retrieve-intervals
                        child
                        time-points
                        :type 'right-hand-articulation)))

                ;;------------------------------------------------------------;;
                ;; Condition B: the tier is the non-dominant hand gloss tier  ;;
                ;;------------------------------------------------------------;;
                ;; if the tier is the non-dominant hand gloss tier, we use it to extract intervals
                ;; we set non-dominant-intervals to the list of extracted intervals
                ((eql tier-id 'LH-GLOSSES)
                 (setf non-dominant-intervals
                       (retrieve-intervals
                        child
                        time-points
                        :type 'left-hand-articulation)))

                ;;------------------------------------------------------;;
                ;; Condition C: the tier is the dominant hamnosys tier  ;;
                ;;------------------------------------------------------;;
                ;; if the tier is the dominant hamnosys tier, we use add-hamnosys to add the
                ;; hamnosys-representations on the tier to their corresponding dominant-intervals
                ((eql tier-id 'RH-HAMNOSYS)
                 (add-hamnosys
                  child
                  dominant-intervals))
                
                ;;----------------------------------------------------------;;
                ;; Condition D: the tier is the non-dominant hamnosys tier  ;;
                ;;----------------------------------------------------------;;
                ;; if the tier is the non-dominant hamnosys tier, we use add-hamnosys to add the
                ;; hamnosys-representations on the tier to their corresponding non-dominant-intervals
                ((eql tier-id 'LH-HAMNOSYS)
                 (add-hamnosys
                  child
                  non-dominant-intervals))))

    ;; create the alignment predicates from the intervals
    (setf
     alignments
     (append
      (make-adjacent-predicates
       dominant-intervals)
      (make-alignment-predicates
       dominant-intervals
       non-dominant-intervals)))

    ;; return a list of all hamnosys and alignment predicates
    (make-instance
     'signed-form-predicates
     :predicates
     (append
      (make-hamnosys-predicates dominant-intervals)
      (make-hamnosys-predicates non-dominant-intervals)
      alignments))))
  
