(in-package :slp)

(defun dominant-hand-tier? (tier-name)
  "returns t if tier-name refers to a tier
   that contains information about the dominant hand"
  (let ((tier-hand
         (read-from-string
          (upcase
           (first
            (split-sequence::split-sequence
             #\-
             (string tier-name)))))))
    (eql *dominant-hand* tier-hand)))

(defun type-from-tier-name (tier-name)
  "returns the type of information described by the
   tier to which tier-name refers"
  (read-from-string
   (upcase
    (first
     (last
      (split-sequence::split-sequence
       #\-
       (string tier-name)))))))



(defun find-by-elan-id (elan-intervals elan-id)
  "find the interval in elan-intervals that has the specified elan-id"
  (loop for interval in elan-intervals
        when (eql (elan-id interval) elan-id)
          do (return interval)))

(defun measure-overlap (interval-1 interval-2)
  "measures whether two interval-1 and interval-2 overlap"
  (max
   0
   (-
    (min
     (cdr (end interval-1))
     (cdr (end interval-2)))
    (max
     (cdr (begin interval-1))
     (cdr (begin interval-2))))))

(defun equal-relaxed (time-1 time-2 error-margin)
  "compares whether time-1 and time-2 are the same using a error-margin to define"
  (<=
   (max
    (- time-1 time-2)
    (- time-2 time-1))
   error-margin))


(defun find-preceding-interval (target-interval reference-intervals)
  "finds the interval in reference-intervals that directly precedes the target-interval"
  (loop for reference-interval in reference-intervals
        when (equal-relaxed
              (cdr (end reference-interval))
              (cdr (begin target-interval))
              100)
          do (return reference-interval)))

(defun find-following-interval (target-interval reference-intervals)
  "finds the interval in reference-intervals that directly follows the target-interval"
  (loop for reference-interval in reference-intervals
        when (equal-relaxed
              (cdr (begin reference-interval))
              (cdr (end target-interval))
              100)
          do (return reference-interval)))

(defun find-tier-by-tier-id (annotation-document-xmls &key (target-tier-id 'english-translation))
  "finds the tier in annotation-document-xmls that has target-tier-id as its tier-id and returns the first annotation on this tier. Is usedto extract a global annotation that spans the whole time of the video."
  (loop for child in (xmls:node-children annotation-document-xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when
                              (string=
                               (first attribute)
                               "TIER_ID")
                              do
                                (return
                                 (read-from-string
                                  (downcase
                                   (replace-spaces
                                    (second attribute))))))
        when (eql tier-id target-tier-id)
          do (return
               (first
                (xmls:node-children
                 (first
                  (xmls:node-children
                   (first
                    (xmls:node-children
                     (first
                      (xmls:node-children child)))))))))))



