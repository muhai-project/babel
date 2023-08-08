(in-package :cle)

;; -----------
;; + Concept +
;; -----------

(defclass concept ()
  ((id
    :initarg :id :accessor id :initform (make-id "CONCEPT") :type symbol
    :documentation "Id of the concept."))
  (:documentation "Abstract class for concepts."))

(defclass concept-distribution (concept)
  ((prototypes
    :initarg :prototypes :accessor prototypes :initform nil :type list))
  (:documentation "Concept representation using prototypes."))
 
(defmethod make-concept ((agent cle-agent) (object cle-object) (mode (eql :distribution)))
  (let ((prototypes (loop for channel being the hash-keys of (attributes object) using (hash-value observation)
                          for initial-weight = (get-configuration agent :initial-weight)
                          for distribution = (make-distribution agent
                                                                observation
                                                                (get-configuration agent :distribution))
                          for new-prototype = (make-instance 'prototype
                                                             :channel channel
                                                             :weight initial-weight
                                                             :weight-mode (get-configuration agent :weight-update-strategy)
                                                             :distribution distribution)
                          ;; only create prototypes for working sensors
                          if (not (find channel (disabled-channels agent)))
                            collect new-prototype)))
    ;; create the concept
    (make-instance 'concept-distribution :prototypes prototypes)))

;; ----------------
;; + get-channels +
;; ----------------
(defmethod get-available-prototypes ((agent cle-agent) (concept concept))
  (loop with disabled-channels = (disabled-channels agent)
        for prototype in (prototypes concept)
        if (not (find (channel prototype) disabled-channels))
          collect prototype))

(defmethod switch-channel-availability ((agent cle-agent) (channel symbol))
  (if (find channel (disabled-channels agent))
    (setf (disabled-channels agent) (remove channel (disabled-channels agent)))
    (setf (disabled-channels agent) (cons channel (disabled-channels agent)))))

(defmethod copy-object ((concept concept-distribution))
  (make-instance 'concept-distribution
                 :id (id concept)
                 :prototypes (copy-object (prototypes concept))))
