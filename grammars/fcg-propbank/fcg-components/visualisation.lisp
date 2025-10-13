(in-package :fcg-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Extracting and visualising frame representations. ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; Representation ;;
;;;;;;;;;;;;;;;;;;;;

;; Frame-set ;;
;;;;;;;;;;;;;;;

(defclass frame-set (irl:entity)
  ((frames :type list
           :accessor frames
           :initarg :frames
           :initform nil
           :documentation "The list of frames."))
  (:documentation "Class for representing a set of frames."))

(defmethod print-object ((frame-set frame-set) (stream t))
  "Printing a frame-set object."
  (format stream "<frame-set: {~{~a~^, ~}}>" (frames frame-set )))


;; Frame ;;
;;;;;;;;;;;

(defclass frame (irl:entity)
  ((frame-name :type symbol
               :accessor frame-name
               :initarg :frame-name
               :initform nil
               :documentation "The name of the frame.")
   (frame-evoking-element :type frame-evoking-element
                          :accessor frame-evoking-element
                          :initarg :frame-evoking-element
                          :initform nil
                          :documentation "The frame evoking element object.")
   (frame-elements :type list
                   :accessor frame-elements
                   :initarg :frame-elements
                   :initform nil
                   :documentation "A list of frame element objects."))
  (:documentation "Class for representing frames."))

(defmethod print-object ((frame frame) (stream t))
  "Printing a frame object."
  (format stream "<frame: ~a>" (frame-name frame)))

;; Frame evoking element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass frame-evoking-element ()
  ((fel-string :type string
               :accessor fel-string
               :initarg :fel-string
               :initform nil
               :documentation "The string of the frame-evoking element.")
   (indices :accessor indices
            :initarg :indices
            :initform nil
            :documentation "A list of indices.")
   (lemma :type symbol
          :accessor lemma
          :initarg :lemma
          :initform nil
          :documentation "The lemma of the frame-evoking-element."))
  (:documentation "Class for representing frames."))


(defmethod print-object ((frame-evoking-element frame-evoking-element) (stream t))
  "Printing a frame-evoking-element object."
  (format stream "<frame-evoking-element: ~a>" (fel-string frame-evoking-element)))

;; Frame element ;;
;;;;;;;;;;;;;;;;;;;

(defclass frame-element ()
  ((fe-name :type symbol
                       :accessor fe-name
                       :initarg :fe-name
                       :initform nil
                       :documentation "The name of the frame element (e.g. cognizer).")
   (fe-role :type symbol
               :accessor fe-role
               :initarg :fe-role
               :initform nil
               :documentation "The role name of the frame element (e.g. arg0).")
   (fe-string :type string
              :accessor fe-string
              :initarg :fe-string
              :initform nil
              :documentation "The string.")
   (indices :accessor indices
            :initarg :indices
            :initform nil
            :documentation "A list of indices."))
  (:documentation "Class for representing frame elements."))

(defmethod print-object ((frame-element frame-element) (stream t))
  "Printing a frame-element object."
  (format stream "<frame-element: ~a>" (fe-name frame-element)))


;;;;;;;;;;;;;;;;;;;;
;; HTML           ;;
;;;;;;;;;;;;;;;;;;;;

(defmethod irl:make-html-for-entity-details ((s frame-set) &key)
  "Visualising a frame-set."
  `(((div :class "entity-detail") 
     ,@(loop for f in (frames s)
             collect (make-html f :expand-initially t)))))

(defmethod irl:make-html-for-entity-details ((frame frame) &key)
  "Visualising a frame."
  `(((div :class "entity-detail")
    ,(format nil "FEE: ~s" (fel-string (frame-evoking-element frame))))
    ,@(loop for fe in (frame-elements frame)
            collect
            `((div :class "entity-detail") ,(format nil "~@(~a~): ~s" (fe-name fe) (fe-string fe))))))

(defmethod make-html ((e frame-set)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  "Visualising a frame."
  `((div :class "entity")
    ,(let ((element-id (make-id (irl:id e))))
          (wi:make-expandable/collapsable-element
           element-id expand/collapse-all-id
           `((div :class "entity-box")
             ((div :class "entity-title")
              ((a ,@(wi:make-expand/collapse-link-parameters 
                     element-id t "expand entity")
                  :name ,(mkstr (irl:id e)))
               ,(format nil "Frame set"))))
           (lambda ()
             `((div :class "entity-box")
               ((div :class "entity-title")
                ((a ,@(wi:make-expand/collapse-link-parameters element-id nil 
                                                            "collapse entity")
                    :name ,(mkstr (irl:id e)))
                 ,(format nil "Frame set")))
               ((table :class "entity" :cellpadding "0" :cellspacing "0") 
                ((tr)
                 ((td :class "entity-details")
                  ,@(apply 'irl:make-html-for-entity-details e parameters))))))
           :expand-initially expand-initially))))


(defmethod make-html ((e frame)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  "Visualising a frame set."
  `((div :class "entity")
    ,(let ((element-id (make-id (irl:id e))))
          (wi:make-expandable/collapsable-element 
           element-id expand/collapse-all-id
           `((div :class "entity-box")
             ((div :class "entity-title")
              ((a ,@(wi:make-expand/collapse-link-parameters 
                     element-id t "expand entity")
                  :name ,(mkstr (irl:id e)))
               ,(format nil "~(~a~)" (frame-name e)))))
           (lambda ()
             `((div :class "entity-box")
               ((div :class "entity-title")
                ((a ,@(wi:make-expand/collapse-link-parameters element-id nil 
                                                            "collapse entity")
                    :name ,(mkstr (irl:id e)))
                 ,(format nil "~(~a~)" (frame-name e))))
               ((table :class "entity" :cellpadding "0" :cellspacing "0") 
                ((tr)
                 ((td :class "entity-details")
                  ,@(apply 'irl:make-html-for-entity-details e parameters))))))
           :expand-initially expand-initially))))

