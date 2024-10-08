(in-package :duckie-language-learning)

;; ------------------
;; + Util functions +
;; ------------------

(defmethod copy-object-content ((src duckie-object) (copy duckie-object))
  ;; for sets of objects, the id does not matter
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (rfid copy) (rfid src)
        (color copy) (color src)
        (zone copy) (zone src)
        ))

(defmethod copy-object-content ((src object-set) (copy object-set))
  ;; for sets of objects, the id does not matter
  (setf (objects copy) (mapcar #'copy-object (objects src))))

(defmethod copy-object-content ((src duckie-car) (copy duckie-car))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (rfid copy) (rfid src)
        (zone copy) (zone src)
        (color copy) (color src)))

(defmethod copy-object-content ((src duckie-building) (copy duckie-building))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (rfid copy) (rfid src)
        (color copy) (color src)
        (zone copy) (zone src)
        (building-function copy) (building-function src)))

(defmethod copy-object-content ((src duckie-home) (copy duckie-home))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (building copy) (building src)
        (rfid copy) (rfid src)
        (color copy) (color src)
        (zone copy) (zone src)
        ))

(defmethod copy-object-content ((src number-category) (copy number-category))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (num copy) (num src)))

(defmethod copy-object-content ((src zone-category) (copy zone-category))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (zone copy) (zone src)))

(defmethod copy-object-content ((src duckie-category) (copy duckie-category))
  (setf (category copy) (category src)))
