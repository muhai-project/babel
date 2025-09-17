(in-package :slp)

;;---------------------------;;
;; Functions for making HTML ;;
;;---------------------------;;

(defparameter *avatar-counter* -1)

(defun make-empty-cell ()
  "makes and returns the html for an empty cell in the table"
  `((td :class "empty" :style "width: 20px;")))

(defun clean-file-name (filename)
  "clean a filename so that it conforms to norms of filenames"
  (let ((output filename))
    (setf output (remove #\\ output))
    (setf output (remove #\/ output))
    (setf output (remove #\: output))
    (setf output (remove #\* output))
    (setf output (remove #\? output))
    (setf output (remove #\" output))
    (setf output (remove #\< output))
    (setf output (remove #\> output))
    (setf output (remove #\| output))
    output))

(defun find-lh-cell-by-column-nr (column-nr lh-cells)
  "Finds the lh-cell occurring at the column-nr by looking at its start and end points.
   If no such a cell exists, the function does not return anything"
  (loop for lh-cell in lh-cells
        for start-column-nr = (start-column-nr lh-cell)
        for end-column-nr = (end-column-nr lh-cell)
        when (or
              (eql start-column-nr column-nr)
              (eql end-column-nr column-nr)
              (and (> column-nr start-column-nr)
                   (< column-nr end-column-nr)))
          do (return lh-cell)))


(defun make-id-gloss-cell (span-id fcg-tag colspan hamnosys &key (avatar t))
  "makes and returns the html for an id-gloss-cell with the given span-id,
   fcg-tag and colspan. The cell contains a play button, sending the sigml
   of the provided hamnosys to the avatar"
  (let* ((clean-fcg-tag (clean-file-name fcg-tag))
         (sigml (hamnosys->sigml hamnosys))
         (filename (babel-pathname :directory '(".tmp" "sigml-files")
                                   :name (format nil "~a" clean-fcg-tag)
                                   :type "sigml"))
         (url (format nil "http://localhost:8000/~a.sigml" clean-fcg-tag))
         (uri (format nil "/~a.sigml" clean-fcg-tag)))
    (ensure-directories-exist filename)
    (with-open-file
        (out-stream
         filename
         :direction :output
         :if-exists :supersede
         :if-does-not-exist :create
         :external-format :utf-8
         :element-type 'cl:character)
      (xmls:write-xml
       sigml
       out-stream
       :indent t))
    (setf web-interface::*dispatch-table*
          (append web-interface::*dispatch-table*
                  (list (web-interface::create-static-file-dispatcher-and-handler   
                         uri filename))))
  `((td
     :class "id-gloss-cell"
     :colspan
     ,(write-to-string colspan))
    ((span :style "display:inline-block")
     ((span
       :id ,(string span-id)
       :onclick
       ,(format
         nil
         "highlightSymbol('~a','~a','~a');"
         (string-upcase fcg-tag)
         span-id
         (web-interface::get-color-for-symbol (string-upcase fcg-tag))))
      ((p
        :class "articulation-tag"
        :name ,fcg-tag)
       ,fcg-tag)))
    ,(if avatar
       `((button :type "button"
             :class "playsigml"
             :onclick
             ,(format
               nil
               "CWASA.playSiGMLURL(`~a`, ~a)" url *avatar-counter*)) "&#9658;" )
       `((p)"")))))

(defun make-hamnosys-cell (value colspan)
   "makes and returns the html for a hamnosys-cell
    with the given value and colspan"
  `((td
     :class "hamnosys-cell"
     :colspan
     ,(write-to-string colspan))
     ((p)
      ,value)))

(defun make-right-hand-rows (right-hand-cells number-of-columns &key (avatar t))
  "makes and returns html for both the id-gloss and hamnosys-rowsof the right hand"
  (loop with right-hand-id-gloss-row
          = `((TR :CLASS "header" :style "width:100%;")
              ((TD :CLASS "header")
               "right hand"))
        with right-hand-hamnosys-row
          = `((TR :CLASS "header" :style "width:100%;")
              ((TD :CLASS "header")))
        for x from 0 to number-of-columns
        for right-hand-cell
          = (find-rh-cell-by-column-nr
             right-hand-cells
             x)
        for fcg-tag
          = (downcase
             (string
              (fcg-tag
               right-hand-cell)))
        for value
          = (string
             (hamnosys
              right-hand-cell))
        for span-id
          = (make-const "S")
        do
          (if (string=
               (utils::remove-numeric-tail
                fcg-tag)
               "dummy")
            (progn
              (pushend
               (make-empty-cell)
               right-hand-id-gloss-row)
              (pushend
               (make-empty-cell)
               right-hand-hamnosys-row))
            (progn
              (pushend
               (make-id-gloss-cell
                span-id
                fcg-tag
                1
                value
                :avatar avatar)
               right-hand-id-gloss-row)
              (pushend
               (make-hamnosys-cell
                value
                1)
               right-hand-hamnosys-row)))
        finally
          (return
           (list
            right-hand-id-gloss-row
            right-hand-hamnosys-row))))

(defun make-left-hand-rows (left-hand-cells number-of-columns &key (avatar t))
  "makes and returns html for both the id-gloss and hamnosys-rows of the left hand"
  (loop with left-hand-id-gloss-row
          = `((tr :class "header" :style "width:100%;")
              ((td :class "header")
               "Left hand"))
        with left-hand-hamnosys-row
          = `((tr :class "header" :style "width:100%;")
              ((td :class "header")))
        with colwait = nil
        for x from 0 to number-of-columns
        for left-hand-cell
          = (find-lh-cell-by-start-column-nr
             left-hand-cells
             x)
        for fcg-tag
          = (when left-hand-cell
              (downcase
               (string
                (fcg-tag
                 left-hand-cell))))
        for value
          = (when left-hand-cell
              (string
               (hamnosys
                left-hand-cell)))
        for sigml-source
          = (when left-hand-cell
              (if (or (eql (char value 0)
                         #\)
                        (eql (char value 0)
                         #\))
              value
              (concatenate 'string "" value)))
        for span-id = (make-const "S")
        for start = x
        for end
          = (when left-hand-cell
              (end-column-nr left-hand-cell))
        for col-dif
          = (when end
              (- end start))
        for colspan
          = (when col-dif
              (if (eql col-dif 0)
                nil
                (+ col-dif 1)))
        do 
          (unless colwait
            (if left-hand-cell
              (progn
                (pushend
                 (make-id-gloss-cell
                  span-id
                  fcg-tag
                  colspan
                  sigml-source
                  :avatar avatar)
                 left-hand-id-gloss-row)
                (pushend
                 (make-hamnosys-cell
                  value
                  colspan)
                 left-hand-hamnosys-row))
              (progn
                (pushend
                 (make-empty-cell)
                 left-hand-id-gloss-row)
                (pushend
                 (make-empty-cell)
                 left-hand-hamnosys-row))))
          (when colspan
            (setf colwait colspan))
          (when colwait
            (decf colwait))
          (when (eql colwait 0)
            (setf colwait nil))
        finally
          (return
              (list
               left-hand-id-gloss-row
               left-hand-hamnosys-row))))

(defun make-time-header (number-of-columns)
  "makes and returns the html for the header row of the table (time)"
  `((tr :class "header" :style "width:100%;")
    ((td)
     ((span)
      ((span)
       ((p :class "header-text")
        "time&#x2192;"))))
    ,@(loop for x from 0 to number-of-columns
            collect 
              `((td
                 :class "empty")))))
  

(defmethod make-html ((signed-form-predicates signed-form-predicates)
                      &key (avatar t) &allow-other-keys)
  "transforms a list of sign-predicates into a html object that can be added to the
   webinterface"
  (incf *avatar-counter*)
  (let* ((sign-table
          (make-sign-table (predicates signed-form-predicates)))
         (number-of-columns
          (determine-number-of-columns sign-table))
         (right-hand-rows
          (make-right-hand-rows
           (right-hand-cells sign-table)
           number-of-columns
           :avatar avatar))
         (left-hand-rows
          (make-left-hand-rows
           (left-hand-cells sign-table)
           number-of-columns
           :avatar avatar))
         (sigml
          (hamnosys-list->sigml
           (sign-table-to-hamnosys-string
            sign-table)))
         (utterance-tag (make-const "utterance"))
         (filename (babel-pathname :directory '(".tmp" "sigml-files")
                                   :name (format nil "~a" utterance-tag)
                                   :type "sigml"))
         (url (format nil "http://localhost:8000/~a.sigml" utterance-tag))
         (uri (format nil "/~a.sigml" utterance-tag)))
    (ensure-directories-exist filename)
    (with-open-file
        (out-stream
         filename
         :direction :output
         :if-exists :supersede
         :if-does-not-exist :create
         :external-format :utf-8
         :element-type 'cl:character)
      (format out-stream "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
      (xmls:write-xml
       sigml
       out-stream
       :indent t))
    (setf web-interface::*dispatch-table*
          (append web-interface::*dispatch-table*
                  (list (web-interface::create-static-file-dispatcher-and-handler   
                         uri filename))))
    
    `((div :style "margin-top: 20px; margin-bottom: 20px; margin-left: 10px; width: 100%;")
      ,(if avatar
         `((div :style "width:20%; height: 30%; margin-bottom: 10px; margin-top: 20px; display: inline-block;")
           ((div :style "background: #DEEFE7;"
                 :class ,(format nil "CWASAAvatar av~a" *avatar-counter*)
                 :align "center"
                 :onclick "CWASA.init({ambIdle:false,useClientConfig:false});"))
           ((button :type "button"
                    :onclick ,(format
                               nil
                               "CWASA.playSiGMLURL(`~a`, ~a)" url *avatar-counter*)) "animate utterance"))
         `((p)""))
      ((table :class "sign-table" :style "margin-bottom:20px; display: inline-block;")
       ((tbody)
        ((tr)
        ,(first right-hand-rows)
        ,(second right-hand-rows)
        ,(first left-hand-rows)
        ,(second left-hand-rows)))))))
  
     
     
