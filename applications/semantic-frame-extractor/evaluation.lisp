
(in-package :frame-extractor)
;(ql:quickload :frame-extractor)
(defun pie-comprehend-with-timeout (utterance timeout)
  (handler-case (trivial-timeout:with-timeout (timeout)
                  (pie-comprehend utterance
                                  :cxn-inventory *fcg-constructions*
                                  :silent t))
    (trivial-timeout:timeout-error (error)
      'timeout)))

(defun process-entry (json-object &key (time-out 30))
  ""
  (let* ((decoded-json-object (decode-json-from-string json-object))
         (sentence (cdr (assoc :sentence decoded-json-object)))
         (sentence-id (assoc :sentence-id  decoded-json-object))
         (article-id (assoc :article-id  decoded-json-object)))

    (when (cl-ppcre::scan-to-strings " [cC]aus.+" sentence) ;;only when sentence contains caus+
      
      (format t "~%[~a]:Comprehending \"~a\"~%" (cdr sentence-id) sentence)
      
      (let ((frame-set (pie-comprehend-with-timeout sentence time-out)))
        
        (if (eql frame-set 'timeout)
          
          (encode-json-alist-to-string
           `(,sentence-id
             ("sentence" . ,sentence)
             ("frame-elements" . nil)
             ,article-id
             ("timeout" . t)))

          (when (pie::entities frame-set)
            (let ((cause (frame-extractor::cause (first (pie::entities frame-set)))) ;;what if there are more?
                  (effect (frame-extractor::effect (first (pie::entities frame-set)))))
              (when (and cause effect)
                (encode-json-alist-to-string
                 `(,sentence-id
                   ("sentence" . ,sentence)
                   ("frame-elements" (("frame-evoking-element" . ,(pie::frame-evoking-element (first (pie::entities frame-set))))
                                      ("cause" . ,cause)
                                      ("effect" . ,effect)))
                   ,article-id
                   ("timeout" . nil)))))))))))


(defun evaluate-guardian-grammar-in-parallel (inputfile outputfile &optional (time-out 30))   
  (process-corpus :function #'process-entry
                  :function-kwargs (list :time-out time-out)
                  :inputfile inputfile
                  :outputfile outputfile
                  :number-of-threads 8
                  :number-of-lines-per-thread 50))

(evaluate-guardian-grammar-in-parallel (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                       :name "guardian-article-sentences-one-sentence-per-line"
                                                       :type "json")
                                       (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                       :name "guardian-causation-frames"
                                                       :type "json"))