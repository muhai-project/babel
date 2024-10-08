;;;; Web service for the semantic frame extractor

(in-package :frame-extractor)

(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
  (let (missing-keys)
    (loop for key in keys
          unless (assoc key json)
          do (push key missing-keys))
    missing-keys))
#|
(defmethod encode-json ((frame frame)
                        &optional (stream *json-output*))
  "Overwrite of encode-json for a frame"
  (with-object (stream)
    (cl-mop:map-slots (lambda (key value)
                        (encode-object-member
                         (internal-symb (string-replace (mkstr key) "-" "--"))
                         (when value (mkstr value))
                         stream))
                      frame)))
|#

(defmethod snooze:explain-condition ((condition snooze:http-condition)
                                     resource
                                     ct)
  "Overload the explain-condition method to provide clearer error handling
   to the user of the API. A JSON object with status-code and error message
   will be send back."
  (encode-json-to-string
   `((:status--code . ,(format nil "~a" (snooze:status-code condition)))
     (:details . ,(apply #'format nil (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'extract-frames)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :utterance :frames))
         (utterance (rest (assoc :utterance json)))
         (frames (rest (assoc :frames json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (snooze:http-condition 400 "Utterance is not a string! Instead, received something of type ~a" (type-of utterance)))
    
    (load-frames frames)
    
    (let ((frame-set (handler-case (pie-comprehend utterance :silent silent :cxn-inventory *fcg-constructions*)
                       (error (e)
                         (snooze:http-condition 500 "Error in precision language processing module!" e)))))
      (encode-json-alist-to-string
       `((:frame-set . ,(loop for frame in (pie::entities frame-set)
                           collect frame)))))))

(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'texts-extract-frames)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :texts :frames))
         (texts (rest (assoc :texts json)))
         (frames (rest (assoc :frames json)))
         (silent (if (assoc :silent json)
                   (rest (assoc :silent json))
                   t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp texts)
      (snooze:http-condition 400 "Texts is not a list! Instead, received something of type ~a" (type-of texts)))
    (load-frames frames)
     (handler-case (trivial-timeout:with-timeout (280)
                     (let ((text-frame-sets (loop for text in texts
                                                  for utterances = (get-penelope-sentence-tokens text)
                                                  collect (loop for utterance in utterances
                                                                for frame-set = (if (cl-ppcre:scan-to-strings ".*([ ^][Cc]aus.+|[ ^][Dd]ue to|[ ^][Ll]ea?d(s|ing)? to|[ ^][rR]esult(s|ed|ing)? in|[ ^][Bb]ecause|[ ^][gG][ia]v(e|es|ing|en) rise to).*" utterance)
                                                                                  (handler-case (pie-comprehend-with-timeout utterance :silent silent :cxn-inventory *fcg-constructions*)
                                                                                    (error (e)
                                                                                      (snooze:http-condition 500 (format nil "Error in precision language processing module! Sentence: ~a" utterance) e)))
                                                                                  (make-instance 'frame-set
                                                                                                 :entities nil
                                                                                                 :utterance (or utterance "")
                                                                                                 :id (make-id 'frame-set)))
                                                                when frame-set
                                                                collect it))))
                       (encode-json-alist-to-string
                        `((:frame-sets . ,text-frame-sets)))))
       (trivial-timeout:timeout-error (time-out-error)
         (snooze:http-condition 500 "Total timeout exceeded, try to send fewer text(s)!" time-out-error)))))

(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'texts-extract-causes-effects)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :texts))
         (texts (rest (assoc :texts json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp texts)
      (snooze:http-condition 400 "Texts is not a list! Instead, received something of type ~a" (type-of texts)))
    
    (load-frames '("Causation"))

    (handler-case (trivial-timeout:with-timeout (280)
                    (let* ((text-frame-sets (loop for text in texts
                                                  for utterances = (get-penelope-sentence-tokens text)
                                                  append (loop for utterance in utterances
                                                               for frame-set = (when (cl-ppcre:scan-to-strings ".*([ ^][Cc]aus.+|[ ^][Dd]ue to|[ ^][Ll]ea?d(s|ing)? to|[ ^][rR]esult(s|ed|ing)? in|[ ^][Bb]ecause|[ ^][gG][ia]v(e|es|ing|en) rise to).*" utterance)
                                                                                 (handler-case (pie-comprehend-with-timeout utterance :silent silent :cxn-inventory *fcg-constructions*)
                                                                                   (error (e)
                                                                                     (snooze:http-condition 500 (format nil "Error in precision language processing module! Sentence: ~a" utterance) e))))
                                                               when frame-set
                                                               collect it)))
                           (utterances-with-causes-and-effects (loop for frameset in text-frame-sets
                                                                     for utterance = (utterance frameset)
                                                                     append (loop for entity in (pie::entities frameset)
                                                                                  for cause = (if (or (eql (cause entity) nil)
                                                                                                      (stringp (cause entity)))
                                                                                                (cause entity)
                                                                                                (utterance (cause entity)))
                                                                                  for effect = (if (or (eql (effect entity) nil)
                                                                                                       (stringp (effect entity)))
                                                                                                 (effect entity)
                                                                                                 (utterance (effect entity)))
                                                                                  when (or cause effect)
                                                                                  collect `((:utterance . ,utterance)
                                                                                            (:cause . ,cause)
                                                                                            (:effect . ,effect))))))
                      
                      (encode-json-alist-to-string
                       `((:causal-relations . ,utterances-with-causes-and-effects)))))
                    (trivial-timeout:timeout-error (time-out-error)
                      (snooze:http-condition 500 "Total timeout exceeded, try to send fewer text(s)!" time-out-error)))))


(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'texts-extract-causes-effects-indices)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :texts))
         (texts (rest (assoc :texts json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp texts)
      (snooze:http-condition 400 "Texts is not a list! Instead, received something of type ~a" (type-of texts)))
    
    (load-frames '("Causation"))

    (handler-case (trivial-timeout:with-timeout (280)
                    (let* ((text-frame-sets (loop for text in texts
                                                  for utterances = (get-penelope-sentence-tokens text)
                                                  collect (loop for utterance in utterances
                                                                for frame-set = (if (cl-ppcre:scan-to-strings ".*([ ^][Cc]aus.+|[ ^][Dd]ue to|[ ^][Ll]ea?d(s|ing)? to|[ ^][rR]esult(s|ed|ing)? in|[ ^][Bb]ecause|[ ^][gG][ia]v(e|es|ing|en) rise to).*" utterance)
                                                                                  (handler-case (pie-comprehend-with-timeout utterance :silent silent :cxn-inventory *fcg-constructions* :strings-as-output nil)
                                                                                    (error (e)
                                                                                      (snooze:http-condition 500 (format nil "Error in precision language processing module! Sentence: ~a" utterance) e)))
                                                                                  (make-instance 'frame-set
                                                                                                 :entities nil
                                                                                                 :utterance (or utterance "")
                                                                                                 :id (make-id 'frame-set)))
                                                                when frame-set
                                                                collect it)))
                           (utterances-with-causes-and-effects (loop for framesets in text-frame-sets
                                                                     collect (loop for frameset in framesets
                                                                                   for utterance = (utterance frameset)
                                                                                   append (if (pie::entities frameset)
                                                                                            (loop for entity in (pie::entities frameset)
                                                                                                  collect entity)
                                                                                            `(((:utterance . ,utterance))))))))
                      (encode-json-alist-to-string
                       `((:frame-sets . ,text-frame-sets)))))
      (trivial-timeout:timeout-error (time-out-error)
        (snooze:http-condition 500 "Total timeout exceeded, try to send fewer text(s)!" time-out-error)))))


(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'causation-tracker)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :phrase :direction :data))
         (phrase (rest (assoc :phrase json)))
         (direction (rest (assoc :direction json)))
         (data (rest (assoc :data json))))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp phrase)
      (snooze:http-condition 400 "Phrase is not a string! Instead, received something of type ~a" (type-of phrase)))
    (unless (stringp direction)
      (snooze:http-condition 400 "Direction is not a string! Instead, received something of type ~a" (type-of direction)))
    (unless (stringp data)
      (snooze:http-condition 400 "Data is not a string! Instead, received something of type ~a" (type-of data)))
    (if (equalp direction "cause->effect")
      (cause->effect-graph phrase data)
      (effect->cause-graph phrase data))))

(snooze:defroute semantic-frame-extractor (:options :text/* (op (eql 'causation-tracker))))

;; curl -H "Content-Type: application/json" -d '{"texts" : ["Satellite measurements have problems because of calibration changes when they are replaced every few years."]}' http://localhost:9007/semantic-frame-extractor/texts-extract-causes-effects-indices

;;(activate-monitor trace-fcg)

;;(pie-comprehend "I will remind you that the Fukusihima accident caused zero deaths or serious injuries due to radiation exposure and the main harm was caused by an overblown evacuation and fear of radiation totally out of proportion to risk spread by unscrupulous organizations.")

;;(pie-comprehend "Perhaps the concerns of many revolve around the fact that nuclear power became substantially safer only after the disaster.\nBQEND\nYou would have something to complain about if there was not a major review of nuclear safety right around the world. What exactly are you complaining about? I will remind you that the Fukusihima accident caused zero deaths or serious injuries due to radiation exposure and the main harm was caused by an overblown evacuation and fear of radiation totally out of proportion to risk spread by unscrupulous organizations. Greenpeace is still at it.\nYou might like to compare these consequences to those of the Bhopal disaster if you want to see what a really, really bad industrial accident with release of hazardous materials looks like.\nHeres a collection of scientific papers that cast a great deal of doubt upon the scale of evacuation ordered by the Japanese government.\nCoping with a big nuclear accident; Closing papers from the NREFS project\nAs for your reference - it is not observations by the Office for Nuclear Regulation. It is not authored by the Office for Nuclear Regulation. It is in fact a submission from a member of the public for the report into the Implications of the Fukushima Nuclear Accident. The ONR takes public submissions and responds to them. Among other things it contains climate change denier garbage: \n40% of a one-degree centigrade rise in the Earth’s surface temperature may be attributed directly to heat output [of thermal power plants].\nThis submission is authored by one John Urquhart who appears to be a fruit cake. Heres some more of his work:\nGuest Blog: Why the Nuclear Industry is Killing Off the Human Race by John Urquhart\nNow that\'s quite fruity. I will remind you that the Fukusihima accident caused zero deaths or serious injuries due to radiation exposure and the main harm was caused by an overblown evacuation and fear of radiation totally out of proportion to risk spread by unscrupulous organizations")
;;curl -H "Content-Type: application/json" -d '{"texts" : ["Perhaps the concerns of many revolve around the fact that nuclear power became substantially safer only after the disaster.\nBQEND\nYou would have something to complain about if there was not a major review of nuclear safety right around the world. What exactly are you complaining about? I will remind you that the Fukusihima accident caused zero deaths or serious injuries due to radiation exposure and the main harm was caused by an overblown evacuation and fear of radiation totally out of proportion to risk spread by unscrupulous organizations. Greenpeace is still at it.\nYou might like to compare these consequences to those of the Bhopal disaster if you want to see what a really, really bad industrial accident with release of hazardous materials looks like.\nHeres a collection of scientific papers that cast a great deal of doubt upon the scale of evacuation ordered by the Japanese government.\nCoping with a big nuclear accident; Closing papers from the NREFS project\nAs for your reference - it is not observations by the Office for Nuclear Regulation. It is not authored by the Office for Nuclear Regulation. It is in fact a submission from a member of the public for the report into the Implications of the Fukushima Nuclear Accident. The ONR takes public submissions and responds to them. Among other things it contains climate change denier garbage: \n40% of a one-degree centigrade rise in the Earth’s surface temperature may be attributed directly to heat output [of thermal power plants].\nThis submission is authored by one John Urquhart who appears to be a fruit cake. Heres some more of his work:\nGuest Blog: Why the Nuclear Industry is Killing Off the Human Race by John Urquhart\nNow that\'s quite fruity. I will remind you that the Fukusihima accident caused zero deaths or serious injuries due to radiation exposure and the main harm was caused by an overblown evacuation and fear of radiation totally out of proportion to risk spread by unscrupulous organizations"]}' http://localhost:9004/semantic-frame-extractor/texts-extract-causes-effects


;; {"frameSets":[[[{"id":"causationFrame15","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame30","frameEvokingElement":"cause","cause":"they","effect":"damage to their own clothes","actor":null,"affected":null}],[{"id":"causationFrame16","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]],[[{"id":"causationFrame17","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]]]}


;; curl -H "Content-Type: application/json" -d '{"utterance" : "Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs caused by people."}' http://localhost:9007/propbank-frame-extractor/extract-frames




;; {"frameSet":[{"id":"causationFrame4","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame8","frameEvokingElement":"cause","cause":"the company","effect":"damage to their own clothes","actor":null,"affected":null},]}



;; curl -H "Content-Type: application/json" -d '{"texts" : ["Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs. This causes that.", "This is a sentence. This causes that."], "frames" : ["Causation"]}' http://localhost:9004/semantic-frame-extractor/texts-extract-frames
;; {"frameSets":[[[{"id":"causationFrame15","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame30","frameEvokingElement":"cause","cause":"they","effect":"damage to their own clothes","actor":null,"affected":null}],[{"id":"causationFrame16","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]],[[{"id":"causationFrame17","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]]]}



;;Testing on AI Lab server:

;; curl -H "Content-Type: application/json" -d '{"texts" : ["With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels."], "frames" : ["Causation"]}' https://penelope.vub.be/semantic-frame-extractor/texts-extract-frames
