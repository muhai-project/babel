;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

(defun pie-comprehend-log (utterance &key (cxn-inventory *fcg-constructions*) (silent nil))
  "Utility function to comprehend an utterance and extract the frames in one go.
   Returns both a frame-set and the last cip-node."
  (multiple-value-bind (meaning cipn) (comprehend utterance :cxn-inventory cxn-inventory :silent silent)
    (values cipn (run-pie cipn))))

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

(defun log-parsing-output-into-json-file (frame-evoking-elem-list)
  "Parses sentences from the Guardian training-corpus that contain the specified frame-evoking-elems.
   Encodes the resulting frame-sets into json-format and writes them into 'frame-extractor-output.json' file."
  (let* ((sentence-objs (get-sentences-from-json (babel-pathname :directory '(:up "Corpora" "Guardian") :name "100-causation-frame-annotations" :type "json")))
         (sentences (loop for sent in sentence-objs
                          when (intersection
                                (mapcar #'cdr (mapcar (lambda (x) (assoc :frame-evoking-element x)) (cdr (assoc :frame-elements sent))))
                                frame-evoking-elem-list :test #'string=)
                          collect (cdr (assoc :sentence sent)) into sentences
                          finally (return sentences))))
      (loop for sent in sentences
            for (last-cipn raw-frame-set) = (multiple-value-list (pie-comprehend-log (string-trim '(#\Space #\Backspace #\Linefeed #\Page #\Return) sent) :silent t))
            collect (encode-json-alist-to-string `((:sentence . ,sent)
                                                   (:frame-elements . ,(loop for frame in (pie::entities raw-frame-set)
                                                                             collect `((:frame-evoking-element . ,(pie::frame-evoking-element frame))
                                                                                       (:cause . ,(cause frame))
                                                                                       (:effect . ,(effect frame)))))
                                                   (:applied-cxns . ,(mapcar #'name (applied-constructions last-cipn))))) into results
            finally (with-open-file (out (babel-pathname :directory '(:up "corpora" "Guardian") :name "frame-extractor-output" :type "json")
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (loop for result in results
                            do (progn
                                 (format out result)
                                 (format out  "~%")))))))

;(activate-monitor trace-fcg)
;(log-parsing-output-into-json-file '("cause"))



;(pie-comprehend "Because the phenomenon causes less rain to fall in many areas of the tropics, forests become especially vulnerable to man-made fires, which accelerate carbon dioxide buildup in the atmosphere and reduce air quality.") ;working with new causative-to-cxn

;(pie-comprehend "The new study is led by Professor Stephan Lewandowsky, chair of cognitive psychology at the University of Bristol, and follows his previous study which caused the metaphorical head of the climate science denial blogosphere to explode.") ;working with new causative-to-cxn



;(pie-comprehend "With an immense scientific consensus that manmade greenhouse gases cause climate change, there is pressure to reduce carbon emissions, but little sign that governments can reach a binding agreement to cut back sufficiently.") ;NOT working, spacy tree incorrect. Maybe partial cxn for effect-filler?

;(pie-comprehend "As so many other polls have shown consistently, the majority of Australians believe climate change is happening and is caused by human activity.") ;NOT working, ellipsis not resoluted by spacy.

;(pie-comprehend "The great advantage that climate change has over other pressing issues is that the gases that cause it can be measured down to the last gram.") ;NOT recognising "it", spacy tree incorrect

;(pie-comprehend "On top of the upheaval caused by the drive to boost productivity, mountain biodiversity must now withstand climate change.") ;NOT working, spacy tree incorrect

;(pie-comprehend "Back in 1984, journalists reported from Ethiopia about a famine of biblical proportions caused by widespread drought.") ;NOT working, but maybe should be like this?

;(pie-comprehend "For example, King joined colleagues to look at the record warm sea temperatures that caused the mass bleaching of corals on the Great Barrier Reef last summer.") ;NOT working since annotation includes "last summer"...

;(pie-comprehend "It has been devastated by a combination of a long drought caused by a strong El NiÃ±o weather cycle and climate change.") ;NOT working, whole NP because spacy does not understand "combination", so should probably be like this?



;(pie-comprehend "But the ASA said the ad implied that the vehicle's emission rate was low in relation to all vehicles and that readers were likely to understand that the car caused little or no harm to the environment.The watchdog concluded that the ads were likely to mislead and banned the ads.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "Last year, Hurricane Felix caused widespread devastation to Nicaragua's coffee plantations.") ;NOT working, spacy excludesprepositional modifiers



;(pie-comprehend "This includes the extinction of the dinosaurs 65m years ago , thought to have been caused by the impact of a large asteroid on the Yucatan peninsula and beneath the Gulf of Mexico.") ;NOT working, thought=conj

;(pie-comprehend "Global warming is very likely to have been caused by human activity, the most authoritative global scientific body studying climate change said in a report today.") ;NOT working with stricter XcausedbyY

;(pie-comprehend "In 2001, the body - which brings together 2,500 scientists from more than 30 countries - said global warming was only likely, or 66% probable, to have been caused by humans.") ;NOT working with stricter XcausedbyY

(pie-comprehend "The extinction, thought to have been caused by the impact.")
(pie-comprehend "Warming is likely to have been caused by human activity.")
(pie-comprehend "Warming was only likely or probable to have been caused by humans.")
;maybe look for "acomp/comp"?
;otherwise build up syntactic substructure by hand ("to have been caused" and nearest preceding NP as effect) and then match on it?



;(pie-comprehend "Ultimately, temperature rise is the thing that matters, as warming causes all the other symptoms of climate change.") ;sometimes parataxis-cxn interferes

