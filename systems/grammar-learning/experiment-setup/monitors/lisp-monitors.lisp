;;;; monitors.lisp

(in-package :grammar-learning)

;;;; Communicative success             
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Grammar size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :documentation "records the avg grammar size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "grammar-size" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (length (get-cxns-of-type (learner experiment) 'all))))


;;;; Gnuplot Display monitor
(define-monitor display-metrics
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success)
                                (average record-lexicon-size))
                :update-interval 100
                :caption '("communicative success"
                           "grammar size")
                :x-label "# Games"
                :use-y-axis '(1 2)
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0
                :y2-label "Grammar Size"
                :y2-min 0
                :draw-y1-grid t
                :error-bars nil)

;;;; Avg cxn score
(define-monitor record-avg-cxn-score
                :class 'data-recorder
                :average-window 100
                :documentation "record the avg cxn score")

(define-monitor export-avg-cxn-score
                :class 'lisp-data-file-writer
                :documentation "exports avg cxn score"
                :data-sources '((average record-avg-cxn-score))
                :file-name (babel-pathname :name "avg-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)
                
(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (get-cxns-of-type (learner experiment) 'all)))))

;;;; lexicon size per cxn type 
(define-monitor record-number-of-holophrase-cxns
                :class 'data-recorder)

(define-monitor export-number-of-holophrase-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-holophrase-cxns)
                :file-name (babel-pathname :name "number-of-holophrase-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-holophrase-cxns interaction-finished)
  (record-value
   monitor
   (length (get-cxns-of-type (learner experiment) 'gl::holophrase))))

(define-monitor record-number-of-item-based-cxns
                :class 'data-recorder)

(define-monitor export-number-of-item-based-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-item-based-cxns)
                :file-name (babel-pathname :name "number-of-item-based-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-item-based-cxns interaction-finished)
  (record-value
   monitor
   (length (get-cxns-of-type (learner experiment) 'gl::item-based))))

(define-monitor record-number-of-lexical-cxns
                :class 'data-recorder)

(define-monitor export-number-of-lexical-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-lexical-cxns)
                :file-name (babel-pathname :name "number-of-lexical-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-lexical-cxns interaction-finished)
  (record-value
   monitor
   (length (get-cxns-of-type (learner experiment) 'gl::lexical))))


;;;; avg cxn score per cxn type
(define-monitor record-avg-holophrase-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-holophrase-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-holophrase-cxn-score))
                :file-name (babel-pathname :name "avg-holophrase-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-holophrase-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (get-cxns-of-type (learner experiment) 'gl::holophrase)))))

(define-monitor record-avg-item-based-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-item-based-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-item-based-cxn-score))
                :file-name (babel-pathname :name "avg-item-based-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-item-based-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (get-cxns-of-type (learner experiment) 'gl::item-based)))))

(define-monitor record-avg-lexical-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-lexical-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-lexical-cxn-score))
                :file-name (babel-pathname :name "avg-lexical-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-lexical-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (get-cxns-of-type (learner experiment) 'gl::lexical)))))

;; nr of item-based cxns with slots 
(define-monitor record-num-item-based-1
                :class 'data-recorder)

(define-monitor export-num-item-based-1
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-1)
                :file-name (babel-pathname :name "num-item-based-1" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-1 interaction-finished)
  (record-value
   monitor
   (count 1 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-2
                :class 'data-recorder)

(define-monitor export-num-item-based-2
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-2)
                :file-name (babel-pathname :name "num-item-based-2" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-2 interaction-finished)
  (record-value
   monitor
   (count 2 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-3
                :class 'data-recorder)

(define-monitor export-num-item-based-3
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-3)
                :file-name (babel-pathname :name "num-item-based-3" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-3 interaction-finished)
  (record-value
   monitor
   (count 3 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-4
                :class 'data-recorder)

(define-monitor export-num-item-based-4
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-4)
                :file-name (babel-pathname :name "num-item-based-4" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-4 interaction-finished)
  (record-value
   monitor
   (count 4 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-5
                :class 'data-recorder)

(define-monitor export-num-item-based-5
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-5)
                :file-name (babel-pathname :name "num-item-based-5" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-5 interaction-finished)
  (record-value
   monitor
   (count 5 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-6
                :class 'data-recorder)

(define-monitor export-num-item-based-6
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-6)
                :file-name (babel-pathname :name "num-item-based-6" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-6 interaction-finished)
  (record-value
   monitor
   (count 6 (get-cxns-of-type (learner experiment) 'gl::item-based)
          :key #'item-based-number-of-slots :test #'=)))

;; repair monitors
(define-monitor record-repair-usage-nothing->holophrase
                :class 'data-recorder)

(define-monitor export-repair-usage-nothing->holophrase
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-nothing->holophrase)
                :file-name (babel-pathname :name "repair-usage-nothing-to-holophrase" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-nothing->holophrase interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "h")
     1
     0)))

(define-monitor record-repair-usage-lexical->item-based
                :class 'data-recorder)

(define-monitor export-repair-usage-lexical->item-based
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-lexical->item-based)
                :file-name (babel-pathname :name "repair-usage-lexical-to-item-based" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-lexical->item-based interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "i")
     1
     0)))

(define-monitor record-repair-usage-item-based->lexical
                :class 'data-recorder)

(define-monitor export-repair-usage-item-based->lexical
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-item-based->lexical)
                :file-name (babel-pathname :name "repair-usage-item-based-to-lexical" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-item-based->lexical interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "l")
     1
     0)))

(define-monitor record-repair-usage-holophrase->item-based+lexical+lexical--substitution
                :class 'data-recorder)

(define-monitor export-repair-usage-holophrase->item-based+lexical+lexical--substitution
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-holophrase->item-based+lexical+lexical--substitution)
                :file-name (babel-pathname :name "repair-usage-holophrase-to-item-based+lexical+lexical--substitution" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-holophrase->item-based+lexical+lexical--substitution interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "s")
     1
     0)))

(define-monitor record-repair-usage-holophrase->item-based+lexical+lexical--addition
                :class 'data-recorder)

(define-monitor export-repair-usage-holophrase->item-based+lexical+lexical--addition
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-holophrase->item-based+lexical+lexical--addition)
                :file-name (babel-pathname :name "repair-usage-holophrase-to-item-based+lexical+lexical--addition" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-holophrase->item-based+lexical+lexical--addition interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "a")
     1
     0)))

(define-monitor record-repair-usage-holophrase->item-based+lexical+lexical--deletion
                :class 'data-recorder)

(define-monitor export-repair-usage-holophrase->item-based+lexical+lexical--deletion
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-holophrase->item-based+lexical+lexical--deletion)
                :file-name (babel-pathname :name "repair-usage-holophrase-to-item-based+lexical+lexical--deletion" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-holophrase->item-based+lexical+lexical--deletion interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "d")
     1
     0)))


(define-monitor record-repair-usage-add-categorial-links
                :class 'data-recorder)

(define-monitor export-repair-usage-add-categorial-links
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-add-categorial-links)
                :file-name (babel-pathname :name "repair-usage-add-categorial-links" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-add-categorial-links interaction-finished)
  (record-value
   monitor
   (if (string= (last-elt (repair-buffer experiment)) "t")
     1
     0)))


;;;; Number of nodes in the type hierarchy
(define-monitor record-number-of-nodes-in-th
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of nodes in the type hierarchy")

(define-monitor export-number-of-nodes-in-th
                :class 'lisp-data-file-writer
                :documentation "exports the number of nodes in the type hierarchy"
                :data-sources '(record-number-of-nodes-in-th)
                :file-name (babel-pathname :name "number-of-th-nodes" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-nodes-in-th interaction-finished)
  (record-value monitor
                  (categories
                   (grammar
                    (learner experiment)))))


;;;; Number of edges in the type hierarchy
(define-monitor record-number-of-edges-in-th
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of edges in the type hierarchy")

(define-monitor export-number-of-edges-in-th
                :class 'lisp-data-file-writer
                :documentation "exports the number of edges in the type hierarchy"
                :data-sources '(record-number-of-edges-in-th)
                :file-name (babel-pathname :name "number-of-th-edges" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-edges-in-th interaction-finished)
  (record-value monitor
                
                  (links
                   (grammar
                    (learner experiment)))))

;; utilty function to get all of them
(defun get-all-lisp-monitors ()
  '("export-communicative-success"
    "export-lexicon-size"
    "export-avg-cxn-score"
    "export-number-of-nodes-in-th"
    "export-number-of-edges-in-th"
    "export-num-item-based-1"
    "export-num-item-based-2"
    "export-num-item-based-3"
    "export-num-item-based-4"
    "export-num-item-based-5"
    "export-num-item-based-6"
    "export-repair-usage-nothing->holophrase"
    "export-repair-usage-lexical->item-based"
    "export-repair-usage-item-based->lexical"
    "export-repair-usage-holophrase->item-based+lexical+lexical--substitution"
    "export-repair-usage-holophrase->item-based+lexical+lexical--addition"
    "export-repair-usage-holophrase->item-based+lexical+lexical--deletion"
    "export-repair-usage-add-categorial-links"
    "export-number-of-holophrase-cxns"
    "export-number-of-item-based-cxns"
    "export-number-of-lexical-cxns"
    "export-avg-holophrase-cxn-score"
    "export-avg-item-based-cxn-score"
    "export-avg-lexical-cxn-score"
))
