;(ql:quickload :frame-extractor)
(ql:quickload :cl-ppcre)
(in-package :frame-extractor)

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

(defun filter-frames (filterfn sentence)
  "Applies given function to filter out unwanted frames in (annotated-)frame-elements of given sentence."
  (let* ((annotated-elems (cdr (assoc :annotated-frame-elements sentence)))
         (elems (cdr (assoc :frame-elements sentence)))
         (first-subst-sentence (subst (delete-if-not filterfn annotated-elems) annotated-elems sentence)))
    (subst (delete-if-not filterfn elems) elems first-subst-sentence)))

(defun load-parsings-with-annotations (parsing-path annotation-path)
  "Loads given frame-extractor output and corresponding annotations into one datastructure."
  (let* ((annotations (get-sentences-from-json annotation-path))
         (parsings (get-sentences-from-json parsing-path))
         (annotations-by-sentence (mapcar (lambda (a) (cons (cdr (assoc :sentence a)) (list a))) annotations)))
    (mapcar (lambda (parsing)
              (let ((annotation (cadr (assoc (cdr (assoc :sentence parsing)) annotations-by-sentence :test #'string=))))
                (append parsing (list `(:annotated-frame-elements . ,(cdr (assoc :frame-elements annotation)))))))
            parsings)))

(defun clean-slot-filler (frame-elem)
  "Cleans up the slot filler of given frame element by downcasing and replacing punctuation."
  (downcase (cl-ppcre:regex-replace-all "  " (cl-ppcre:regex-replace-all "'" (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "[,|\.|\"|:]" (cdr frame-elem) "") " ") " '") " ")))

(defun frame-similarity (this-frame other-frame)
  "Returns similarity between given frames via string matching."
  (length
    (intersection this-frame other-frame :key #'clean-slot-filler :test #'string=)))

(defun frame-slots (this-frame other-frame)
  "Returns number of different frame slots in given frames."
  (length
    (union
      (mapcar #'car this-frame)
      (mapcar #'car other-frame))))

(defun all-permutations (lst &optional (remain lst))
  "Returns all possible permutations of a given list."
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun bruteforce-alignment (these-frames other-frames)
  "Tries to align the most similar frames from two given frame sets
   by trying out all possible permutations."
  (let* ((these-count (length these-frames))
         (other-count (length other-frames))
         (these-padded (concatenate 'list these-frames (make-list (max 0 (- other-count these-count)) :initial-element nil)))
         (other-padded (concatenate 'list other-frames (make-list (max 0 (- these-count other-count)) :initial-element nil))))
    (car (sort (mapcar
                (lambda (perm)
                    (mapcar (lambda (this that) (cons this that))
                            these-padded perm))
                (all-permutations other-padded))
               #'> :key
               (lambda (e)
                   (reduce #'+ (mapcar (lambda (pair) (frame-similarity (car pair) (cdr pair))) e)))))))

(defun evaluate-sentence (sentence-structure)
  "Assigns the number of correct frames and frame-slot-fillers to each given sentence-output."
  (let* ((frames (cdr (assoc :frame-elements sentence-structure)))
         (annotated (cdr (assoc :annotated-frame-elements sentence-structure)))
         (alignment (bruteforce-alignment frames annotated))
         (frame-similarity
          (mapcar (lambda (pair)
                    (list (frame-similarity (car pair) (cdr pair))
                          (frame-slots (car pair) (cdr pair))))
                  alignment)))
    (append sentence-structure
            (list
             (cons
              :frame-similarity
              frame-similarity)
             (cons
              :slot-similarity
              (reduce (lambda (a v) (mapcar #'+ a v)) frame-similarity :initial-value (list 0 0)))))))

(defun total-slot-similarity (sentences)
  "Calculates the total number of frame-slots and the number of correct slot-fillers
   over a set of sentences."
  (reduce (lambda (a v) (mapcar #'+ a v)) (mapcar (lambda (v) (cdr (assoc :slot-similarity v))) sentences) :initial-value (list 0 0)))

(defun total-correct-sentences (sentences)
  "Calculates the total number of sentences that were parsed correctly over a given set of sentences."
  (length
   (remove-if-not (lambda (slot-sim) (equal (first slot-sim) (second slot-sim))) sentences :key (lambda (sent) (cdr (assoc :slot-similarity sent))))))

(defun evaluate-grammar-output-for-evoking-elem (evoking-elems)
  "Evaluates the frame-extractor output for given frame-evoking-elements by comparing it with corresponding annotations.
   Writes resulting output, annotations and correctness into json-file.
   Returns the total number of frame-slots and the number of correct slot-fillers as well as the number of correctly parsed sentences."
  
  (let* ((path-to-parse-results (babel-pathname :directory '(:up "Corpora" "Guardian") :name "frame-extractor-output" :type "json"))
         (path-to-annotations (babel-pathname :directory '(:up "Corpora" "Guardian") :name "100-causation-frame-annotations" :type "json"))
         (parsing-with-annotations (load-parsings-with-annotations path-to-parse-results path-to-annotations))
         (filtered-parsings (mapcar (lambda (s)
                                      (filter-frames (lambda (s)
                                                       (find (cdr (assoc :frame-evoking-element s)) evoking-elems :test #'string=))
                                                     s))
                                    parsing-with-annotations))
         (print-result (mapcar #'evaluate-sentence filtered-parsings)))
    
    (spit-json (babel-pathname :directory '(:up "Corpora" "Guardian") :name "frame-extractor-output-with-annotations" :type "json")
               print-result)
    
    (values (total-slot-similarity print-result)
            (total-correct-sentences print-result))))

(defun spit-json (path-name output-list)
  "Encodes given list into json and writes resulting json-objects into file of given name."
  (with-open-file (out path-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
     (write-line (encode-json-alist-to-string `((:evaluations ,@output-list)))
                 out)))


;(evaluate-grammar-output-for-evoking-elem '("cause"))


