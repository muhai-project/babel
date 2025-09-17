;;; Some additional math functions

(in-package :utils)

(export '(sum
          multf
	  compose
	  average
          median
	  stdev
          nth-percentile
	  correlation
          fac!
	  combination
          permutations
          rad-to-deg
          deg-to-rad
          euclidean
          iota
          log-transform-vector
          sum-list-of-vectors
          random-from-range
          normalize
          +inf -inf
          +inf.f -inf.f
          +inf.d -inf.d))

(declaim (inline sum))
(defun sum (values)
  (reduce #'+ values))

#| not used
(defun random-between (a b)
  "a random number in the interval [a b]"
  (declare (real a) (real b))
  (if (= a b) a
    (+ a (random (- b a)))))
|#

(define-modify-macro multf (&rest args)
  * "multiplies x with a number")

;(defun compose (f g)
;  (declare (function f) (function g))
;  "Return a function h such that (h x) = (f (g x))."
;  #'(lambda (x) (funcall f (funcall g x))))

(defun compose (&rest functions)
  "Given an arbitrary number of functions
   (e.g. f, g and h), return a function F
   such that (F x) = (f (g (h x)))"
  (if (= (length functions) 1)
      (car functions)
      (lambda (&rest args)
        (funcall (first functions)
                 (apply (apply #'compose (rest functions))
                        args)))))

(defun average (list &key (key #'identity))
  "the average of a list"
  (declare (list list) (function key))
  (loop for elm in list
        count t into length
        sum (funcall key elm) into sum
        finally (return (if (= length 0)
                          0 (coerce (/ sum length) 'float)))))

(defun %fast-average (list)
  (declare (list list))
  (loop for elm in list
        count t into length
        sum elm into sum
        finally (return (if (= length 0)
                          0 (coerce (/ sum length) 'float)))))

(define-compiler-macro average (&whole w list &key (key nil keyp))
  (if (or (not keyp)
          (equal key '(function identity))
          (equal key '(quote identity)))
    `(%fast-average ,list)
    w))

(defun median (list &key (already-sorted nil) (key #'identity))
  "the median of a list"
  (unless already-sorted 
    (setf list (sort (copy-list list) #'< :key key)))
  (let* ((length (length list))
         (half-length (/ length 2)))
    (if (evenp length)
        (/ (+ (nth (1- half-length) list)
              (nth half-length list)) 2)
        (nth (floor half-length) list))))

;; (defun stdev (l &key average (key #'identity))
;;   "the standard deviation of a list"
;;   (declare (list l) (function key))
;;   (let ((avg (or average (average l :key key))))
;;     (expt
;;      (/ (reduce #'+ (mapcar #'(lambda (e) 
;;                                 (expt (- (funcall key e) avg) 2))
;;                             l))
;;         (length l)
;; 	1.0)
;;      0.5)))

(defun stdev (list &key average (key #'identity))
  (declare (list list) (function key))
  (if list
      (let ((avg (or average (average list :key key))))
        (loop for elm in list
           count t into length
           sum (expt (- (funcall key elm) avg) 2) into sum
           finally (return (expt (coerce (/ sum length) 'float) 0.5))))
      0))

(defun %fast-stdev (list average)
  (declare (list list))
  (if list 
      (let ((avg (or average (average list))))
        (loop for elm in list
           count t into length
           sum (expt (- elm avg) 2) into sum
           finally (return (expt (coerce (/ sum length) 'float) 0.5))))
      0))

(define-compiler-macro stdev (&whole w list &key average (key nil keyp))
  (if (or (not keyp)
          (equal key '(function identity))
          (equal key '(quote identity)))
    `(%fast-stdev ,list ,average)
    w))

(defun nth-percentile (list n)
  ;; formula taken from definition 3 on http://cnx.org/content/m10805/latest/
  ;;; It uses interpolation to estimate the percentile
  (flet ((limit (nr low high)
           (cond ((< nr low) low)
                 ((> nr high) high)
                 (t nr))))
    (let* ((sorted-list (sort list #'<))
           (len (length sorted-list))
           (rank (limit (* (/ n 100) (+ 1 len)) 1 len)))
      (multiple-value-bind (ir fr) (floor rank)
        (cond ((= ir rank)
               (nth (- rank 1) sorted-list))
              (t
               (+ (* fr (- (nth ir sorted-list)
                           (nth (- ir 1) sorted-list)))
                  (nth (- ir 1) sorted-list))))))))

(defun correlation (datapoints)
  "Datapoints should be a list of (x . y) pairs. Returns values a,b,r**2 for
which y=a+bx is a least square fitting with the square of the correlation
coefficient equal to r**2."
  (labels ((ss (key1 key2 avg1 avg2)
	     (loop for p in datapoints sum
		   (* (- (funcall key1 p) avg1)
		      (- (funcall key2 p) avg2)))))
    (let* ((avgx (average datapoints :key #'car))
	   (avgy (average datapoints :key #'cdr))
	   (ssxx (ss #'car #'car avgx avgx))
	   (ssxy (ss #'car #'cdr avgx avgy))
	   (ssyy (ss #'cdr #'cdr avgy avgy))
	   (b (if (> ssxx 0) (/ ssxy ssxx) 0.0)))
      (values (- avgy (* b avgx))
	      b
	      (if (> ssyy 0) (/ (* b ssxy) ssyy) 0)))))

(defun fac! (nr &optional (start 1))
  (loop for i from start to nr
        for product = i then (* product i)
        finally (return product)))

(defun permutations (a &optional (b a))
  "The number of permutations of length b out of a"
  (fac! a (1+ (- a b))))

(defun combination (a b)
  "The combination for b out of a"
  (/ (permutations a b) (fac! b)))

(defun count-subsets (n)
  (1+ (loop for i from 1 to n
	 sum (combination n i))))


(defun vector-magnitude (vector)
  (let ((magnitude 0))
    (dolist (el vector)
          (setf magnitude (+ magnitude (* el el))))
    (sqrt magnitude)))

(defun dot-product (vector1 vector2 &rest more-vectors)
  (loop with result = 0
     for i from 0
     for el1 in vector1
     for el2 in vector2
     for more-els = (mapcar #'(lambda (vec)
                                (nth i vec))
                            more-vectors)
     do
       (setf result (+ result
                       (if (car more-vectors)
                           (eval `(* ,el1 ,el2 ,@more-els))
                           (* el1 el2))))
     finally (return result)))

(export '(cosine-similarity multiply-list-of-vectors))

(defun cosine-similarity (vector1 vector2 &rest more-vectors)
  (let ((magnitudes nil))
    (setf magnitudes
          (loop for vector in `(,vector1 ,vector2 ,@more-vectors)
                collect (vector-magnitude vector)))
    (if (find 0 magnitudes :test #'equalp)
      0
      (min 1 (/ (dot-product vector1 vector2 more-vectors)
                (eval `(funcall #'* ,@magnitudes)))))))

(defun multiply-list-of-vectors (list-of-vectors)
  "Cross product of a list of vectors."
  (reduce #'multiply-two-vectors list-of-vectors))

(defun multiply-two-vectors (vector-1 vector-2)
  "Cross product of two vectors."
  (mapcar #'* vector-1 vector-2))

(defun sum-list-of-vectors (list-of-vectors)
  "Sum a list of vectors."
  (reduce #'sum-two-vectors list-of-vectors))

(defun sum-two-vectors (vector-1 vector-2)
  "Cross product of two vectors."
  (mapcar #'+ vector-1 vector-2))


(defun rad-to-deg (rad)
  "Radians to degrees"
  (* 180.0 (/ rad pi)))

(defun deg-to-rad (deg)
  "Degrees to radians"
  (* pi (/ deg 180.0)))

(defun euclidean (a b)
  "Compute Euclidean distance between lists a and b"
  (let ((diff (mapcar #'- a b)))
    (float (sqrt (reduce #'+ (mapcar #'* diff diff))))))

(defun iota (max &key (min 0) (step 1))
  "Generate a list of numbers from min
   (default 0) to max by step (default 1)"
  (loop for n from min below max by step collect n))

(defun log-transform-vector (vector)
  "Take the log of all elements in the vector. Cannot contain zeros!"
  (mapcar #'log vector))

(defun random-from-range (start end)
  "Generate a random integer/float in the range [start,end]"
  (if (= start end)
    start
    (+ start (random (- end start)))))

(defun normalize (x min-x max-x)
  "Normalizes x between 0 and 1."
  (float (/ (- x min-x)
            (- max-x min-x))))

(defun linspace (start stop &key (num-points 50) (include-endpoint-p t))
  "Return 'num-points' evenly spaced points over the interval [start, stop].
   Use 'include-enpoint-p' to include or exclude the endpoint."
  (declare (fixnum num-points))
  (declare (number start stop))
  (let* ((step (/ (- stop start) (1- num-points)))
         (lst (do ((n 1 (1+ n))
		   (x (list start) (push (+ step (car x)) x)))
		  ((>= n (1- num-points)) (reverse x)))))
    (if include-endpoint-p
      (append lst (list stop))
      lst)))

(defun logspace (start stop &key (base 10) (num-points 50) (include-endpoint-p t))
  "Returns 'num-points' evenly spaced over a log scale with 'base'.
   In linear space, the points are spaced over the interval
   [base**start, base**stop]."
  (declare (fixnum num-points))
  (declare (number start stop))
  (let ((ln (linspace start stop
                      :num-points num-points
                      :include-endpoint-p include-endpoint-p)))
    (mapcar #'(lambda (x) (expt base x)) ln)))
         
;(mapcar #'round (linspace 0 250000 :num-points 1000))
;(remove-duplicates (mapcar #'round (logspace 0 (log 250000 10) :num-points 1000)))

(defconstant +inf most-positive-fixnum "positive infinity, integer")
(defconstant -inf most-negative-fixnum "negative infinity, integer")
(defconstant +inf.f most-positive-single-float "positive infinity, single precision floating-point")
(defconstant -inf.f most-negative-single-float "negative infinity, single precision floating-point")
(defconstant +inf.d most-positive-double-float "positive infinity, double precision floating-point")
(defconstant -inf.d most-negative-double-float "negative infinity, double precision floating-point")
