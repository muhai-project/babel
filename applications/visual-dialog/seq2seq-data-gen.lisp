(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(defparameter *clevr-data-path*
  (parse-namestring "/Users/jensnevens/corpora/CLEVR-v1.0/"))

(defparameter *mnist-data-path*
  (parse-namestring "/Users/jensnevens/corpora/MNIST-Dialog/"))

(defparameter *clevr-val-world*
  (make-instance 'world
                 :entries '((:dataset .  :clevr)
                            (:datasplit . :val)
                            (:mode . :symbolic))))

(defparameter *mnist-val-world*
  (make-instance 'world
                 :entries '((:dataset . :mnist)
                            (:datasplit . :val)
                            (:mode . :symbolic))))

(activate-monitor trace-fcg)

(comprehend-dialogs 0 5 *clevr-val-world*)


(defun add-irl-program-to-web-interface (irl-program)
  (add-element `((div) ,(irl-program->svg (read-from-string (mkstr (shuffle irl-program)))))))


#|
;; how about the previous purple thing
(clevr-meaning->rpn
 '((QUERY ?SPECIFIC-ATTRIBUTE-942 ?ONETARGET-155 ?SCENE-4087 ?ATTRIBUTE-1546)
   (FILTER-BY-ATTRIBUTE ?TARGET-5557 ?TARGET-5436 ?SCENE-6237 ?COLOR-100)
   (BIND SHAPE-CATEGORY ?OBJECT-1246 THING)
   (FILTER-BY-ATTRIBUTE ?TARGET-5436 ?SOURCE-2318 ?SCENE-6099 ?OBJECT-1246)
   (BIND COLOR-CATEGORY ?COLOR-100 PURPLE)
   (UNIQUE ?ONETARGET-155 ?TARGET-5620)
   (FIND-IN-CONTEXT ?TARGET-5620 ?SOURCE-2401 ?TARGET-5557)
   (SEGMENT-SCENE ?SOURCE-2401 ?SCENE-6547)
   (GET-LAST-ATTRIBUTE-CATEGORY ?ATTRIBUTE-1546 ?MEMORY-2366)))

;; to do: make sure that caption bindings/constants are also added
(clevr-meaning->rpn
 '((BIND shape-category ?SHAPE-4315 CYLINDER)
   (FILTER-BY-ATTRIBUTE ?TARGET-447664 ?SOURCE-190852 ?SCENE-502946 ?SHAPE-4315)
   (MORE-THAN-1 YES ?TARGET-447664)
   (SEGMENT-SCENE ?SOURCE-190852 ?SCENE-503290)))

(clevr-meaning->rpn
 '((EXTREME-RELATE ?TARGET-210 ?SOURCE-104 ?SCENE-235 ?RELATION-53)
   (BIND COLOR-CATEGORY ?COLOR-23 PURPLE)
   (FILTER-BY-ATTRIBUTE ?TARGET-420 ?TARGET-210 ?SCENE-468 ?OBJECT-69)
   (BIND SHAPE-CATEGORY ?OBJECT-69 THING)
   (BIND SPATIAL-RELATION-CATEGORY ?RELATION-53 LEFT)
   (SELECT-ONE ?UNIQUE-443 ?TARGET-544)
   (FILTER-BY-ATTRIBUTE ?TARGET-544 ?TARGET-420 ?SCENE-605 ?COLOR-23)
   (BIND BOOLEAN-CATEGORY ?YES-111 YES)
   (EXIST ?YES-111 ?UNIQUE-443)
   (SEGMENT-SCENE ?SOURCE-104 ?SCENE-887)))

(clevr-meaning->rpn
 '((BIND MATERIAL-CATEGORY ?MATERIAL-1140 RUBBER) (FILTER-BY-ATTRIBUTE ?TARGET-62712 ?SOURCE-26703 ?SCENE-70466 ?OBJECT-16540) (BIND SPATIAL-RELATION-CATEGORY ?RELATION-16345 FRONT) (BIND SHAPE-CATEGORY ?ROUND-34 SPHERE) (FILTER-BY-ATTRIBUTE ?TARGET-62292 ?SOURCE-26527 ?SCENE-69996 ?OBJECT-16442) (BIND SHAPE-CATEGORY ?OBJECT-16442 THING) (SELECT-ONE ?UNIQUE-43506 ?TARGET-62373) (FILTER-BY-ATTRIBUTE ?TARGET-62373 ?TARGET-62292 ?SCENE-70087 ?ROUND-34) (BIND SHAPE-CATEGORY ?OBJECT-16540 THING) (SELECT-ONE ?UNIQUE-43813 ?TARGET-62774) (FILTER-BY-ATTRIBUTE ?TARGET-62774 ?TARGET-62712 ?SCENE-70529 ?MATERIAL-1140) (EXIST ?YES-10366 ?UNIQUE-43813) (BIND BOOLEAN-CATEGORY ?YES-10366 YES) (SEGMENT-SCENE ?SOURCE-26527 ?SCENE-70999) (IMMEDIATE-RELATE ?SOURCE-26703 ?UNIQUE-43506 ?SOURCE-26527 ?SCENE-70756 ?RELATION-16345)))
;; segmentScene segmentScene filter_shape[thing] filter_shape[sphere] selectOne immediateRelate_front filter_shape[thing] filter_material[rubber] selectOne exist[yes]



(add-element
 `((div)
   ,(irl-program->svg 
     (preprocess-program
      (read-from-string
       (mkstr
        '((CLEVR-DIALOG-GRAMMAR:FIND-IN-CONTEXT #:?TARGET-363312 #:?SOURCE-154961 #:?TARGET-363280)
          (CLEVR-DIALOG-GRAMMAR:UNIQUE #:?ONETARGET-9923 #:?TARGET-363312)
          (UTILS:BIND CLEVR-DIALOG-GRAMMAR:MATERIAL-CATEGORY #:?MATERIAL-3053 CLEVR-DIALOG-GRAMMAR:METAL)
          (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362737 #:?SOURCE-154718 #:?SCENE-407320 #:?OBJECT-94042)
          (CLEVR-DIALOG-GRAMMAR:COUNT-OBJECTS #:?COUNT-79204 #:?TARGET-362461)
          (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94097 CLEVR-DIALOG-GRAMMAR:THING)
          (UTILS:BIND CLEVR-DIALOG-GRAMMAR:ATTRIBUTE-CATEGORY #:?ATTRIBUTE-155040 FCG:SIZE)
          (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94042 CLEVR-DIALOG-GRAMMAR:THING)
          (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362943 #:?SOURCE-154961 #:?SCENE-407556 #:?OBJECT-94097)
          (CLEVR-DIALOG-GRAMMAR:SET-DIFF #:?SOURCE-154596 #:?TARGET-362943 #:?TARGET-363280 #:?SCENE-406661)
          (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-363280 #:?TARGET-362737 #:?SCENE-407925 #:?MATERIAL-3053)
          (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362461 #:?SOURCE-154596 #:?SCENE-407014 #:?SPEC-ATTR-19793)
          (CLEVR-DIALOG-GRAMMAR:QUERY #:?SPEC-ATTR-19793 #:?ONETARGET-9923 #:?SCENE-407014 #:?ATTRIBUTE-155040)
          (CLEVR-DIALOG-GRAMMAR:SEGMENT-SCENE #:?SOURCE-154961 #:?SCENE-407760))))))))

(clevr-meaning->rpn
 '((CLEVR-DIALOG-GRAMMAR:FIND-IN-CONTEXT #:?TARGET-363312 #:?SOURCE-154961 #:?TARGET-363280)
   (CLEVR-DIALOG-GRAMMAR:UNIQUE #:?ONETARGET-9923 #:?TARGET-363312)
   (UTILS:BIND CLEVR-DIALOG-GRAMMAR:MATERIAL-CATEGORY #:?MATERIAL-3053 CLEVR-DIALOG-GRAMMAR:METAL)
   (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362737 #:?SOURCE-154718 #:?SCENE-407320 #:?OBJECT-94042)
   (CLEVR-DIALOG-GRAMMAR:COUNT-OBJECTS #:?COUNT-79204 #:?TARGET-362461)
   (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94097 CLEVR-DIALOG-GRAMMAR:THING)
   (UTILS:BIND CLEVR-DIALOG-GRAMMAR:ATTRIBUTE-CATEGORY #:?ATTRIBUTE-155040 FCG:SIZE)
   (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94042 CLEVR-DIALOG-GRAMMAR:THING)
   (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362943 #:?SOURCE-154961 #:?SCENE-407556 #:?OBJECT-94097)
   (CLEVR-DIALOG-GRAMMAR:SET-DIFF #:?SOURCE-154596 #:?TARGET-362943 #:?TARGET-363280 #:?SCENE-406661)
   (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-363280 #:?TARGET-362737 #:?SCENE-407925 #:?MATERIAL-3053)
   (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362461 #:?SOURCE-154596 #:?SCENE-407014 #:?SPEC-ATTR-19793)
   (CLEVR-DIALOG-GRAMMAR:QUERY #:?SPEC-ATTR-19793 #:?ONETARGET-9923 #:?SCENE-407014 #:?ATTRIBUTE-155040)
   (CLEVR-DIALOG-GRAMMAR:SEGMENT-SCENE #:?SOURCE-154961 #:?SCENE-407760)))


;; how many X share the same size as the aforementioned metal thing
;; getMemory(0/1) filter_material[metal](1/1) segmentScene(0/1) findInContext(2/1) unique(1/1) query_size(1/1)
;; getMemory(0/1) filter_material[metal](1/1) segmentScene(0/1) setDiff(2/1)
;; filter(2/1) countObjects(1/1)
(add-element
 `((div)
   ,(irl-program->svg 
     (read-from-string
      (mkstr
       (shuffle
       '((CLEVR-DIALOG-GRAMMAR:FIND-IN-CONTEXT #:?TARGET-363312 #:?SOURCE-154961 #:?TARGET-363280)
         (CLEVR-DIALOG-GRAMMAR:UNIQUE #:?ONETARGET-9923 #:?TARGET-363312)
         (UTILS:BIND CLEVR-DIALOG-GRAMMAR:MATERIAL-CATEGORY #:?MATERIAL-3053 CLEVR-DIALOG-GRAMMAR:METAL)
         (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362737 #:?SOURCE-154718 #:?SCENE-1 #:?OBJECT-94042)
         (CLEVR-DIALOG-GRAMMAR:COUNT-OBJECTS #:?COUNT-79204 #:?TARGET-362461)
         (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94097 CLEVR-DIALOG-GRAMMAR:THING)
         (UTILS:BIND CLEVR-DIALOG-GRAMMAR:ATTRIBUTE-CATEGORY #:?ATTRIBUTE-155040 FCG:SIZE)
         (UTILS:BIND CLEVR-DIALOG-GRAMMAR:SHAPE-CATEGORY #:?OBJECT-94042 CLEVR-DIALOG-GRAMMAR:THING)
         (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362943 #:?SOURCE-154961 #:?SCENE-2 #:?OBJECT-94097)
         (CLEVR-DIALOG-GRAMMAR:SET-DIFF #:?SOURCE-154596 #:?TARGET-362943 #:?TARGET-363280 #:?SCENE-3)
         (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-363280 #:?TARGET-362737 #:?SCENE-4 #:?MATERIAL-3053)
         (CLEVR-DIALOG-GRAMMAR:FILTER-BY-ATTRIBUTE #:?TARGET-362461 #:?SOURCE-154596 #:?SCENE-5 #:?SPEC-ATTR-19793)
         (CLEVR-DIALOG-GRAMMAR:QUERY #:?SPEC-ATTR-19793 #:?ONETARGET-9923 #:?SCENE-6 #:?ATTRIBUTE-155040)
         (CLEVR-DIALOG-GRAMMAR:SEGMENT-SCENE #:?SOURCE-154961 #:?SCENE-7))))))))
|#