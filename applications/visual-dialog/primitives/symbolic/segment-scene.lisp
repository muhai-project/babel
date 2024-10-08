(in-package :visual-dialog)

;; -----------------------
;; SEGMENT-SCENE primitive ;;
;; -----------------------

(defprimitive segment-scene ((segmented-scene world-model)
                             (scene-pathname pathname-entity))
  ;; first case; given scene-pathname compute segmented-scene
  ;; if computed scene is already calculated, find it back in ontology, so no problems with ids changing over turns in dialog
  ((scene-pathname => segmented-scene)
   (if (or (not (find-data ontology 'segmented-scene))
           (not (find (path scene-pathname) (get-data ontology 'segmented-scene) :test #'equal :key #'first)))
     (let* ((world (get-data ontology 'world))
            (scene-index (position (path scene-pathname) (scenes world)))
            (scene (get-scene-by-index world scene-index))
            (context (make-context world)))
       (if (not (find-data ontology 'segmented-scene))
         (set-data ontology 'segmented-scene (list (cons (path scene-pathname) context)))
         (set-data ontology 'segmented-scene (push (cons (path scene-pathname) context) (get-data ontology 'segmented-scene))))
       (bind (segmented-scene 1.0 context)))
     (when (find (path scene-pathname) (get-data ontology 'segmented-scene) :key #'first)
       (bind (segmented-scene 1.0 (cdr (find (path scene-pathname) (get-data ontology 'segmented-scene) :key #'first)))))))
  
  ;; second case; given segmented-scene compute scene-pathname
  ((segmented-scene => scene-pathname)
   (bind (scene-pathname 1.0 (path segmented-scene))))
  
  ;; third case; check consistency
  ((segmented-scene scene-pathname =>)
   (equal (path segmented-scene) scene-pathname))
   :primitive-inventory *symbolic-primitives*)

           