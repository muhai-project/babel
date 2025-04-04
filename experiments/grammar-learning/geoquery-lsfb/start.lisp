(ql:quickload :geoquery-lsfb)
(in-package :geoquery-lsfb)

;(monitors::activate-monitor trace-fcg)

;;------------------------;;
;; + elan-to-predicates + ;;
;;------------------------;;

;; To create a set of predicates from an elan-file (xml), read in
;; the file using read-xml and pass it to elan->predicates

;(pprint (elan->predicates (read-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/43_0_386.eaf")))

;;---------------------------;;
;; + Reading dataset-files + ;;
;;---------------------------;;

;(jsonl->list-of-json-alists "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-250.jsonl")

;;------------------------;;
;; visualizing predicates ;;
;;------------------------;;
;(add-element (predicates->html (elan->predicates (read-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/87_0_801.eaf"))))
;(add-element (predicates->html (elan->predicates (read-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/1_0_1.eaf"))))

;;------------------------------;;
;; + Generating dataset files + ;;
;;------------------------------;;

; global variable determining where the data for the experiment is stored
(defparameter *data-folder*
  "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data")

;; loading in background data needed to generate dataset files
(load-data-for-generation *data-folder*)

;; see dataset-generation script