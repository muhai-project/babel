(defsystem casa
  :serial t
  :depends-on (:utils :fcg :amr :nlp-tools :cl-json)
  :components ((:file "utils")
               (:file "grammar")
               (:file "de-render")
               )
  :description "A demo FCG grammar for CASA (Herbst and Hoffmann, 2024).")