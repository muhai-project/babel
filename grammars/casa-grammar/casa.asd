(defsystem casa
  :serial t
  :depends-on (:utils :fcg :amr)
  :components ((:file "grammar"))
  :description "A demo FCG grammar for CASA (Herbst and Hoffmann, 2024).")