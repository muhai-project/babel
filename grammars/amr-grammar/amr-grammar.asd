(defsystem amr-grammar
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license ""
  :homepage "https://gitlab.ai.vub.ac.be/ehai/amr-grammar"
  :serial t
  :depends-on (:fcg :amr)
  :components ((:file "package"))
  :description "An FCG grammar for semantic parsing of English utterances into AMR.")