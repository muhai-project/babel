;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :asdf)

(pushnew :fcg *features*)

(defsystem :fcg
  :description "All files part of the implementation of Fluid
  Construction Grammar"
  :depends-on (:experiment-framework :test-framework :utils :monitors :meta-layer-learning :cl-store :graph-utils
               #+:hunchentoot-available-on-this-platform :web-interface
               :network
               :s-dot
               :cl-json)
  :serial t
  :components 
  ((:file "package")
   (:file "demo-grammar")
   (:module unify-and-merge
    :serial t
    :components ((:file "matcher")
                 (:file "matcher-extensions")
                 (:file "expansion-operator")
                 (:file "structures")
                 (:file "match-structures")
                 (:file "handle-sequences")))
   (:file "construction")
   (:file "construction-application")
   (:module construction-inventories
    :serial t
    :components ((:file "construction-inventory")
                 (:file "construction-set")
                 (:file "construction-network")
                 (:file "hashed-construction-set")
                 (:file "construction-inventory-collection")))
   (:module parse-and-produce 
    :serial t
    :components ((:file "parse-and-produce")
                 (:file "create-initial-structure")
                 (:file "render")
                 (:file "render-search")
                 (:file "de-render")))
   (:module utils 
    :serial t
    :components ((:file "equivalent-predicate-networks")
                 (:file "utils")))
   (:file "legacy-functions")
   (:module construction-inventory-processor
    :serial t
    :components ((:file "construction-inventory-processor")
                 (:file "cxn-suppliers")
                 (:file "hashed-cxn-suppliers")
                 (:file "node-tests")
                 (:file "goal-tests")))
   (:module heuristic-search
    :serial t
    :components ((:file "heuristic-search")))
   (:module monitoring
    :serial t
    :components ((:file "monitors")
                 #+:hunchentoot-available-on-this-platform 
                 (:file "html")
                 #+:hunchentoot-available-on-this-platform
                 (:file "web-monitors")
                   #+:hunchentoot-available-on-this-platform
                 (:file "visualisation-helpers")))
   (:module check-cxn
    :serial t
    :components ((:file "report")
                 (:file "check-cxn")
                 (:module monitoring
                  :serial t
                  :components ((:file "monitors")
                               #+:hunchentoot-available-on-this-platform
                               (:file "web-monitors")))))
   
   (:module fcg-light
    :serial t
    :components ((:file "utilities")
                 (:file "fcg-light-construction")
                 #+:hunchentoot-available-on-this-platform
                 (:file "html-fcg-light")
                 (:file "monitors-fcg-light")
                 (:file "fcg-light-to-fcg2")
                 (:file "fcg-light-to-latex")
		 (:file "processing-cxn-to-fcg-cxn")))
   
   (:module categorial-networks
    :serial t
    :components ((:file "typed-edge-undirected-graph-class")
                 (:file "graph-utils-additions")
                 (:file "categorial-network")
                 (:file "html")
                 (:file "web-monitor")
                 (:file "export-utils")))
   (:module meta-layer
    :serial t
    :components ((:file "fcg-meta-layer-lib")))
   (:module anti-unification
    :serial t
    :components ((:file "utils")
                 (:module algorithms
                  :serial t
                  :components ((:file "basic-algorithm")
                               (:file "anti-unify-fcg")
                               (:file "anti-unify-fcg-specialise")
                               (:file "pro-unification")
                               (:file "anti-unify-set-of-predicates")
                               (:file "anti-unify-strings")
                               (:file "sequence-alignment")
                               (:file "anti-unify-sequences")))
                 (:file "calculate-source-patterns")
                 (:file "anti-unification-cost")
                 (:file "robust-matching")))
   (:module evaluation
    :serial t
    :components ((:file "measures")
                 (:file "evaluate-grammar")
                 (:file "monitors")))
   (:module constructional-dependencies
    :serial t
    :components ((:file "application-dependencies")
                 (:file "data")
                 (:file "html")))
   (:module grammar-configurator
    :serial t
    :components ((:file "grammar-configurator")
                 (:file "html")
                 (:file "css")
                 (:file "js")))
   (:file "make-json")
   (:module tests
    :serial t
    :components ((:file "helpers")
                 (:file "test-matcher-extensions")
                 (:file "test-render")
                 (:file "test-construction-application")
                 (:file "test-construction-inventory")
                 (:file "test-cip")
                 (:file "test-structures")
                 (:file "test-anti-unification")
                 (:file "test-fcg-light")
                 (:file "test-categorial-networks")
                 (:file "test-form-sequences")
                 (:file "test-sequence-alignment")))))
