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
;;;;; (c) Tania Marques and Katrien Beuls
(in-package #:asdf)

(defsystem :portuguese-grammar
  :depends-on (:utils :monitors :fcg
               #+:hunchentoot-available-on-this-platform :web-interface)

  :serial t
  :components (;(:file "utils")
               (:file "extended-grammar")
               (:file "construction-templates")
               (:file "create-grammar")
             ;  (:file "unit-tests")
               ))  
            
