;;;;; --------------------------------------------------------------------------------------
;;;;; Copyright: Sony Computer Science Laboratories, Paris Lab
;;;;; Author:    Remi van Trijp
;;;;; 
;;;;; --------------------------------------------------------------------------------------

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

(in-package #:asdf)

(defsystem :geom-world
  :author "Remi van Trijp"
  :version "1.0"
  :description "Package for generating Talking Heads-style scenes of 
                colored geommetric shapes on a white background."
  :depends-on (:utils
               :xmls
               :opticl
               :experiment-framework
               :web-interface
               :irl)
  :serial t
  :components ((:file "package")
               (:file "classes")
               (:file "helper-functions")
               (:file "web-interface")
               (:file "scene-generator")))