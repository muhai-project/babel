;; Copyright AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

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

(in-package :inn)

(defmethod inn-double-click (selection 
                             (network inn:integrative-narrative-network))
  (declare (ignore network))
  (add-element `((script :type "text/javascript")
                 ,(format nil
                          "var selectedNodeId = '~a';
                           if (network.isCluster(selectedNodeId) == true) {
                                network.openCluster(selectedNodeId); }"
                          selection))))

(defmethod inn-right-click ((network integrative-narrative-network))
  (declare (ignore network))
  nil)