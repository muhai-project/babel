(in-package :social-network)

;; ------------------
;; + Social Network +
;; ------------------

;; A social network consists of the connections that an agent
;; has to other agents in the network. We imagine all agents
;; forming a circle. Social links can be directed clockwards
;; and counter-clockwards.

(defclass social-network ()
  ((social-network
    :documentation "Neighbours of an agent"
    :type list :accessor social-network :initform nil)))

;; ----------------------------------
;; + Fully Connected Social Network +
;; ----------------------------------

;; By default, we use a fully connected social network in 
;; which each agent can interact with each other agent.

(defun create-fully-connected-network (population &optional reverse-population)
  (if (null population) 
    (reverse reverse-population)
    (progn
      (setf (social-network (first population))
            (append reverse-population (rest population)))
      (create-fully-connected-network (rest population)
                                      (cons (first population) reverse-population)))))

;; --------------------------
;; + Regular Social Network +
;; --------------------------

;; A "regular network" is a network where the node degree 
;; distribution is a constant value for all nodes.

(defun create-regular-population-network (population &optional l)
  "Turn an unstructured population into a regular lattice 
   in which each agent has connections to its l (local-connectivity) nearest neighbors. 
   If l is not specified, create a fully connected network."
  (let ((agents (if (listp population) population (agents population))))
    (if (null l)
      (create-fully-connected-network agents)
      (let* ((population-size (length agents))
             (max-l (floor (/ (- population-size 1) 2)))
             (k (min l max-l)) ;; desired degree
             (cw (floor k 2)) ;; clockwise edges
             (ccw (- k cw))) ;; counterclockwise edges
        (let ((clockwise (copy-list agents))
              (counterclockwise (reverse agents)))
          ;; make lists cyclic:
          (setf (rest (last clockwise)) clockwise
                (rest (last counterclockwise)) counterclockwise)
          ;; define clockwise links:
          (loop for agent in agents
                do (setf (social-network agent) (loop for i from 1 to cw
                                                      collect (nth i clockwise)))
                do (setf clockwise (rest clockwise)))
          ;; add counter-clockwise direction:
          (loop for agent in (reverse agents)
                do (setf (social-network agent) (append (loop for i from 1 to ccw
                                                              collect (nth i counterclockwise))
                                                        (social-network agent)))
                do (setf counterclockwise (rest counterclockwise)))
          agents)))))

;; -----------------------
;; + Small World Network +
;; -----------------------

;; The Small World network model (~ Watts and Strogatz) is 
;; a simple but popular social network structure because it 
;; resembles real-world social networks. We create such
;; networks by rewiring local links in clockwise (outgoing)
;; direction with a long-distance link with probability p.

(defun connected-p (a b)
  (member b (social-network a) :test #'eq))

(defun add-undirected-edge (a b)
  (push b (social-network a))
  (push a (social-network b)))

(defun remove-undirected-edge (a b)
  (setf (social-network a) (remove b (social-network a) :test #'eq))
  (setf (social-network b) (remove a (social-network b) :test #'eq)))

(defun rewire-regular-network (population rewiring-probability)
  "Rewire local links with long-distance links with probability p."
  (loop with agents = (if (listp population) population (agents population))
        with agent-vector = (coerce agents 'vector)
        with n = (length agent-vector)
        for i from 0 below n
        for a = (aref agent-vector i)
        for degree = (length (social-network a))
        for local-connectivity = (floor (/ degree 2))
        ;; k indexes the canonical clockwise neighbours in the ring
        do (loop for k from 1 to local-connectivity 
                 for j = (mod (+ i k) n)
                 for b = (aref agent-vector j)
                 when (and (connected-p a b) (<= (random 1.0) rewiring-probability))
                   ;; pick a new partner c that is not a or any current neighbour of a.
                   do (let* ((forbidden (cons a (social-network a)))
                             (candidates (set-difference agents forbidden :test #'eq)))
                        (when candidates
                          (let ((c (random-elt candidates)))
                            ;; remove the old undirected edge and add the new undirected edge
                            (remove-undirected-edge a b)
                            ;; prevent accidental duplication if c was connected in the meantime
                            (unless (connected-p a c)
                              (add-undirected-edge a c))))))
        finally (return agents)))


;; Documentation:
;; ------------------------------------------------------------------------------------------
;; To exploit the social network of agents, you have to initialize your experiment with 
;; at minimum these functions and configurations:
;;
;;        (initialize-social-network experiment) -> goes through the population to make links
;;        (set-configuration experiment :determine-interacting-agents-mode :random-from-social-network)
;;
;; Without any other configuration, the population will instantiate a fully connected network.
;; This behaviour can change using the following configurations:
;;
;;        (set-configuration experiment :network-topology :normal)
;;        (set-configuration experiment :local-connectivity 3)
;;
;;        -> Whataver the value specified, the code will currently assume that you want either 
;;           a normal network topology, or a small world topology.
;;        -> The configuration :local-connectivity specifies how many links the agent will have 
;;           clockwise in the population as well as counter-clockwise. In other words, if this
;;           configuration is set to 3, the agent will have 6 links with its nearest neighbours
;;           (three nearest to its left, and three nearest to its right).
;;
;; In a normal network, new conventions are expected to diffuse locally first and only then spread 
;; towards the rest of the network in an almost contiguous way. To turn this normal network into 
;; a small-world network, you need to set the rewiring probability to a value between 0.0 and 1.0:
;;
;;        (set-configuration experiment :rewiring-probability 0.3)
;;
;; This is the probability that a local link will be replaced by a long-distance link. These long-
;; distance links allow conventions to make bigger jumps in the network, which may speed up its 
;; diffusion among agents.