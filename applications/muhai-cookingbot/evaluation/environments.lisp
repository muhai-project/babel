;(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulation-environment ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (kitchen-state :type kitchen-state :initarg :kitchen-state :accessor kitchen-state)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (final-node :type irl-program-processor-node :accessor final-node)
   (primary-output-var :type symbol :initarg :primary-output-var :accessor primary-output-var :initform nil)
   (output-node :type irl-program-processor-node :accessor output-node :initform '())
   (execution-time :accessor execution-time :initform '()))
  (:documentation "Class wrapping all information for setting up and evaluating an environment."))

(defmethod initialize-instance :after ((simulation-environment simulation-environment) &key)
  "Execute the simulation environment's network once and already store the solution (to prevent multiple re-executions)."
  (when (meaning-network simulation-environment)
    (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network simulation-environment) nil)))
      (init-kitchen-state simulation-environment)
      (multiple-value-bind (bindings nodes) (evaluate-irl-program extended-mn nil)
        ; store the time it took to execute the whole recipe (i.e., to have all bindings available) 
        (setf (execution-time simulation-environment) (compute-execution-time (first bindings)))
         ; we only expect there to be one solution
        (setf (final-node simulation-environment) (first nodes)))))
  (when (and (final-node simulation-environment) (primary-output-var simulation-environment)) 
    (let ((node (final-node simulation-environment))
          (var-to-find (primary-output-var simulation-environment)))
      (loop for output-var = (second (irl::primitive-under-evaluation node))
            when (eql output-var var-to-find)
              do (setf (output-node simulation-environment) node)
            do (setf node (parent node))
            while (and node (not (output-node simulation-environment)))))))

(defmethod init-kitchen-state ((simulation-environment simulation-environment))
  "Set initial kitchen state to be used in simulation to the one of the given environment."
  (setf *initial-kitchen-state* (kitchen-state simulation-environment)))

(defparameter *almond-crescent-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'butter
                                                                                                     :temperature
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 5))
                                                                                                     :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'pantry
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'vanilla-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list
                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                                  ;; baking equipment
                                                  (make-instance 'baking-tray)
                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter 18 degrees-celsius)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 120 g)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-sugar ?target-container-3 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-almond ?ks-with-almond ?ks-with-vanilla ?target-container-4 almond-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-almond ?target-container-5 all-purpose-flour 340 g)
                       '(fetch-and-proportion ?proportioned-almond-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 almond-flour 120 g)
                       '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 30 g)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-powdered-sugar ?empty-container-a ?warm-butter ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixing-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond ?quantity-d ?unit-d)
                       '(mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool) ; reuse the mixing tool
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-flour ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?intermediate-mixture ?proportioned-almond-flour ?quantity-f ?unit-f)
                       '(mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool) ; reuse the mixing tool
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
                       '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-balls ?bakeable-balls crescent-shape)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-crescents baking-tray 1)
                       '(fetch ?baking-paper ?ks-with-baking-paper ?ks-with-baking-tray baking-paper 1)
                       '(line ?lined-baking-tray ?ks-with-lined-tray ?ks-with-baking-paper ?baking-tray ?baking-paper)
                       '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-lined-tray ?bakeable-crescents ?default-pattern ?lined-baking-tray)
                       '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 15 minute 175 degrees-celsius)
                       '(sprinkle ?almond-crescent-cookies ?ks-with-almond-crescent-cookies ?ks-with-baked-crescents ?baked-crescents ?proportioned-powdered-sugar))
                 :primary-output-var
                 '?almond-crescent-cookies))

(defparameter *afghan-biscuits-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'afghan-biscuits
                 :kitchen-state  (make-instance 
                                  'kitchen-state
                                  :contents
                                  (list (make-instance 'fridge
                                                       :contents (list (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'butter :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 250)))))))
                                        (make-instance 'pantry
                                                       :contents (list (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'caster-sugar :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'cocoa-powder :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'corn-flakes :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'icing-sugar :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'water :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'l)
                                                                                                                                    :quantity (make-instance 'quantity :value 1)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'almond-flakes :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity :value 250)))))))
                                        (make-instance 'kitchen-cabinet
                                                       :contents (list (make-instance 'baking-tray)
                                                                       (make-instance 'baking-paper)
                                                                       (make-instance 'whisk)
                                                                       (make-instance 'whisk)
                                                                       (make-instance 'whisk)
                                                                       (make-instance 'whisk)
                                                                       (make-instance 'whisk)
                                                                       (make-instance 'sift)
                                                                       (make-instance 'wooden-spoon)
                                                                       (make-instance 'wooden-spoon)
                                                                       (make-instance 'wooden-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'rolling-pin)
                                                                       (make-instance 'wire-rack)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       ))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 200 g)
                       '(bring-to-temperature ?butter-at-room-temp ?kitchen-state-with-butter-at-room-temp ?kitchen-state-with-butter ?proportioned-butter 18 degrees-celsius)
                       '(fetch-and-proportion ?proportioned-caster-sugar ?kitchen-state-with-caster-sugar ?kitchen-state-with-butter ?target-container-2 caster-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-all-purpose-flour ?kitchen-state-with-all-purpose-flour ?kitchen-state-with-caster-sugar ?target-container-3 all-purpose-flour 300 g)
                       '(fetch-and-proportion ?proportioned-cocoa-powder ?kitchen-state-with-cocoa-powder ?kitchen-state-with-all-purpose-flour ?target-container-4 cocoa-powder 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-corn-flakes ?kitchen-state-with-corn-flakes ?kitchen-state-with-cocoa-powder ?target-container-5 corn-flakes 300 g)
                       '(fetch-and-proportion ?proportioned-icing-sugar ?kitchen-state-with-icing-sugar ?kitchen-state-with-corn-flakes ?target-container-6 icing-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-icing-cocoa-powder ?kitchen-state-with-icing-cocoa-powder ?kitchen-state-with-icing-sugar ?target-container-7 cocoa-powder 30  g)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-icing-cocoa-powder ?target-container-8 water 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-almonds ?kitchen-state-with-almonds ?kitchen-state-with-water ?target-container-9 almond-flakes 50 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-almonds ?oven 180 degrees-celsius)
                       '(fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-preheated-oven baking-tray 1)
                       '(fetch ?baking-paper ?kitchen-state-with-baking-paper ?kitchen-state-with-baking-tray baking-paper 1)
                       '(line ?lined-baking-tray ?kitchen-state-with-lined-baking-tray ?kitchen-state-with-baking-paper ?baking-tray ?baking-paper)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-lined-baking-tray ?target-container-10 ?butter-at-room-temp ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-caster-sugar ?quantity-y ?unit-y)
                       '(beat ?container-with-creamed-butter ?kitchen-state-with-creamed-butter ?output-kitchen-state-y ?output-container-y ?beating-tool)
                       '(sift ?container-with-sifted-flour ?kitchen-state-with-sifted-flour ?kitchen-state-with-creamed-butter
          ?target-container-11 ?proportioned-all-purpose-flour ?sifting-tool)
                       '(sift ?container-with-sifted-ingredients ?kitchen-state-with-sifted-ingredients ?kitchen-state-with-sifted-flour
          ?container-with-sifted-flour ?proportioned-cocoa-powder ?sifting-tool)
                       '(transfer-contents ?container-with-flour-cocoa-and-butter ?rest-z ?kitchen-state-with-flour-cocoa-and-butter-in-bowl
                       ?kitchen-state-with-sifted-ingredients ?container-with-creamed-butter ?container-with-sifted-ingredients ?quantity-z ?unit-z)
                       '(fetch ?wooden-spoon ?kitchen-state-with-wooden-spoon ?kitchen-state-with-flour-cocoa-and-butter-in-bowl wooden-spoon 1)
                       '(mix ?flour-cocoa-butter-mixture ?kitchen-state-with-flour-cocoa-butter-mixture ?kitchen-state-with-wooden-spoon ?container-with-flour-cocoa-and-butter ?wooden-spoon)
                       '(transfer-contents ?container-with-cornflakes-added ?rest-a ?kitchen-state-with-cornflakes-in-bowl ?kitchen-state-with-flour-cocoa-butter-mixture ?flour-cocoa-butter-mixture ?proportioned-corn-flakes ?quantity-a ?unit-a)
                       '(mix ?flour-cocoa-butter-cornflakes-mixture ?kitchen-state-with-cornflakes-mixture ?kitchen-state-with-cornflakes-in-bowl ?container-with-cornflakes-added ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions-on-countertop ?kitchen-state-with-cornflakes-mixture ?flour-cocoa-butter-cornflakes-mixture 30 g ?default-pattern ?countertop)
                       '(shape ?dough-balls ?kitchen-state-with-doughballs ?kitchen-state-with-portions-on-countertop ?portioned-dough ball-shape)
                       '(flatten ?flattened-dough-balls ?kitchen-state-with-flattened-doughballs ?kitchen-state-with-doughballs ?dough-balls ?rolling-pin)
                       '(transfer-items ?cookies-on-tray ?kitchen-state-with-cookies-on-tray ?kitchen-state-with-flattened-doughballs ?flattened-dough-balls 5-cm-apart ?lined-baking-tray)
                       '(bake ?baked-cookies ?kitchen-state-with-baking-cookies ?kitchen-state-with-cookies-on-tray ?cookies-on-tray ?preheated-oven 15 minute ?temp-quantity ?temp-unit)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-baking-cookies wire-rack 1)
                       '(transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?baked-cookies ?default-pattern-2 ?wire-rack)
                       '(bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack 18 degrees-celsius)
                       '(fetch ?medium-bowl ?kitchen-state-with-bowl ?kitchen-state-with-cooling-cookies medium-bowl 1)
                       '(transfer-contents ?container-for-icing-with-sugar ?rest-b ?kitchen-state-with-container-for-icing-with-sugar ?kitchen-state-with-bowl ?medium-bowl ?proportioned-icing-sugar ?quantity-b ?unit-b)
                       '(transfer-contents ?container-for-icing-with-sugar-and-cocoa ?rest-c ?kitchen-state-with-container-for-icing-with-sugar-and-cocoa ?kitchen-state-with-container-for-icing-with-sugar ?container-for-icing-with-sugar ?proportioned-icing-cocoa-powder ?quantity-c ?unit-c)
                       '(transfer-contents ?container-for-icing-with-sugar-cocoa-and-water ?rest-d ?kitchen-state-with-container-for-icing-with-sugar-cocoa-and-water ?kitchen-state-with-container-for-icing-with-sugar-and-cocoa ?container-for-icing-with-sugar ?proportioned-water ?quantity-d ?unit-d)
                       '(mix ?icing ?kitchen-with-icing-ready ?kitchen-state-with-container-for-icing-with-sugar-cocoa-and-water ?container-for-icing-with-sugar-cocoa-and-water ?mixing-tool)
                       '(fetch ?table-spoon ?kitchen-state-with-table-spoon ?kitchen-with-icing-ready table-spoon 1)
                       '(spread ?iced-cookies ?kitchen-state-with-iced-cookies ?kitchen-state-with-table-spoon  ?cooled-cookies ?icing ?table-spoon)
                       '(sprinkle ?sprinkled-cookies ?kitchen-state-with-sprinkled-cookies ?kitchen-state-with-iced-cookies ?iced-cookies ?proportioned-almonds))
                 :primary-output-var '?sprinkled-cookies))

(defparameter *best-brownies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'best-brownies
                 :kitchen-state (make-instance 'kitchen-state
                                 :contents
                                 (list (make-instance 'fridge
                                                      :contents (list (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'butter
                                                                                                                    :temperature
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'degrees-celsius)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 5))
                                                                                                                    :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 500)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'egg
                                                                                                                    :temperature
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'degrees-celsius)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 5))
                                                                                                                    :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'piece)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 12)))))))
                                       (make-instance 'pantry
                                                      :contents (list (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'white-sugar :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 1000)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'vanilla-extract :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 100)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'chopped-walnut :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 100)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 1000)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'cocoa-powder :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 500)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'salt :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 500)))))))
                                       (make-instance 'kitchen-cabinet
                                                      :contents (list
                                                                 ;; bowls
                                                                 (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                                 ;; tools
                                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                 (make-instance 'spatula) (make-instance 'knife)

                                                                 ;; baking equipment
                                                                 (make-instance 'baking-tray)
                                                                 (make-instance 'pan)
                                                                 (make-instance 'baking-paper)))))
                                  :meaning-network
                                  (list '(get-kitchen ?kitchen-state)
                                        '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?new-container-1 butter 120 g)
                                        '(melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter ?microwave)
                                        '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter ?new-container-2 white-sugar 200 g)
                                        '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?new-container-3 egg 2 piece)
                                        '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs ?new-container-4 all-purpose-flour 70 g)
                                        '(fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour ?new-container-5 cocoa-powder 45 g)
                                        '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa ?new-container-6 salt 0.25 teaspoon)
                                        '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt ?new-container-7 vanilla-extract 1 teaspoon)
                                        '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla ?new-container-8 chopped-walnut 50 g)
                                        '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-walnuts ?oven 175 degrees-celsius)
                                        '(fetch ?pan ?kitchen-state-with-pan ?kitchen-state-with-preheated-oven pan 1)
                                        '(grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-pan ?pan ?grease)
                                        '(flour ?floured-pan ?kitchen-state-with-floured-pan ?kitchen-state-with-greased-pan ?greased-pan ?all-purpose-flour)
                                        '(fetch ?medium-bowl-1 ?kitchen-state-with-medium-bowl ?kitchen-state-with-floured-pan medium-bowl 1)
                                        '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-medium-bowl ?medium-bowl-1 ?melted-butter ?quantity-x ?unit-x)
                                        '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
                                        '(beat ?beaten-mixture-bowl ?kitchen-state-with-beaten-mixture ?output-kitchen-state-y ?output-container-y ?beating-tool)
                                        '(crack ?mixture-with-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-beaten-mixture ?proportioned-eggs ?beaten-mixture-bowl)
                                        '(mix ?egg-sugar-mixture ?kitchen-state-with-egg-sugar-mixture ?kitchen-state-with-cracked-eggs ?mixture-with-cracked-eggs ?beating-tool)
                                        '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?kitchen-state-with-egg-sugar-mixture ?egg-sugar-mixture ?proportioned-flour ?quantity-z ?unit-z)
                                        '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?output-kitchen-state-z ?output-container-z ?proportioned-cocoa ?quantity-a ?unit-a)
                                        '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-salt ?quantity-b ?unit-b)
                                        '(mix ?flour-sugar-mixture-bowl ?kitchen-state-with-flour-sugar-mixture ?output-kitchen-state-b ?output-container-b ?beating-tool)
                                        '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?kitchen-state-with-flour-sugar-mixture ?flour-sugar-mixture-bowl ?proportioned-vanilla ?quantity-c ?unit-c)
                                        '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-walnuts ?quantity-d ?unit-d)
                                        '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?output-container-d ?beating-tool)
                                        '(spread ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?floured-pan ?dough ?scraper)
                                        '(bake ?baked-brownie ?kitchen-state-with-baked-brownie ?kitchen-state-with-dough-in-pan ?pan-with-dough ?preheated-oven 25 minute ?temp-quantity ?temp-unit)
                                        '(bring-to-temperature ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-baked-brownie ?baked-brownie 18 degrees-celsius)
                                        '(cut ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie squares ?knife))
                                  :primary-output-var '?cut-brownie))

(defparameter *chocolate-fudge-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-fudge-cookies
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents   (list (make-instance 'fridge
                                                   :contents (list (make-instance 'medium-bowl
                                                                                  :contents (list (make-instance 'butter
                                                                                                                 :temperature
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'degrees-celsius)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 5))
                                                                                                                 :amount
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'g)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 500)))))
                                                                   (make-instance 'medium-bowl
                                                                                  :contents (list (make-instance 'egg
                                                                                                                 :temperature
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'degrees-celsius)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 5))
                                                                                                                 :amount
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'piece)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 12)))))))
                                    (make-instance 'pantry
                                                   :contents (list (make-instance 'medium-bowl
                                                                                  :contents (list (make-instance 'devils-food-cake-mix :amount
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'g)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 517)))))
                                                                   (make-instance 'medium-bowl
                                                                                  :contents (list (make-instance 'vegetable-oil :amount
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'g)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 200)))))
                                                                   (make-instance 'medium-bowl
                                                                                  :contents (list (make-instance 'semisweet-chocolate-chips :amount
                                                                                                                 (make-instance 'amount
                                                                                                                                :unit (make-instance 'g)
                                                                                                                                :quantity (make-instance 'quantity
                                                                                                                                                         :value 250)))))))
                                    (make-instance 'kitchen-cabinet
                                                   :contents (list
                                                              ;; bowls
                                                              (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                              ;; tools
                                                              (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                              (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                              (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                              (make-instance 'spatula) (make-instance 'knife)

                                                              ;; baking equipment
                                                              (make-instance 'wire-rack)
                                                              (make-instance 'baking-tray)
                                                              (make-instance 'cookie-sheet)
                                                              (make-instance 'pan)
                                                              (make-instance 'baking-paper)))))
                                  :meaning-network
                                  (list '(get-kitchen ?kitchen-state)
                                        '(fetch-and-proportion ?proportioned-devils-food-cake-mix ?kitchen-state-with-devils-food-cake-mix ?kitchen-state ?target-container-1 devils-food-cake-mix 500 g)
                                        '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-devils-food-cake-mix ?target-container-2 egg 2 piece)
                                        '(fetch-and-proportion ?proportioned-vegetable-oil ?kitchen-state-with-vegetable-oil ?kitchen-state-with-eggs ?target-container-3 vegetable-oil 125 ml)
                                        '(fetch-and-proportion ?proportioned-semisweet-chocolate-chips ?kitchen-state-with-semisweet-chocolate-chips ?kitchen-state-with-vegetable-oil ?target-container-4 semisweet-chocolate-chips 160 g)
                                        '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-semisweet-chocolate-chips ?oven 175 degrees-celsius)
                                        '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1)
                                        '(grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)
                                        '(fetch ?medium-bowl-1 ?kitchen-state-with-medium-bowl ?kitchen-state-with-greased-sheet medium-bowl 1)
                                        '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-medium-bowl ?medium-bowl-1 ?proportioned-devils-food-cake-mix ?quantity-x ?unit-x)
                                        '(crack ?output-container-y ?output-kitchen-state-y ?output-kitchen-state-x ?proportioned-eggs ?medium-bowl-1)
                                        '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-vegetable-oil ?quantity-z ?unit-z)
                                        '(mix ?stirred-mixture-bowl ?kitchen-state-with-stirred-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)
                                        '(transfer-contents ?output-container-with-chips ?rest-chips ?kitchen-state-with-folded-chips ?kitchen-state-with-stirred-mixture ?stirred-mixture-bowl ?proportioned-semisweet-chocolate-chips ?quantity-chips ?unit-chips)
                                        '(mix ?chips-mixture-bowl ?kitchen-state-with-chips-mixture ?output-kitchen-state-z ?output-container-with-chips ?mixing-tool)
                                        '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-chips-mixture ?chips-mixture-bowl 20 g ?default-pattern ?countertop)
                                        '(shape ?shaped-bakeables ?ks-with-dough-balls ?kitchen-state-with-portions ?portioned-dough ball-shape)
                                        '(transfer-items ?cookies-on-sheet ?ks-with-dough-on-sheet ?ks-with-dough-balls ?shaped-bakeables 5-cm-apart ?greased-sheet)
                                        '(bake ?baked-cookies-on-sheet ?kitchen-state-with-cookies ?ks-with-dough-on-sheet ?cookies-on-sheet ?preheated-oven 8 minute ?bake-quantity ?bake-unit)
                                        '(cool-for-time ?cooling-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies ?baked-cookies-on-sheet 5 minute)
                                        '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-cookies wire-rack 1)
                                        '(transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?cooling-cookies ?default-pattern-2 ?wire-rack)
                                        '(bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooled-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack 18 degrees-celsius))
                                  :primary-output-var '?cooled-cookies))

(defparameter *easy-banana-bread-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-banana-bread
                 :kitchen-state   (make-instance
                                   'kitchen-state
                                   :contents
                                   (list (make-instance 'fridge
                                                        :contents (list (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'butter
                                                                                                                      :temperature
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 5))
                                                                                                                      :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 500)))))
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'egg
                                                                                                                      :temperature
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 5))
                                                                                                                      :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'piece)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 12)))))))
                                         (make-instance 'pantry
                                                        :contents (list (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'white-sugar :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 1000)))))
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'banana :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'piece)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 6)))))
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 100)))))
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'self-rising-flour :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 1000)))))
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 1000)))))
                                        
                                                                        (make-instance 'medium-bowl
                                                                                       :contents (list (make-instance 'salt :amount
                                                                                                                      (make-instance 'amount
                                                                                                                                     :unit (make-instance 'g)
                                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                                              :value 500)))))))
                                         (make-instance 'kitchen-cabinet
                                                        :contents (list
                                                                   ;; bowls
                                                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                                   ;; tools
                                                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                   (make-instance 'fork) (make-instance 'whisk) (make-instance 'whisk)
                                                                   (make-instance 'spatula) (make-instance 'knife)

                                                                   ;; baking equipment
                                                                   (make-instance 'pan)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 60 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-butter ?target-container-2 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-eggs ?target-container-3 sugar 200 g)
                       '(fetch-and-proportion ?proportioned-bananas ?kitchen-state-with-bananas ?kitchen-state-with-sugar ?target-container-4 banana 3 piece)
                       '(mash ?mashed-bananas ?kitchen-state-with-mashed-bananas ?kitchen-state-with-bananas ?proportioned-bananas ?fork)
                       '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-mashed-bananas ?target-container-5 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-self-rising-flour ?kitchen-state-with-self-rising-flour ?kitchen-state-with-vanilla ?target-container-6 self-rising-flour 200 g)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-self-rising-flour ?target-container-7 ?proportioned-butter ?quantity-x ?unit-x)
                       '(crack ?output-container-y ?output-kitchen-state-y ?output-kitchen-state-x ?proportioned-eggs ?output-container-x)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-sugar ?quantity-z ?unit-z)
                       '(beat ?creamed-mixture ?kitchen-state-with-creamed-mixture ?output-kitchen-state-z ?output-container-z ?beating-tool)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-creamed-mixture ?creamed-mixture ?mashed-bananas ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
                       '(beat ?beaten-mixture ?kitchen-state-with-beaten-mixture ?output-kitchen-state-b ?output-container-b ?beating-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?kitchen-state-with-beaten-mixture ?beaten-mixture ?proportioned-self-rising-flour ?quantity-c ?unit-c)
                       '(mix ?banana-bread-batter ?kitchen-state-with-banana-bread-batter ?output-kitchen-state-c ?output-container-c ?beating-tool)
                       '(fetch ?pan ?kitchen-state-with-pan ?kitchen-state-with-banana-bread-batter pan 1)
                       '(grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-pan ?pan ?grease)
                       '(spread ?pan-with-batter ?kitchen-state-with-batter-in-pan ?kitchen-state-with-greased-pan ?greased-pan ?banana-bread-batter ?scraper)
                       '(bake ?baked-banana-bread ?kitchen-state-with-baked-banana-bread ?kitchen-state-with-batter-in-pan ?pan-with-batter ?oven 60 minute 165 degrees-celsius))
                 :primary-output-var '?baked-banana-bread))

(defparameter *easy-oatmeal-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-oatmeal-cookies
                 :kitchen-state  (make-instance
                                  'kitchen-state
                                  :contents
                                  (list (make-instance 'fridge
                                                       :contents (list (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'butter
                                                                                                                     :temperature
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 5))
                                                                                                                     :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'water
                                                                                                                     :temperature
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 5))
                                                                                                                     :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'ml)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'egg
                                                                                                                     :temperature
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 5))
                                                                                                                     :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'piece)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 12)))))))
                                        (make-instance 'pantry
                                                       :contents (list (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'raisin :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 200)))))
                                                                       (make-instance 'large-bowl
                                                                                      :contents (list (make-instance 'vegetable-oil :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'ml)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))
                                                   
                                        
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'brown-sugar :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 1000)))))

                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'oats :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'chopped-walnut :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'baking-soda :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 50)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'vanilla-extract :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 50)))))
                                                                       (make-instance 'medium-bowl
                                                                                      :contents (list (make-instance 'salt :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 500)))))
                                                                       (make-instance 'small-bowl
                                                                                      :contents (list (make-instance 'ground-cinnamon :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 50)))))
                                                                       (make-instance 'small-bowl
                                                                                      :contents (list (make-instance 'ground-nutmeg :amount
                                                                                                                     (make-instance 'amount
                                                                                                                                    :unit (make-instance 'g)
                                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                                             :value 50)))))))
                                        (make-instance 'kitchen-cabinet
                                                       :contents (list
                                                                  ;; bowls
                                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)


                                                                  ;; tools
                                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                  (make-instance 'fork) (make-instance 'whisk) (make-instance 'whisk)
                                                                  (make-instance 'spatula) (make-instance 'knife) (make-instance 'sift)

                                                                  ;; baking equipment
                                                                  (make-instance 'baking-tray)
                                                                  (make-instance 'cookie-sheet)
                                                                  (make-instance 'pan)
                                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-raisins ?kitchen-state-with-raisins ?kitchen ?target-container-1 raisin 150 g)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-raisins ?target-container-2 water 125 ml)
                       '(bring-to-temperature ?hot-water ?kitchen-state-with-hot-water ?kitchen-state-with-water ?proportioned-water 60 degrees-celsius)
                       '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-hot-water ?target-container-3 all-purpose-flour 280 g)
                       '(fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-flour ?target-container-4 baking-soda 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-baking-soda ?target-container-5 salt 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-oats ?kitchen-state-with-oats ?kitchen-state-with-salt ?target-container-6 oats 160 g)
                       '(fetch-and-proportion ?proportioned-cinnamon ?kitchen-state-with-cinnamon ?kitchen-state-with-oats ?target-container-7 ground-cinnamon 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-nutmeg ?kitchen-state-with-nutmeg ?kitchen-state-with-cinnamon ?target-container-8 ground-nutmeg 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-nutmeg ?target-container-9 brown-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-sugar ?target-container-10 chopped-walnut 75 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-walnuts ?target-container-11 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-oil ?kitchen-state-with-oil ?kitchen-state-with-eggs ?target-container-12 vegetable-oil 200 ml)
                       '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-oil ?target-container-13 vanilla-extract 1 teaspoon)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheating-oven ?kitchen-state-with-vanilla ?oven 175 degrees-celsius)
                       '(transfer-contents ?container-with-soaked-raisins ?empty-raisin-bowl ?kitchen-state-with-soaking-raisins ?kitchen-state-with-preheating-oven ?hot-water ?proportioned-raisins ?quantity ?unit)
                       '(transfer-contents ?flour-with-soda ?empty-soda-bowl ?kitchen-state-with-flour-and-soda ?kitchen-state-with-soaking-raisins ?proportioned-flour ?proportioned-baking-soda ?quantity-1 ?unit-1)
                       '(transfer-contents ?flour-with-soda-and-salt ?empty-salt-bowl ?kitchen-state-with-flour-soda-and-salt ?kitchen-state-with-flour-and-soda ?flour-with-soda ?proportioned-salt ?quantity-2 ?unit-2)
                       '(transfer-contents ?flour-soda-salt-cinnamon ?empty-cinnamon-bowl ?kitchen-state-with-flour-soda-salt-cinnamon ?kitchen-state-with-flour-soda-and-salt ?flour-with-soda-and-salt ?proportioned-cinnamon ?quantity-3 ?unit-3)
                       '(transfer-contents ?flour-soda-salt-cinnamon-nutmeg ?empty-nutmeg-bowl ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg ?kitchen-state-with-flour-soda-salt-cinnamon ?flour-soda-salt-cinnamon ?proportioned-nutmeg ?quantity-4 ?unit-4)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-bowl-for-sifting ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg large-bowl 1)
                       '(sift ?bowl-with-sifted-ingredients ?kitchen-state-after-sifting ?kitchen-state-with-fetched-bowl-for-sifting ?large-bowl ?flour-soda-salt-cinnamon-nutmeg ?sifting-tool)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-after-sifting ?bowl-with-sifted-ingredients ?proportioned-oats ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-walnuts ?quantity-z ?unit-z)
                       '(mix ?blended-in-oats-mixture ?kitchen-state-with-blended-oats-in-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)
                       '(fetch ?bowl-for-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?kitchen-state-with-blended-oats-in-mixture medium-bowl 1)
                       '(crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?proportioned-eggs ?bowl-for-eggs)
                       '(fetch ?fetched-fork ?kitchen-state-with-fetched-fork ?kitchen-state-with-cracked-eggs fork 1)
                       '(beat ?container-w-beaten-eggs ?kitchen-state-w-beaten-eggs ?kitchen-state-with-fetched-fork ?container-w-cracked-eggs ?fetched-fork)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-w-beaten-eggs ?container-w-beaten-eggs ?proportioned-oil ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
                       '(transfer-contents ?container-w-eggs-oil-vanilla-raisins ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?container-with-soaked-raisins ?quantity-c ?unit-c)
                       '(transfer-contents ?container-with-flour-and-mixture ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?flour-soda-salt-cinnamon-nutmeg ?container-w-eggs-oil-vanilla-raisins ?quantity-d ?unit-d)
                       '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?container-with-flour-and-mixture ?mixing-tool)
                       '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-dough cookie-sheet 1)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-cookie-sheet ?dough 5 g ?pattern ?countertop)
                       '(transfer-items ?sheet-with-dough ?kitchen-state-with-dough-on-sheet ?kitchen-state-with-portions ?portioned-dough 5-cm-apart ?cookie-sheet)
                       '(bake ?baked-cookies ?kitchen-state-with-baked-cookies ?kitchen-state-with-dough-on-sheet ?sheet-with-dough ?preheated-oven 10 minute ?bake-quantity ?bake-unit))
                 :primary-output-var '?baked-cookies))

(defparameter *whole-wheat-ginger-snaps-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'whole-wheat-ginger-snaps
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'butter
                                                                                                     :temperature
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 5))
                                                                                                     :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 250)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'egg
                                                                                                     :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'piece)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 2)))))))
                        (make-instance 'pantry
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'whole-wheat-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'baking-soda :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 250)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'baking-powder :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 250)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'ground-ginger :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 50)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'ground-nutmeg :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 50)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'ground-cinnamon :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 50)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'ground-cloves :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 50)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'ground-allspice :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 50)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'molasses :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 900)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list
                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   
                                                  ;; baking equipment
                                                  (make-instance 'wire-rack)
                                                  (make-instance 'baking-tray)
                                                  (make-instance 'cookie-sheet)
                                                  (make-instance 'pan)
                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 225 g)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-butter ?target-container-2 white-sugar 300 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?target-container-3 egg 2 piece)
                       '(crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-eggs ?proportioned-eggs ?bowl-for-eggs)
                       '(beat ?beaten-eggs ?kitchen-state-with-beaten-eggs ?kitchen-state-with-cracked-eggs ?container-w-cracked-eggs ?beating-tool)
                       '(fetch-and-proportion ?proportioned-molasses ?kitchen-state-with-molasses ?kitchen-state-with-beaten-eggs ?target-container-4 molasses 280 g)
                       '(fetch-and-proportion ?proportioned-whole-wheat-flour ?kitchen-state-with-whole-wheat-flour ?kitchen-state-with-molasses ?target-container-5  whole-wheat-flour 480 g)
                       '(fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-whole-wheat-flour ?target-container-6 baking-soda 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-baking-powder ?kitchen-state-with-baking-powder ?kitchen-state-with-baking-soda ?target-container-7 baking-powder 2 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-ginger ?kitchen-state-with-ground-ginger ?kitchen-state-with-baking-powder ?target-container-8 ground-ginger 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-ground-nutmeg ?kitchen-state-with-ground-nutmeg ?kitchen-state-with-ground-ginger ?target-container-9  ground-nutmeg 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-cinnamon ?kitchen-state-with-ground-cinnamon  ?kitchen-state-with-ground-nutmeg ?target-container-10  ground-cinnamon 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-cloves ?kitchen-state-with-ground-cloves ?kitchen-state-with-ground-cinnamon ?target-container-11  ground-cloves 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-allspice ?kitchen-state-with-ground-allspice ?kitchen-state-with-ground-cloves ?target-container-12  ground-allspice 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-decoration-sugar ?kitchen-state-with-sugar-for-decoration ?kitchen-state-with-ground-allspice ?target-container-13  white-sugar 300 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-sugar-for-decoration ?oven 175 degrees-celsius)
                       '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1)
                       '(grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-bowl ?kitchen-state-with-greased-sheet large-bowl 1)
                       '(transfer-contents ?butter-in-large-bowl ?empty-butter-bowl ?kitchen-state-with-butter-in-large-bowl ?kitchen-state-with-fetched-bowl ?large-bowl ?proportioned-butter ?quantity-butter ?unit-butter)
                       '(transfer-contents ?butter-sugar-bowl ?empty-sugar-bowl ?kitchen-state-with-butter-sugar-mix ?kitchen-state-with-butter-in-large-bowl ?butter-in-large-bowl ?proportioned-sugar ?quantity-sugar ?unit-sugar)
                       '(mix ?butter-sugar-cream ?kitchen-state-with-creamed-mix ?kitchen-state-with-butter-sugar-mix ?butter-sugar-bowl ?mixing-tool)
                       '(transfer-contents ?mix-and-eggs ?empty-egg-bowl ?kitchen-state-with-eggs-in-mix ?kitchen-state-with-creamed-mix ?butter-sugar-cream ?beaten-eggs ?quantity-eggs ?unit-eggs)
                       '(transfer-contents ?mix-eggs-and-molasses ?empty-molasses-bowl ?kitchen-state-with-molasses-in-mix ?kitchen-state-with-eggs-in-mix ?mix-and-eggs ?proportioned-molasses ?quantity-molasses ?unit-molasses)
                       '(mix ?cream-eggs-molasses-mix ?kitchen-state-with-cream-eggs-molasses-mix ?kitchen-state-with-molasses-in-mix ?mix-eggs-and-molasses ?beating-tool)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-cream-eggs-molasses-mix ?empty-container ?proportioned-whole-wheat-flour ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-baking-soda ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-baking-powder ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-ground-ginger ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-ground-nutmeg ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?proportioned-ground-cinnamon ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?proportioned-ground-cloves ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?proportioned-ground-allspice ?quantity-h ?unit-h)
                       '(mix ?dry-mixture ?kitchen-state-with-dry-mixture ?output-kitchen-state-h ?output-container-h ?mixing-tool)
                       '(transfer-contents ?molasses-dry-mixture ?rest-molasses-dry-mix ?kitchen-state-with-molasses-dry-mixture ?kitchen-state-with-dry-mixture ?cream-eggs-molasses-mix ?dry-mixture ?quantity-stir ?unit-sir)
                       '(mix ?dough ?kitchen-state-with-dough ?kitchen-state-with-molasses-dry-mixture ?molasses-dry-mixture ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?shaped-bakeables ?kitchen-state-with-shaped-bakeables ?kitchen-state-with-portions ?portioned-dough ball-shape)
                       '(dip ?dipped-bakeables ?kitchen-state-with-dipped-bakeables ?kitchen-state-with-shaped-bakeables ?shaped-bakeables ?proportioned-decoration-sugar)
                       '(transfer-items ?bakeables-on-sheet ?kitchen-out-bakeables-on-sheet ?kitchen-state-with-dipped-bakeables ?dipped-bakeables 5-cm-apart ?greased-sheet)
                       '(bake ?baked-snaps ?kitchen-out-with-baked-snaps ?kitchen-out-bakeables-on-sheet ?bakeables-on-sheet ?preheated-oven 10 minute ?preheated-quantity ?preheated-unit)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-out-with-baked-snaps wire-rack 1)
                       '(transfer-items ?snaps-on-wire-rack ?kitchen-state-with-snaps-on-wire-rack ?kitchen-state-with-wire-rack ?baked-snaps ?default-pattern ?wire-rack)
                       '(bring-to-temperature ?cooled-snaps ?kitchen-state-with-cooled-snaps ?kitchen-state-with-snaps-on-wire-rack ?snaps-on-wire-rack 18 degrees-celsius))
                 :primary-output-var '?cooled-snaps))

(defparameter *cucumber-slices-with-dill-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'cucumber-slices-with-dill
                 :kitchen-state
                 (make-instance 
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'cherry-tomato :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'cucumber :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'piece)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 10)))))))
                        (make-instance 'pantry
                                       :contents (list

                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'onion :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'piece)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 10)))))
                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'dried-dill-weed :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'white-sugar :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'white-vinegar :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                          
                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'water :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                       
                                                  (make-instance 'medium-bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'salt :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list

                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; bowl-lids
                                                  (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                                  (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                                  (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cucumbers ?kitchen-state-with-cucumbers ?kitchen ?target-container-1 cucumber 4 piece)
                       '(cut ?sliced-cucumbers ?kitchen-state-with-sliced-cucumbers ?kitchen-state-with-cucumbers ?proportioned-cucumbers slices ?knife)
                       '(fetch-and-proportion ?proportioned-onions ?kitchen-state-with-onions ?kitchen-state-with-sliced-cucumbers ?target-container-2 onion 1 piece)
                       '(cut ?sliced-onions ?kitchen-state-with-sliced-onions ?kitchen-state-with-onions ?proportioned-onions slices ?knife)
                       '(fetch-and-proportion ?proportioned-dill-weed ?kitchen-state-with-dill-weed ?kitchen-state-with-sliced-onions ?target-container-3 dried-dill-weed 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-dill-weed ?target-container-4 white-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-white-vinegar ?kitchen-state-with-white-vinegar ?kitchen-state-with-white-sugar ?target-container-5 white-vinegar 120 ml)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-white-vinegar ?target-container-6 water 120 ml)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-water ?target-container-7 salt 1 teaspoon)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-salt large-bowl 1)
                       '(transfer-contents ?output-a ?rest-a ?kitchen-out-a ?kitchen-state-with-fetched-large-bowl ?large-bowl ?proportioned-cucumbers ?quantity-a ?unit-a)
                       '(transfer-contents ?output-b ?rest-b ?kitchen-out-b ?kitchen-out-a ?output-a ?proportioned-onions ?quantity-b ?unit-b)
                       '(transfer-contents ?output-c ?rest-c ?kitchen-out-c ?kitchen-out-b ?output-b ?proportioned-dill-weed ?quantity-c ?unit-c)
                       '(mingle ?cucumber-mixture ?kitchen-state-with-cucumber-mixture ?kitchen-out-c ?output-c ?mingling-tool)
                       '(fetch ?medium-bowl ?kitchen-state-with-fetched-medium-bowl ?kitchen-state-with-cucumber-mixture medium-bowl 1)
                       '(transfer-contents ?output-d ?rest-d ?kitchen-out-d ?kitchen-state-with-fetched-medium-bowl ?medium-bowl ?proportioned-white-sugar ?quantity-d ?unit-d)
                       '(transfer-contents ?output-e ?rest-e ?kitchen-out-e ?kitchen-out-d ?output-d ?proportioned-white-vinegar ?quantity-e ?unit-e)
                       '(transfer-contents ?output-f ?rest-f ?kitchen-out-f ?kitchen-out-e ?output-e ?proportioned-water ?quantity-f ?unit-f)
                       '(transfer-contents ?output-g ?rest-g ?kitchen-out-g ?kitchen-out-f ?output-f ?proportioned-salt ?quantity-g ?unit-g)
                       '(mix ?liquid-mixture ?kitchen-state-with-liquid-mixture ?kitchen-out-g ?output-g ?mixing-tool)
                       '(transfer-contents ?output-h ?rest-h ?kitchen-out-h ?kitchen-state-with-liquid-mixture ?cucumber-mixture ?liquid-mixture ?quantity-pour ?unit-pour)
                       '(cover ?covered-mixture ?kitchen-state-with-covered-mixture ?kitchen-out-h ?output-h ?bowl-lid)
                       '(refrigerate ?cooled-mixture ?kitchen-state-with-cooled-mixture ?kitchen-state-with-covered-mixture ?covered-mixture ?fridge 2 hour)
                       '(uncover ?served-salad ?cover ?kitchen-state-with-served-salad ?kitchen-state-with-cooled-mixture ?cooled-mixture))
                 :primary-output-var '?served-salad))

(defparameter *easy-cherry-tomato-corn-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-cherry-tomato-corn-salad
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'cherry-tomato :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))

                                                       (make-instance 'bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'cucumber :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))
                                                       (make-instance 'bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'onion :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'piece)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 10)))))))
                        (make-instance 'freezer
                                       :contents (list
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'frozen-corn :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))))
                        (make-instance 'pantry
                                       :contents (list
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'fresh-basil :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'white-sugar :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'olive-oil :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                          
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'ground-black-pepper :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'shallot :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'piece)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 5)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'jalapeno :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'piece)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 5)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'lime-juice :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))
                                                  (make-instance 'bowl
                                                                 :used T
                                                                 :contents (list (make-instance 'salt :amount
                                                                                                (make-instance 'amount
                                                                                                               :unit (make-instance 'g)
                                                                                                               :quantity (make-instance 'quantity
                                                                                                                                        :value 500)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list

                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; bowl-lids
                                                  (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                                  (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                                  ;; jars
                                                  (make-instance 'jar) (make-instance 'jar) (make-instance 'jar)

                                                  ;; jar-lids
                                                  (make-instance 'jar-lid) (make-instance 'jar-lid) (make-instance 'jar-lid)

                                                  ;; wrapping
                                                  (make-instance 'plastic-wrap)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                                  (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)))))
         
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-basil ?kitchen-state-with-basil ?kitchen ?target-container-1 fresh-basil 5 g)
                       '(cut ?minced-basil ?kitchen-state-with-cut-basil ?kitchen-state-with-basil ?proportioned-basil minced ?knife)
                       '(fetch-and-proportion ?olive-oil ?kitchen-state-with-olive-oil ?kitchen-state-with-cut-basil ?target-container-2 olive-oil 3 tablespoon)
                       '(fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-olive-oil ?target-container-3 lime-juice 2 teaspoon)
                       '(fetch-and-proportion ?white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-lime-juice ?target-container-4 white-sugar 1 teaspoon)
                       '(fetch-and-proportion ?salt ?kitchen-state-with-salt ?kitchen-state-with-white-sugar ?target-container-5 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 0.25 teaspoon)
                       '(fetch-and-proportion ?frozen-corn ?kitchen-state-with-frozen-corn ?kitchen-state-with-pepper ?target-container-7 frozen-corn 350 g)
                       '(bring-to-temperature ?thawed-corn ?kitchen-state-with-thawed-corn ?kitchen-state-with-frozen-corn ?frozen-corn 18 degrees-celsius)
                       '(fetch-and-proportion ?cherry-tomatoes ?kitchen-state-with-cherry-tomatoes ?kitchen-state-with-thawed-corn ?target-container-8 cherry-tomato 300 g)
                       '(cut ?cut-tomatoes ?kitchen-state-with-cut-tomatoes ?kitchen-state-with-cherry-tomatoes ?cherry-tomatoes halved ?knife)
                       '(fetch-and-proportion ?cucumber ?kitchen-state-with-cucumber ?kitchen-state-with-cut-tomatoes ?target-container-9 cucumber 160 g)
                       '(peel ?peeled-cucumber ?cucumber-peels ?kitchen-state-with-peeled-cucumber ?kitchen-state-with-cucumber ?cucumber ?knife)
                       '(seed ?seeded-cucumber ?cucumber-seeds ?kitchen-state-with-seeded-cucumber ?kitchen-state-with-peeled-cucumber ?peeled-cucumber ?knife)
                       '(cut ?chopped-cucumber ?kitchen-state-with-chopped-cucumber ?kitchen-state-with-seeded-cucumber ?seeded-cucumber slices ?knife)
                       '(fetch-and-proportion ?jalapeno ?kitchen-state-with-jalapeno  ?kitchen-state-with-chopped-cucumber ?target-container-10 jalapeno 1 piece)
                       '(seed ?seeded-jalapeno ?jalapeno-seeds ?kitchen-state-with-seeded-jalapeno ?kitchen-state-with-jalapeno ?jalapeno ?knife)
                       '(cut ?chopped-jalapeno ?kitchen-state-with-chopped-jalapeno ?kitchen-state-with-seeded-jalapeno ?seeded-jalapeno slices ?knife)
                       '(fetch-and-proportion ?shallot ?kitchen-state-with-shallot ?kitchen-state-with-chopped-jalapeno ?target-container-11 shallot 2 piece)
                       '(cut ?cut-shallot ?kitchen-state-with-cut-shallot ?kitchen-state-with-shallot ?shallot minced ?knife)
                       '(fetch ?jar ?kitchen-state-with-fetched-jar ?kitchen-state-with-cut-shallot jar 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-fetched-jar ?jar ?minced-basil ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?olive-oil ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?lime-juice ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?white-sugar ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?salt ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?pepper ?quantity-f ?unit-f)
                       '(cover ?covered-jar ?kitchen-state-with-covered-jar ?output-kitchen-state-f ?output-container-f ?jar-lid)
                       '(shake ?salad-dressing ?kitchen-state-with-dressing ?kitchen-state-with-covered-jar ?covered-jar)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-dressing large-bowl 1)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?kitchen-state-with-fetched-large-bowl ?large-bowl ?thawed-corn ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?chopped-cucumber ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?chopped-jalapeno ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?cut-shallot ?quantity-j ?unit-j)
                       '(mingle ?salad-base ?kitchen-state-with-salad-base ?output-kitchen-state-j ?output-container-j ?wooden-spoon)
                       '(uncover ?uncovered-jar ?used-jar-lid ?kitchen-state-with-uncovered-jar ?kitchen-state-with-salad-base ?covered-jar)
                       '(sprinkle ?drizzled-salad-base ?kitchen-state-with-drizzled-salad-base ?kitchen-state-with-uncovered-jar ?salad-base ?salad-dressing)
                       '(mingle ?salad ?kitchen-state-with-salad ?kitchen-state-with-drizzled-salad-base ?drizzled-salad-base ?wooden-spoon)
                       '(refrigerate ?cooled-salad ?kitchen-state-with-cooled-salad ?kitchen-state-with-salad ?salad ?fridge ?cooling-quantity ?cooling-unit))
                 :primary-output-var '?cooled-salad))

(defparameter *vegan-black-bean-and-sweet-potato-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'vegan-black-bean-and-sweet-potato-salad
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list  (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'onion :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'piece)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 10)))))))
                        (make-instance 'pantry
                                       :contents (list  (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'sweet-potato :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'black-bean :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'red-onion :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'piece)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 5)))))                                          
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'olive-oil :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 50)))))                                         
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'ground-black-pepper :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'ground-cumin :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'lime-juice :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'coarse-salt :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 500)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'fresh-cilantro :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 50)))))
                                                        (make-instance 'bowl
                                                                       :used T
                                                                       :contents (list (make-instance 'red-pepper-flakes :amount
                                                                                                      (make-instance 'amount
                                                                                                                     :unit (make-instance 'g)
                                                                                                                     :quantity (make-instance 'quantity
                                                                                                                                              :value 50)))))))
                  (make-instance 'kitchen-cabinet
                                 :contents (list
                                            ;; bowls
                                            (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                            (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                            (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                            (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                            (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                            (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                            ;; bowl-lids
                                            (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                            (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                            ;; jars
                                            (make-instance 'jar) (make-instance 'jar) (make-instance 'jar)

                                            ;; jar-lids
                                            (make-instance 'jar-lid) (make-instance 'jar-lid) (make-instance 'jar-lid)

                                            ;; wrapping
                                            (make-instance 'plastic-wrap)

                                            ;; tools
                                            (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                            (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                            (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)

                                            ;; baking equipment
                                            (make-instance 'cookie-sheet)
                                            (make-instance 'baking-tray)
                                            (make-instance 'baking-paper)))))
         
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?kitchen ?target-container-1 sweet-potato 450 g)
                       '(peel ?peeled-sweet-potatoes ?sweet-potato-peels ?kitchen-state-with-peeled-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?proportioned-sweet-potatoes ?knife)
                       '(cut ?sweet-potato-cubes ?kitchen-state-with-sweet-potato-cubes ?kitchen-state-with-peeled-sweet-potatoes ?peeled-sweet-potatoes two-cm-cubes ?knife)
                       '(fetch-and-proportion ?one-tablespoon-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?kitchen-state-with-sweet-potato-cubes ?target-container-1-tablespoon olive-oil 1 tablespoon)
                       '(fetch-and-proportion ?two-tablespoons-olive-oil ?kitchen-state-with-two-tablespoons-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?target-container-2-tablespoons olive-oil 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-ground-cumin ?kitchen-state-with-ground-cumin ?kitchen-state-with-two-tablespoons-olive-oil ?target-container-3 ground-cumin 0.5 tablespoon)
                       '(fetch-and-proportion ?proportioned-red-pepper-flakes ?kitchen-state-with-red-pepper-flakes ?kitchen-state-with-ground-cumin ?target-container-4 red-pepper-flakes 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-red-pepper-flakes ?target-container-5 coarse-salt 1 g)
                       '(fetch-and-proportion ?proportioned-pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 1 g)
                       '(fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-pepper ?target-container-7 lime-juice 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-black-beans ?kitchen-state-with-black-beans ?kitchen-state-with-lime-juice ?target-container-8 black-bean 400 g)
                       '(fetch-and-proportion ?proportioned-red-onion ?kitchen-state-with-red-onion ?kitchen-state-with-black-beans ?target-container-9 red-onion 0.5 piece)
                       '(cut ?finely-chopped-red-onion ?kitchen-state-with-finely-chopped-onion ?kitchen-state-with-red-onion ?proportioned-red-onion fine-slices ?knife)
                       '(fetch-and-proportion ?proportioned-fresh-cilantro ?kitchen-state-with-fresh-cilantro ?kitchen-state-with-finely-chopped-onion ?target-container-10  fresh-cilantro 8 g)
                       '(cut ?chopped-fresh-cilantro ?kitchen-state-with-fresh-chopped-cilantro ?kitchen-state-with-fresh-cilantro ?proportioned-fresh-cilantro slices ?knife)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-fresh-chopped-cilantro ?oven 230 degrees-celsius)
                       '(fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-preheated-oven baking-tray 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-baking-tray ?baking-tray ?sweet-potato-cubes ?quantity-a ?unit-a)
                       '(sprinkle ?drizzled-potatoes ?kitchen-state-with-drizzled-potatoes ?output-kitchen-state-a ?output-container-a ?one-tablespoon-olive-oil)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?kitchen-state-with-drizzled-potatoes ?drizzled-potatoes ?proportioned-ground-cumin ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-red-pepper-flakes ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-salt ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-pepper ?quantity-e ?unit-e)
                       '(mingle ?tossed-potatoes ?kitchen-state-with-tossed-potatoes ?output-kitchen-state-e ?output-container-e ?wooden-spoon)
                       '(bake ?baked-potatoes ?kitchen-state-with-baked-potatoes ?kitchen-state-with-tossed-potatoes ?tossed-potatoes ?preheated-oven 25 minute ?bake-quantity ?bake-unit)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-baked-potatoes large-bowl 1)
                       '(fetch ?whisk ?kitchen-state-with-fetched-whisk ?kitchen-state-with-fetched-large-bowl whisk 1)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?kitchen-state-with-fetched-whisk ?large-bowl ?two-tablespoons-olive-oil ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?lime-juice ?quantity-g ?unit-g)
                       '(mix ?whisked-mixture ?kitchen-state-with-whisked-mixture ?output-kitchen-state-g ?output-container-g ?whisk)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?whisked-mixture ?baked-potatoes ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?proportioned-black-beans ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?finely-chopped-red-onion ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-kitchen-state-k ?output-kitchen-state-j ?output-container-j ?proportioned-fresh-cilantro ?quantity-k ?unit-k)
                       '(mingle ?salad ?kitchen-state-with-salad ?output-kitchen-state-k ?output-container-k ?wooden-spoon))
                 :primary-output-var '?salad))

; list of all available simulation environments
(defparameter *simulation-environments*
  (list *almond-crescent-cookies-environment*
        *afghan-biscuits-environment*
        *best-brownies-environment*
        *chocolate-fudge-cookies-environment*
        *easy-banana-bread-environment*
        *easy-oatmeal-cookies-environment*
        *whole-wheat-ginger-snaps-environment*
        *cucumber-slices-with-dill-environment*
        *easy-cherry-tomato-corn-salad-environment*
        *vegan-black-bean-and-sweet-potato-salad-environment*))