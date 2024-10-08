;(ql:quickload :coco-grammar)
(in-package :coco-grammar)
(activate-monitor trace-fcg)

;; cars photo
(comprehend "do the leftmost car and the rightmost car have the same color?")
(comprehend "are there an equal number of cars and traffic lights?")
;; savana photo
(comprehend "how many animals are either zebras or giraffes") ;; would be better with 'animals' instead of 'things'
;; anslow photo
(comprehend "are there more laptops than people")
;; football photo
;; don't know if this will work
;; assuming the color of the player == the color of the shirt
(comprehend "is there a white player left of the rightmost yellow player") 
;; tennis court photo
(comprehend "are there fewer tennis rackets than people")
;; breakfast photo
(comprehend "are there any cups that have the same color as the rightmost apple")
(comprehend "how many things are either apples or cups?") ;; here 'things' is okay
(comprehend "what color is the thing that is both right of the red apple and left of the cup")
(comprehend "what is the thing that is both right of the red apple and left of the cup") ;; 'category' is maybe a bit weird
;; surfer boy photo
(comprehend "is the tent on the sand") ;; don't know about panoptic segmentatio here, might require beach??
;; cats + teddies photo
(comprehend "how many black cats are there?")
;; home office photo
(comprehend "do the mouse and the bottle have the same color")



(comprehend "how many cows are there?")
(comprehend "is there a sheep left of the cow?")
(comprehend "how many sheep are right of the cow?")
(comprehend "what category is the thing left of the dog?")
(comprehend "how many cats are left of the dog and right of the cow?")
(comprehend "how many things are cats or dogs?")
(comprehend "are there an equal number of cows and sheep?")
(comprehend "are there more cows than sheep?")
(comprehend "are there fewer cows than sheep?")
(comprehend "is there a zebra left of the giraffe?")
(comprehend "is there a sea below the sky?")

(comprehend "what color is the cow?")
(comprehend "is there a cloudy blue rug?")

(comprehend "on which side of the photo is the cow?")
(comprehend "on which side of the photograph is the cow?")
(comprehend "on which side of the picture is the cow?")

(comprehend "where in the photo is the cow?")
(comprehend "where in the photograph is the cow?")
(comprehend "where in the picture is the cow?")

(comprehend "where is the cow?")

(comprehend "is the cow on the left side of the photo?")
(comprehend "is the cow on the left side of the photograph?")
(comprehend "is the cow on the left side of the picture?")
(comprehend "is the cow on the right side of the photo?")

(comprehend "is the cat black?")
(comprehend "is the cat black or white?")
(comprehend "is the zebra black or white?")

(comprehend "Is the woman to the left of a person?")
(comprehend "On which side of the photo is the woman?")
(comprehend "are there any suitcases on the floor")
(comprehend "are there any suitcases beside the man")
(comprehend "what color is the suitcase beside the man")
(comprehend "which side is the tree on")
(comprehend "which side is the clock on")
(comprehend "Which side is the chair on?")
(comprehend "What shape is the pillow to the right of the clock?")
(comprehend "On which side of the picture is the lamp?")
(comprehend "Are there any players on the field?")
(comprehend "Do the sock and the sports ball have the same color?")
(comprehend "The sports ball on the field has what color?")
(comprehend "On which side of the picture is the white sports ball?")
(comprehend "Are there any sheep on the field?")
(comprehend "Are there any players to the right of the red helmet?")
(comprehend "is the baseball bat silver?")
(comprehend "is the belt gray?")
(comprehend "is the stop sign on the window?")
(comprehend "on which side is the cell phone?")
(comprehend "what color is the large car?")
(comprehend "what color is the grass?")
(comprehend "on which side is the coach?")
(comprehend "Is the man in a canoe?")
(comprehend "Are there an equal number of wine glasses and people?")
(comprehend "Are there more men than women?")
(comprehend "is the laptop on the left side of the picture?")
(comprehend "how many people are there?")
(comprehend "what color is the wall?")
(comprehend "are the cats red?")
(comprehend "is the picture on the left side of the photo?")
(comprehend "on which side of the photo is the picture?")
(comprehend "Where in the photo is the picture, on the left or on the right?")
(comprehend "Is the picture on the left or on the right side of the picture?")
(comprehend "On which side of the photo is the black picture?")
(comprehend "Is the red helmet on the left side of the picture?")
(comprehend "Where in the photo is the black helmet, on the left or on the right?")
(comprehend "Is the drawer on the right or on the left?")
(comprehend "Is the umpire on the left side or on the right?")
(comprehend "Is the racket on the right side or on the left?")
(comprehend "Is the bowl on the left or on the right side?")
(comprehend "How many birds are there?")
(comprehend "What color is the sky?")
(comprehend "what color is the dining table?")
(comprehend "Are there either any boys or balls?")
(comprehend "Are there red balls or frisbees?")
(comprehend "are there either any blue bats or tennis rackets?")
(comprehend "are there cans or dish soaps")
(comprehend "Are there pliers or buckets in the picture?")
(comprehend "Are there either any chimneys or flags?")
(comprehend "Are there both giraffes and dogs in the photograph?")
(comprehend "is there both dirt and grass in the photo?")
(comprehend "are there any girls to the right of the boy on the left?")
(comprehend "is the traffic light on the left red?")