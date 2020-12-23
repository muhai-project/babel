;(ql:quickload :coco-grammar)
(in-package :coco-grammar)
(activate-monitor trace-fcg)

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