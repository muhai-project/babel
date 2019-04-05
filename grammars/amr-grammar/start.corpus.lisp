;(ql:quickload :amr-grammar)

(in-package :amr-grammar)

(activate-monitor trace-fcg)

(comprehend "investor") ;; works
(equivalent-amr-predicate-networks (comprehend "investor")
            '((PERSON P) (INVEST-01 I) (:ARG0-OF P I)))

(comprehend "Zintan") ;; works
(equivalent-amr-predicate-networks (comprehend "Zintan")
            '((CITY C) (NAME N) (:NAME C N) (:OP1 N "Zintan")))

(comprehend "bond investor") ;; works
(equivalent-amr-predicate-networks (comprehend "bond investor")
           '((PERSON P) (INVEST-01 I) (BOND B) (:ARG0-OF P I) (:ARG1 I B)))

(comprehend "small investor") ;; works 
(equivalent-amr-predicate-networks (comprehend "small investor")
           '((PERSON P) (INVEST-01 I) (SMALL S) (:ARG0-OF P I) (:MANNER I S)))

(comprehend "atomic bomb") ;; works
(equivalent-amr-predicate-networks (comprehend "atomic bomb")
            '((BOMB B) (ATOM A) (:MOD B A)))

(comprehend "atom bomb") ;;works
(equivalent-amr-predicate-networks (comprehend "atom bomb")
            '((BOMB B) (ATOM A) (:MOD B A)))

(comprehend "Mollie Brown") ;; works
(equivalent-amr-predicate-networks (comprehend "Mollie Brown")
            '((PERSON P) (NAME N) (:NAME P N) (:OP1 N "Mollie") (:OP2 N "Brown")))

(comprehend "President Obama") ;; works 
(equivalent-amr-predicate-networks (comprehend "President Obama")
           '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))

(comprehend "history teacher") ;; works
(equivalent-amr-predicate-networks (comprehend "history teacher")
            '((PERSON P) (TEACH-01 T) (HISTORY H) (:ARG0-OF P T) (:ARG1 T H)))

(comprehend "history professor") ;; works 
(equivalent-amr-predicate-networks (comprehend "history professor")
            '((PERSON P) (TEACH-01 T) (HISTORY H) (:ARG0-OF P T) (:ARG1 T H)))

(comprehend "Obama , the president") ;; works
(equivalent-amr-predicate-networks (comprehend "Obama , the president")
           '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))

(comprehend-all "the attractive spy") ;; works
(equivalent-amr-predicate-networks (comprehend "the attractive spy")
           '((SPY S) (ATTRACT-01 A) (:ARG0-OF S A)))

(comprehend "an edible sandwich") ;; works
(equivalent-amr-predicate-networks (comprehend "an edible sandwich")
           '((SANDWICH S) (EAT-01 E) (POSSIBLE P) (:ARG1-OF S E) (:DOMAIN-OF E P)))

(comprehend-all "a taxable fund") ;; works
(equivalent-amr-predicate-networks (comprehend "a taxable fund")
            '((FUND F) (TAX-01 T) (:ARG1-OF F T)))

(comprehend "the city of Zintan") ;; works but problem with tree. Maybe new construction?
(equivalent-amr-predicate-networks (comprehend  "the city of Zintan")
            '((CITY C) (NAME N) (:NAME C N) (:OP1 N "Zintan")))

(comprehend "the boy cannot go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy cannot go")
            '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "what the girl opined") ;; works
(equivalent-amr-predicate-networks (comprehend "what the girl opined")
            '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "the girl 's opinion") ;; works
(equivalent-amr-predicate-networks (comprehend "the girl 's opinion")
             '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "the marble is white") ;; works
(equivalent-amr-predicate-networks (comprehend "the marble is white")
             '((WHITE W) (MARBLE M) (:DOMAIN W M)))

(comprehend-all "pleasing girls is tough") ;; works
(equivalent-amr-predicate-networks (comprehend "pleasing girls is tough")
            '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the boy works hard") ;; works 
(equivalent-amr-predicate-networks (comprehend "the boy works hard")
            '((WORK-01 W) (BOY B) (HARD H) (:ARG0 W B) (:MANNER W H)))

(comprehend "the soldier feared battle") ;; works
(equivalent-amr-predicate-networks (comprehend  "the soldier feared battle")
            '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))

(comprehend "the comment is inappropriate") ;; works
(equivalent-amr-predicate-networks (comprehend "the comment is inappropriate")
             '((APPROPRIATE A) (COMMENT C) (:DOMAIN A C) (:POLARITY A -)))

(comprehend "the opinion of the girl") ;; works 
(equivalent-amr-predicate-networks (comprehend "the opinion of the girl")
            '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "Mollie Brown , who slew orcs") ;; works
(equivalent-amr-predicate-networks (comprehend "Mollie Brown , who slew orcs")
       '((PERSON P) (NAME N) (SLAY-01 S) (ORC O) (:NAME P N) (:ARG0-OF P S) (:OP1 N "Mollie") (:OP2 N "Brown") (:ARG1 S O)))

(comprehend "the orc-slaying Mollie Brown") ;; new construction for the orc-slaying 
(equivalent-amr-predicate-networks (comprehend "the orc - slaying Mollie Brown")
           '((PERSON P) (NAME N) (SLAY-01 S) (ORC O) (:NAME P N) (:ARG0-OF P S) (:OP1 N "Mollie") (:OP2 N "Brown") (:ARG1 S O)))

(comprehend "the woman is a lawyer") ;; works 
(equivalent-amr-predicate-networks (comprehend "the woman is a lawyer")
            '((LAWYER L) (WOMAN W) (:DOMAIN L W)))

(comprehend "the boy wants to go") ;; to not always in the schema and arg0 not bound to anything
(equivalent-amr-predicate-networks (comprehend "the boy wants to go")
           '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

(comprehend "the college boy who sang") ;; works
(equivalent-amr-predicate-networks (comprehend "the college boy who sang")
           '((BOY B) (SING-01 S) (COLLEGE C) (:ARG0-OF B S) (:SOURCE B C)))

(comprehend "the boy did not go") ;; works but "not" not always in the schema  
(equivalent-amr-predicate-networks (comprehend "the boy did not go")
            '((GO-01 G) (BOY B) (:ARG0 G B) (:POLARITY G -)))

(comprehend "the number of pandas increased") ;; not working active-transitive cxn 
(equivalent-amr-predicate-networks (comprehend "the number of pandas increased")
            '((INCREASE-01 I) (NUMBER N) (PANDA P) (:ARG1 I N) (:QUANT-OF N P)))

(comprehend "the boy must not go") ;;  not always working + schema not correct
(equivalent-amr-predicate-networks (comprehend "the boy must not go")
           '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:ARG0 G B) (:POLARITY G -)))

(comprehend "what did the girl find") ;; not done
(equivalent-amr-predicate-networks (comprehend "what did the girl find")
           '((FIND-01 F) (GIRL G) (AMR-UNKNOWN A) (:ARG0 F G) (:ARG1 F A)))

(comprehend "the girl adjusted the machine") ;; works but problem with order 
(equivalent-amr-predicate-networks (comprehend "the girl adjusted the machine") 
             '((ADJUST-01 A) (GIRL G) (MACHINE M) (:ARG0 A G) (:ARG1 A M)))

(comprehend "the judge saw the explosion") ;; works but problem with order 
(equivalent-amr-predicate-networks (comprehend "the judge saw the explosion")
              '((SEE-01 S) (JUDGE J) (EXPLODE-01 E) (:ARG0 S J) (:ARG1 S E)))

(comprehend "the judge read the proposal") ;; works but problem with order 
(equivalent-amr-predicate-networks (comprehend "the judge read the proposal")
             '((READ-01 R) (JUDGE J) (THING T) (:ARG0 R J) (:ARG1 R T)))

(comprehend "girls are tough to please") ;; not done 
(equivalent-amr-predicate-networks (comprehend "girls are tough to please")
            '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the nation defaulted in June") ;; not done 
(equivalent-amr-predicate-networks (comprehend "the nation defaulted in June")
            '((DEFAULT-01 D) (NATION N) (DATE-ENTITY D2) (:ARG1 D N) (:TIME D D2) (:MONTH D2 6)))

(comprehend "the comment is not appropriate") ;; works
(equivalent-amr-predicate-networks (comprehend "the comment is not appropriate")
           '((APPROPRIATE A) (COMMENT C) (:DOMAIN A C) (:POLARITY A -)))

(comprehend "the marble in the jar") ;; not working 
(equivalent-amr-predicate-networks (comprehend "the marble in the jar")
           '((MARBLE M) (JAR J) (:LOCATION M J)))

(comprehend "the boy destroyed the room") ;; works but problem with order 
(equivalent-amr-predicate-networks (comprehend  "the boy destroyed the room")
           '((DESTROY-01 D) (BOY B) (ROOM R) (:ARG0 D B) (:ARG1 D R)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 41 sentences


(comprehend "the boy need not go") ;; 
(equivalent-amr-predicate-networks (comprehend "the boy need not go")
           '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "the boy looked the answer up") ;; 
(equivalent-amr-predicate-networks (comprehend "the boy looked the answer up")
           '((LOOK-05 L) (BOY B) (ANSWER A) (:ARG0 L B) (:ARG1 L A)))

(comprehend "the boy looked up the answer") ;; 
(equivalent-amr-predicate-networks (comprehend "the boy looked up the answer")
           '((LOOK-05 L) (BOY B) (ANSWER A) (:ARG0 L B) (:ARG1 L A)))

(comprehend "it is tough to please girls") ;; 
(equivalent-amr-predicate-networks (comprehend "it is tough to please girls")
           '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the boy from the college sang") ;; 
(equivalent-amr-predicate-networks (comprehend "the boy from the college sang")
           '((BOY B) (SING-01 S) (COLLEGE C) (:ARG0-OF B S) (:SOURCE B C)))

(comprehend "the boy is a hard worker") ;; 
(equivalent-amr-predicate-networks (comprehend "the boy is a hard worker")
            '((WORK-01 W) (BOY B) (HARD H) (:ARG0 W B) (:MANNER W H)))

(comprehend "the soldier was afraid of battle") ;; 
(equivalent-amr-predicate-networks (comprehend "the soldier was afraid of battle")
            '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))

(comprehend "the girl made adjustments to the machine") ;; 
(equivalent-amr-predicate-networks (comprehend "the girl made adjustments to the machine")
            '((ADJUST-01 A) (GIRL G) (MACHINE M) (:ARG0 A G) (:ARG1 A M)))

(comprehend "the soldier had a fear of battle") ;;  
(equivalent-amr-predicate-networks (comprehend "the soldier had a fear of battle")
             '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))


(evaluate-amr-grammar)

