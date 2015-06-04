module LetItBe where
import Haskore
import AutoComp

vol n = n [Volume 80]
lmap f l = line (map f l)

letitbeMelody = line [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12]

v1 = lmap vol [g 5 sn, g 5 sn, g 5 en, g 5 qn]
v2 = lmap vol [a 5 en, e 5 en, g 5 qn, g 5 en]
v3 = lmap vol [c 6 sn, d 6 sn, e 6 sn, e 6 sn]
v4 = lmap vol [e 6 qn, d 6 en, d 6 qn, c 6 sn]
v5 = lmap vol [c 6 qn, e 6 en, e 6 sn, f 6 en]
v6 = lmap vol [e 6 sn, e 6 en, d 6 qn, e 6 sn]
v7 = lmap vol [d 6 en, c 6 hn, g 5 en, g 5 en]
v8 = lmap vol [g 5 qn, a 5 en, c 6 sn, g 5 en]
v9 = lmap vol [g 5 en, c 6 sn, d 6 en, e 6 sn]
v10 = lmap vol [e 6 qn, e 6 en, d 6 en, d 6 en]
v11 = lmap vol [c 6 sn, c 6 qn, e 6 en, e 6 en]
v12 = lmap vol [f 6 en, e 6 sn, d 6 qn, e 6 en, d 6 en]

letitbeChords1 = map convertFormat [(C,wn),(G,wn)] ++ map convertFormat [(A,hn),(F,hn)] 
letitbeChords2 = map convertFormat [(C,wn),(G,wn)] ++ map convertFormat [(F,hn),(C,hn)] ++ map convertFormat [(D,hn),(C,hn)]
letitbeChords = letitbeChords1 ++ letitbeChords2

letitbeBasic = autoComp letitbeMelody letitbeChords baseSet basic
