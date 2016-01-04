module Twinkle where
import Haskore
import AutoComp

vol n           = n [Volume 10]
lmap f l        = line (map f l)
quarters5 notes = lmap vol [x 5 qn | x <- notes]

v1  = line [v1a, v1b, v1c, v1d]
v1a = quarters5 [c, c, g, g]
v1b = lmap vol [a 5 qn, a 5 qn, g 5 hn]
v1c = quarters5 [f, f, e, e]
v1d = lmap vol [d 5 qn, d 5 qn, c 5 hn]
v2  = line [v2a, v2b, v2c, v2d]
v2a = quarters5 [g, g, f, f]
v2b = lmap vol [e 5 qn, e 5 qn, d 5 hn]
v2c = quarters5 [g, g, f, f]
v2d = lmap vol [e 5 qn, e 5 qn, d 5 hn]

twinkleMelody = line [v1, v2, v1]

cg = map convertFormat [(C,hn),(G,hn)]
twinkleChords1 = map convertFormat [(C,wn),(F,hn)] ++ cg ++ cg ++ map convertFormat [(C,hn)]
twinkleChords2 = cg ++ cg ++ cg ++ cg
twinkleChords = twinkleChords1 ++ twinkleChords2 ++ twinkleChords1

twinkleBasic   = Tempo 2 $ autoComp twinkleMelody twinkleChords baseSet basic
twinkleCalypso = Tempo 2 $ autoComp twinkleMelody twinkleChords baseSet calypso 
twinkleBoogie  = Tempo 2 $ autoComp twinkleMelody twinkleChords baseSet boogie
