\section{Twinkle twinkle}
\label{twinkle}

{\small\begin{verbatim} 

> module Twinkle where
> import Haskore
> import AutoBass
> 
> cmap f l = chord (map f l)
> 
> -- Main Voice:
> v1  = line [v1a, v1b, v1c, v1d]
> v1a = lmap vol [x 5 qn | x <- [c,c,g,g] ]
> v1b = lmap vol [a 5 qn, a 5 qn, g 5 hn]
> v1c = lmap vol [x 5 qn | x <- [f,f,e,e] ]
> v1d = lmap vol [d 5 qn, d 5 qn, c 5 hn]
>
> v2  = line [v2a, v2b, v2c, v2d]
> v2a = lmap vol [x 5 qn | x <- [g,g,f,f] ]
> v2b = lmap vol [e 5 qn, e 5 qn, d 5 hn]
> v2c = lmap vol [x 5 qn | x <- [g,g,f,f] ]
> v2d = lmap vol [e 5 qn, e 5 qn, d 5 hn]
>
> mainVoice = Instr "piano" $ line [v1, v2, v1]
>
>
> -- Chords:
>
> generateAcc c = line $ map (halp . Note) c
>   where halp x = x hn v
>
>
> progression cp = map helper cp
>   where helper x = (x, 4)
>
> acc1 = generateAcc $ progression [C, C, F, C, G, C, G, C]
> acc2 = generateAcc $ progression [C, G, C, G, C, G, C, G]
>
> acc = line [acc1, acc2, acc1]
>
> 
> -- Putting it all together:
> --twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF] bass))
> twinkle = (Tempo 2.2 (Phrase [Dyn SF] mainVoice :=: acc :=: bass))

\end{verbatim} }
