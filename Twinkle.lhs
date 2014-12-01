\section{Twinkle twinkle}
\label{twinkle}

{\small\begin{verbatim} 

> module Twinkle where
> import Haskore
> 
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
> cmap f l = chord (map f l)
> 
> -- Main Voice:
> v1  = line [v1a, v1b, v1c, v1d]
> v1a = lmap vol [c 5 qn, c 5 qn, g 5 qn, g 5 qn]
> v1b = lmap vol [a 5 qn, a 5 qn, g 5 hn]
> v1c = lmap vol [f 5 qn, f 5 qn, e 5 qn, e 5 qn]
> v1d = lmap vol [d 5 qn, d 5 qn, c 5 hn]
>
> v2  = line [v2a, v2b, v2c, v2d]
> v2a = lmap vol [g 5 qn, g 5 qn, f 5 qn, f 5 qn]
> v2b = lmap vol [e 5 qn, e 5 qn, d 5 hn]
> v2c = lmap vol [g 5 qn, g 5 qn, f 5 qn, f 5 qn]
> v2d = lmap vol [e 5 qn, e 5 qn, d 5 hn]
>
> mainVoice = line [v1, v2, v1]
>
> -- Chords:
> c1 = line [cMajor, hnr, fMajor, cg, cg, cMajor]
> c2 = times 4 cg
>
> gMajor = cmap vol [g 4 hn, b 5 hn, d 5 hn]
> fMajor = cmap vol [f 4 hn, a 5 hn, c 5 hn]
> cMajor = cmap vol [c 5 hn, e 5 hn, g 5 hn]
> cg = line [cMajor, gMajor]
>  
> chords = line [c1, c2, c1]
> 
> -- Putting it all together:
> twinkle = Instr "piano" (Tempo 2.2 (Phrase [Dyn SF] mainVoice :=: chords))

\end{verbatim} }
