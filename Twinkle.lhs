\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim} 

> module Twinkle where
> import Haskore
> 
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
> 
> -- repeat something n times
> times  1    m = m
> times n m = m :+: (times (n - 1) m)
> 
> -- Baseline:
> --b1 = lmap (fd dqn) [b  3, fs 4, g  4, fs 4]
> --b2 = lmap (fd dqn) [b  3, es 4, fs 4, es 4]
> --b3 = lmap (fd dqn) [as 3, fs 4, g  4, fs 4]
> 
> --bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
> 
> -- Main Voice:
> v1  = v1a :+: v1b :+: v1c :+: v1d
> v1a = lmap vol [c 5 qn, c 5 qn, g 5 qn, g 5 qn]
> v1b = lmap vol [a 5 qn, a 5 qn, g 5 hn]
> v1c = lmap vol [f 5 qn, f 5 qn, e 5 qn, e 5 qn]
> v1d = lmap vol [d 5 qn, d 5 qn, c 5 hn]
>
> v2  = v2a :+: v2b :+: v2c :+: v2d
> v2a = lmap vol [g 5 qn, g 5 qn, f 5 qn, f 5 qn]
> v2b = lmap vol [e 5 qn, e 5 qn, d 5 hn]
> v2c = lmap vol [g 5 qn, g 5 qn, f 5 qn, f 5 qn]
> v2d = lmap vol [e 5 qn, e 5 qn, d 5 hn]
>
> 
> mainVoice = v1 :+: v2
> 
> -- Putting it all together:
> twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))
> --twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim} }
