\section{Auto Bass}
\label{dfdfrrfchild}

{\small\begin{verbatim} 

> module AutoBass where
> import Control.Exception hiding (assert)
> import Haskore
> 
> 
> 
>
> --cMajor = chord [ Note (x, 4) (hn) [Volume 60] | x<-[C, E, G] ]
> --cNote = Note (C, 4) (hn) [Volume 60]
>
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
>
> 
>
>
> ionianScale pitch = map (helper pitch) [0,2,4,5,7,9,11]
>   where helper pitch x = trans x pitch
>
> keyChord pitch sequence = map (scale !!) $ map (subtract 1) sequence
>   where scale = ionianScale pitch
>
>
> boogieBass note = line $ map (halp . Note) (keyChord (note, 3) [1,5,6,5])
>   where halp x = x en [Volume 60]
>
> bass1 = line $ map boogieBass [C,C,D,C,G,C,G,C]
> bass2 = line $ map boogieBass [C,G,C,G,C,G,C,G]
>
> bass = Instr "bass" $ line [bass1, bass2, bass1]
>
> --autoBass :: BassStyle -> Key -> ChordProgression -> Music
> --autoBass bassStyle key chordProgression = 
>
> --bass bassStyle key chordProgression = 
>
> --type BassStyle = [(Int, Dur)]
> --basic = [(1, hn), (5, hn)]
>
> -- Baseline:
> --list = [b 2, b 3]
> --basic = lmap (fd hn) [b 2, b 3]
> --calypso = times 2 $ line [qnr, c 3 en [Volume 80], e 3 en [Volume 80]] 
> --boogie =  times 2 $ lmap (fd qn) [c 3, g 3, a 3, g 3] 
> --lmap vol [c 3 qnr, c 3 en, d 3 en, c 3 qnr, c 3 en, d 3 en]
> --b3 = lmap (fd dqn) [as 3, fs 4, g  4, fs 4]
> 
> --bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
> --music = times 4 boogie

\end{verbatim} }
