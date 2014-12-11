\section{AutoComp}
\label{dfdfrrfchild}

{\small\begin{verbatim} 

> module AutoComp where
> import Control.Exception hiding (assert)
> import Haskore
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
> majScale pitch = map (flip trans pitch)
> majChord pitch pattern = map (flip trans pitch . (pattern !!)) [0,2,4]
>
> aswrere pc = map (majScale (pc, 3) [0, 2, 4, 5, 7, 9, 11] !!)
>
> boogieBass pc = line $ map (helper . Note) (aswrere pc [0,4,5,4])
>   where helper x = x en [Volume 60]
>
> bass1 = line $ map boogieBass [C,C,D,C,G,C,G,C]
> bass2 = line $ map boogieBass [C,G,C,G,C,G,C,G]
>
> bass = Instr "bass" $ line [bass1, bass2, bass1]
>
>
>
>
> type BassStyle = [(Maybe Int, Dur)]
> basic = [(Just 1, hn), (Just 5, hn)]
> calypso = [(Nothing, qn), (Just 1, en), (Just 3, en)] 
> boogie = [(Just 1, en), (Just 5, en), (Just 6, en), (Just 5, en)] --half
> 
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
