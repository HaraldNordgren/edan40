> module AutoComp where
> import Haskore
> import Data.Ratio
> import Twinkle
> import LetItBe



We first wrote Twinkle and Let It Be. For the Auto Comp, we started out by copying the following code from Linus Åkesson's exemplary solution (from http://www.linusakesson.net/music/functional/index.php):

> showNote :: LNote -> String
> showNote (base, offs) = ("CDEFGAB" !! mod base 7)
>                       : replicate (abs offs)
>                               (if offs > 0 then '#' else 'b')
> baseSet               = zip [0..6] [0,0..] :: [LNote]

> upFifth, dnFifth :: LNote -> LNote
> upFifth (base, offs)  = (
>                               mod (base + 4) 7,
>                               offs + if base == 6 then 1 else 0)
> dnFifth (base, offs)  = (
>                               mod (base + 3) 7,
>                               offs - if base == 3 then 1 else 0)

> modulateUp, modulateDn :: [LNote] -> [LNote]
> modulateUp set        = map upFifth set
> modulateDn set        = map dnFifth set


Key renamed LKey to not clash with the type defined in Performance.lhs

> type LKey             = [LNote]
> type Circle           = [(LKey, LKey)]
> circle :: LKey -> Circle
> circle key            = zip
>                               (iterate modulateUp key)
>                               (iterate modulateDn key)

> set2scale :: LNote -> [LNote] -> [LNote]
> set2scale (base, _) set
>                       = [(scale, o) |
>                               scale <- [base .. base + 6],
>                               (b, o) <- set,
>                               b == mod scale 7]

> major, minor :: LNote -> [LNote]
> major                 = lchord [0, 2, 4] 0
> minor                 = lchord [5, 0, 2] 5

> nearest :: ([LNote] -> Bool) -> Circle -> [LNote]
> nearest pred ((sh, fl) : restCircle)
>       | pred sh       = sh
>       | pred fl       = fl
>       | otherwise     = nearest pred restCircle

again renamed to lchord, LKey, LChord, LStyle, LNote

> lchord :: [Int] -> Int -> LNote -> [LNote]
> lchord subset pos base = map (set !!) subset
>                               where set = nearest
>                                       ((base ==) . (!! pos))
>                                       (circle baseSet)

> type ChordKind        = LNote -> [LNote]
> type LChord            = (LNote, ChordKind)
> type ChordProgression = [(LChord, Ratio Int)]


> type LStyle            = [(Int, Ratio Int)]
> basic, calypso, boogie :: LStyle
> basic                 = [(0, 1%2), (4, 1%2)]
> calypso               = [
>                               (-1, 1%4), (0, 1%8), (2, 1%8),
>                               (-1, 1%4), (0, 1%8), (2, 1%8)]
> boogie                = [
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8),
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8)]

> findChord :: [LNote] -> LKey -> [LNote]
> findChord chord key   = nearest inSet (circle key)
>                               where
>                         inSet set = all (flip elem set) chord

> bassScale :: LChord -> LKey -> [LNote]
> bassScale (base, kind) key
>                       = set2scale base
>                               (findChord (kind base) key)

> semitonify :: LNote -> Int
> semitonify (b, o)     = [0, 2, 4, 5, 7, 9, 11] !! mod b 7
>                       + 12 * quot b 7 + o + 36

> autoBassL :: LStyle -> LKey -> ChordProgression -> Music
> autoBassL style key prog
>                       = foldr1 (:+:) $ map toHask (bassLine
>                               key
>                               (cycle style)
>                               prog)
>                       where
>                               toHask ((-1, 0), dur) = Rest dur
>                               toHask (note, dur)    = Note
>                                       (pitch (semitonify note))
>                                       dur
>                                       []

> bassLine :: LKey -> LStyle -> ChordProgression ->
>                                       [(LNote, Ratio Int)]
> bassLine _ _ []       = []
> bassLine
>       key
>       style@((index, sdur):srest)
>       chords@((chord, cdur):crest)
> 
>               | sdur == 0
>                       = bassLine key srest chords
> 
>               | cdur == 0
>                       = bassLine key style crest
> 
>               | otherwise
>                       = play dur : bassLine key
>                               ((index, sdur - dur):srest)
>                               ((chord, cdur - dur):crest)
> 
>       where
>               dur             = min cdur sdur
>               play dur        = if index == -1
>                       then ((-1, 0), dur)
>                       else (bassScale chord key !! index, dur)

> resolveChord :: [LNote] -> [Int]
> resolveChord chord    = map semitonify chord

> octaveCombinations :: [Int] -> [[Int]]
> octaveCombinations [] = [[]]
> octaveCombinations (c:rChord)
>                       = (++)
>                               (map ((c + 12) :) rest)
>                               (map ((c + 24) :) rest)
>                       where rest = octaveCombinations rChord

> validCombinations :: [Int] -> [[Int]]
> validCombinations rChord
>                       = [ c | c <- octaveCombinations rChord,
>                               all (>= 52) c,
>                               all (<= 67) c]

> sort :: [Int] -> [Int]
> sort []               = []
> sort list             = m: sort [ l | l <- list, l > m ]
>                               where m = minimum list

> combinations :: [LNote] -> [[Int]]
> combinations chord
>                       = map sort (validCombinations
>                               (resolveChord chord))

> staticScore :: [Int] -> Int
> staticScore chord     = maximum chord - minimum chord

> dynamicScore :: [Int] -> [Int] -> Int
> dynamicScore c1 c2    = sum $ map abs $ zipWith (-) c1 c2

> firstChord :: [LNote] -> [Int]
> firstChord chord      = minimize staticScore
>                               (combinations chord)

> nextChord :: [Int] -> [LNote] -> [Int]
> nextChord pre chord   = minimize score (combinations chord)
>                               where score c = (+)
>                                       (staticScore c)
>                                       (dynamicScore pre c)

> minimize :: ([Int] -> Int) -> [[Int]] -> [Int]
> minimize func list    = fst $ head [ e | e <- doubleList,
>                               snd e == minimum
>                                       (map snd doubleList)]
>                               where doubleList = zip list
>                                       (map func list)

> voicing :: ChordProgression -> [([Int], Ratio Int)]
> voicing ((chord, dur):prog)
>                       = (first, dur) : nextVoicing first prog
>                               where
>                               first = firstChord (kind base)
>                               (base, kind) = chord

> nextVoicing :: [Int] -> ChordProgression ->
>                                       [([Int], Ratio Int)]
> nextVoicing _ []      = []
> nextVoicing pre ((chord, dur):prog)
>                       = (next, dur) : nextVoicing next prog
>                               where
>                               next = nextChord pre (kind base)
>                               (base, kind) = chord

> autoChord :: ChordProgression -> Music
> autoChord prog        = foldr1 (:+:)
>                               (map toHask (voicing prog))
>                               where toHask (chord, dur) =
>                                       foldr1 (:=:)
>                                               (map note chord)
>                                       where note n = Note
>                                               (pitch n) dur []


> autoComp melody chords key style
>                       = Tempo 1 $ foldr1 (:=:) [
>                               (Instr "Lead 1 (square)" melody),
>                               (Instr "Acoustic Bass" bass),
>                               (Instr "Overdriven Guitar" comp)]
>                       where
>                               bass = autoBassL style key chords
>                               comp = autoChord chords


We wrote the functions lnote and pitchclass to convert back and forth between Linus' implementation of notes as bare numbers, and our Haskore system of Pitch classes.

> type LNote             = (Int, Int)
> lnote :: PitchClass -> LNote
> lnote pc = case pc of
>      Cf -> (0,-1);  C -> (0,0); Cs -> (0,1);
>      Df -> (1,-1);  D -> (1,0); Ds -> (1,1);
>      Ef -> (2,-1);  E -> (2,0); Es -> (2,1);
>      Ff -> (3,-1);  F -> (3,0); Fs -> (3,1);
>      Gf -> (4,-1);  G -> (4,0); Gs -> (4,1);
>      Af -> (5,-1);  A -> (5,0); As -> (5,1);
>      Bf -> (6,-1);  B -> (6,0); Bs -> (6,1); 

> pitchclass :: LNote -> PitchClass
> pitchclass note = case note of
>     (0,-1) -> Cf;  (0,0) -> C; (0,1) -> Cs;
>     (1,-1) -> Df;  (1,0) -> D; (1,1) -> Ds;
>     (2,-1) -> Ef;  (2,0) -> E; (2,1) -> Es;
>     (3,-1) -> Ff;  (3,0) -> F; (3,1) -> Fs;
>     (4,-1) -> Gf;  (4,0) -> G; (4,1) -> Gs;
>     (5,-1) -> Af;  (5,0) -> A; (5,1) -> As;
>     (6,-1) -> Bf;  (6,0) -> B; (6,1) -> Bs; 


> cg = map helper2 [(C,hn),(G,hn)]
> helper2 (pc, dur) = ((lnote pc, major), dur)

> twinkleprog1 = map helper2 [(C,wn),(F,hn)] ++ cg ++ cg ++ map helper2 [(C,hn)]
> twinkleprog2 = cg ++ cg ++ cg ++ cg
> twinkleprog = twinkleprog1 ++ twinkleprog2 ++ twinkleprog1

> letitbeprog1 = map helper2 [(C,wn),(G,wn)] ++ map helper2 [(A,hn),(F,hn)] 
> letitbeprog2 = map helper2 [(C,wn),(G,wn)] ++ map helper2 [(F,hn),(C,hn)] ++ map helper2 [(D,hn),(C,hn)]
> letitbeprog = letitbeprog1 ++ letitbeprog2 



> twinkleBasic   = autoComp twinkleMelody twinkleprog baseSet basic
> twinkleCalypso = autoComp twinkleMelody twinkleprog baseSet calypso 
> twinkleBoogie  = autoComp twinkleMelody twinkleprog baseSet boogie
> letitbeBasic = autoComp letitbeMelody letitbeprog baseSet basic
