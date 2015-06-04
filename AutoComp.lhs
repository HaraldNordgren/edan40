> module AutoComp where
> import Haskore hiding (Key)
> import Data.Ratio

We started from the exemplary solution from http://www.linusakesson.net/music/functional/index.php. Here, notes are represented as tuples, with the first element a number from 0 to 6 representing the letters C, D, E, F, G, A and B of an octave, and the second one representing sharp or flat notes as 1 or -1, respectively.

> type LNote             = (Int, Int)
> baseSet               = zip [0..6] [0,0..] :: [LNote]

A baseset is generated with every regular (i.e. non-sharp and non-flat) note in an octave. This is the basis for all the accompaniments produced later.

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

Functions are introduced to modulate a set of notes up or down on the note scale,

> type Key             = [LNote]
> type Circle           = [(Key, Key)]
> circle :: Key -> Circle
> circle key            = zip
>                               (iterate modulateUp key)
>                               (iterate modulateDn key)
> set2scale :: LNote -> [LNote] -> [LNote]
> set2scale (base, _) set
>                       = [(scale, o) |
>                               scale <- [base .. base + 6],
>                               (b, o) <- set,
>                               b == mod scale 7]

and more functions to work with the "circle of fifths". For chords, we have functions to generate corresponding major and minor chords from a note.

> major, minor :: LNote -> [LNote]
> major                 = lchord [0, 2, 4] 0
> minor                 = lchord [5, 0, 2] 5
> nearest :: ([LNote] -> Bool) -> Circle -> [LNote]
> nearest pred ((sh, fl) : restCircle)
>       | pred sh       = sh
>       | pred fl       = fl
>       | otherwise     = nearest pred restCircle
> lchord :: [Int] -> Int -> LNote -> [LNote]
> lchord subset pos base = map (set !!) subset
>                               where set = nearest
>                                       ((base ==) . (!! pos))
>                                       (circle baseSet)

Some type definitions, and then three bass patterns we are to use.

> type ChordKind        = LNote -> [LNote]
> type Chord            = (LNote, ChordKind)
> type ChordProgression = [(Chord, Ratio Int)]
> type Style            = [(Int, Ratio Int)]
> basic, calypso, boogie :: Style
> basic                 = [(0, 1%2), (4, 1%2)]
> calypso               = [
>                               (-1, 1%4), (0, 1%8), (2, 1%8),
>                               (-1, 1%4), (0, 1%8), (2, 1%8)]
> boogie                = [
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8),
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8)]

> findChord :: [LNote] -> Key -> [LNote]
> findChord chord key   = nearest inSet (circle key)
>                               where
>                         inSet set = all (flip elem set) chord
> bassScale :: Chord -> Key -> [LNote]
> bassScale (base, kind) key
>                       = set2scale base
>                               (findChord (kind base) key)

Here autoBass is definied, which takes one of the styles defined above, together with a key and a chord progression and generates an accompaniment. The calculated values are made into a Haskore note via the data constructor Note in the toHask function, and then folded into a sequential arragement using the :+: operator.

> semitonify :: LNote -> Int
> semitonify (b, o)     = [0, 2, 4, 5, 7, 9, 11] !! mod b 7
>                       + 12 * quot b 7 + o + 36
> autoBass :: Style -> Key -> ChordProgression -> Music
> autoBass style key prog
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

> bassLine :: Key -> Style -> ChordProgression ->
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

AutoChord in similar to autoBass, with the addition of the :=: operator for parallell composition. The volume of the chord voicing in set at 10% to avoid the bassline being drowned out.

AutoComp puts it all together, invoking autoBass and autoChord and specifying instruments for each Music object with the Instr function and then putting them in a parallell composition.

> autoChord :: ChordProgression -> Music
> autoChord prog        = foldr1 (:+:)
>                               (map toHask (voicing prog))
>                               where toHask (chord, dur) =
>                                       foldr1 (:=:)
>                                               (map note chord)
>                                       where note n = Note
>                                               (pitch n) dur [Volume 10]
> autoComp melody chords key style
>                       = Tempo 1 $ foldr1 (:=:) [
>                               (Instr "Lead 1 (square)" melody),
>                               (Instr "Timpani" bass),
>                               (Instr "Overdriven Guitar" comp)]
>                       where
>                               bass = autoBass style key chords
>                               comp = autoChord chords

Now, to be able to utilize Linus' solution we wrote the functions lnote and convertFormat to convert from Haskore's PitchClasses to a format that fits autoComp.

> lnote :: PitchClass -> LNote
> lnote pc = case pc of
>      Cf -> (0,-1);  C -> (0,0); Cs -> (0,1);
>      Df -> (1,-1);  D -> (1,0); Ds -> (1,1);
>      Ef -> (2,-1);  E -> (2,0); Es -> (2,1);
>      Ff -> (3,-1);  F -> (3,0); Fs -> (3,1);
>      Gf -> (4,-1);  G -> (4,0); Gs -> (4,1);
>      Af -> (5,-1);  A -> (5,0); As -> (5,1);
>      Bf -> (6,-1);  B -> (6,0); Bs -> (6,1);

> convertFormat (pc, dur) = ((lnote pc, major), dur)

In Twinkle.hs we translated the sheet music of Twinkle Twinkle Little Star into Haskore format as melody and a chord progression. For the song of our chosing we picked Let It Be by The Beatles and did the same.

Finally, the autoComp function is put to work in both files where we generate three versions of Twinkle and a basic bass version of Let It Be. By calling testLinux, a test.mid file is generated and then played by timidty.
