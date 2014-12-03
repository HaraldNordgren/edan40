module Twinkle where
import Haskore
import AutoComp

cmap f l = chord (map f l)

-- Main Voice:
fourQuarters notes = lmap vol [x 5 qn | x <- notes]

v1  = line [v1a, v1b, v1c, v1d]
v1a = fourQuarters [c, c, g, g]
v1b = lmap vol [a 5 qn, a 5 qn, g 5 hn]
v1c = fourQuarters [f, f, e, e]
v1d = lmap vol [d 5 qn, d 5 qn, c 5 hn]

v2  = line [v2a, v2b, v2c, v2d]
v2a = fourQuarters [g, g, f, f]
v2b = lmap vol [e 5 qn, e 5 qn, d 5 hn]
v2c = fourQuarters [g, g, f, f]
v2d = lmap vol [e 5 qn, e 5 qn, d 5 hn]

mainVoice = Instr "piano" $ line [v1, v2, v1]

-- Chords:
ionian     = [0, 2, 4, 5, 7, 9, 11]
mixolydian = [0, 2, 4, 5, 7, 9, 10]
lydian     = [0, 2, 4, 6, 7, 9, 11]

chordScale pitch = map (flip trans pitch)

makeChord c
  | c == C = majChord (C, 5) ionian
  | c == G = majChord (G, 4) mixolydian
  | c == F = majChord (F, 4) lydian

generateAcc = line . map (chord . map toMusic)
  where toMusic x = Note x hn [Volume 60]

accompaniment = Instr "piano" $ line [acc1, acc2, acc1]
acc1 = generateAcc $ map makeChord [C, C, F, C, G, C, G, C]
acc2 = times 4 $ generateAcc $ map makeChord [C, G]

-- Putting it all together:
--twinkle = Tempo 2.2 $ Phrase [Dyn SF] mainVoice :=: accompaniment :=: bass
twinkle = Tempo 2.2 $ Phrase [Dyn SF] mainVoice :=: accompaniment
