module Chatterbot where
import Utilities
import Pattern
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      question <- getLine
      answer <- stateOfMind brain
      putStr "\n"
      putStrLn (botName ++ ":" ++ (present . answer . prepare) question)
      if (not . endOfDialog) question
        then botloop
        else do
          putStr "\n"
          return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  rand <- randomIO :: IO Float
  return $ rulesApply $ (map.map2) (id, pick rand) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try . (transformationsApply "*" reflect)

reflect :: Phrase -> Phrase
reflect = map helper
  where helper x = maybe x id (lookup x reflections)


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


--------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

-- Removes the space before the question mark in Eliza's questions
present :: Phrase -> String
present [] = []
present (word:phrase)
  | word == "?" = "?"
  | otherwise = " " ++ word ++ (present phrase)

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (map2 (words . (map toLower), map words))

--------------------------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *"),
    ( "i'm *", "i am *" )
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . (transformationsApply "*" id)
