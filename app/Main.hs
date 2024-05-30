module Main where

import Automata (Automata (..), Character (..), State (..), Word, runAutomata)
import Prelude hiding (Word)

testAlphabet :: [Char]
testAlphabet = "ab"

testStates :: [State]
testStates = map State ["Even", "Odd"]

testStart :: State
testStart = State "Even"

testAccepting :: [State]
testAccepting = map State ["Even"]

testTransitions :: [((State, Character), State)]
testTransitions =
  map
    (\((p, x), q) -> ((State p, Character x), State q))
    [ (("Even", 'a'), "Odd"),
      (("Even", 'b'), "Even"),
      (("Odd", 'a'), "Even"),
      (("Odd", 'b'), "Odd")
    ]

testAutomata :: Automata
testAutomata = Automata testStates testStart testAccepting testTransitions

charToCharacter :: [Char] -> Char -> Maybe Character
charToCharacter alphabet c =
  if c `elem` alphabet
    then Just (Character c)
    else Nothing

stringToWord :: [Char] -> String -> Maybe Word
stringToWord alphabet = mapM (charToCharacter alphabet)

testWord :: String -> Maybe Bool
testWord string = runAutomata testAutomata <$> stringToWord testAlphabet string

main :: IO ()
main = do
  input <- stringToWord testAlphabet <$> getLine
  case input of
    Nothing -> error "That word has characters not in the alphabet!"
    Just word -> print $ runAutomata testAutomata word
