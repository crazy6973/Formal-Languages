module Main where

import Automata (Automata (..), testInput)

newtype Character = Character Char deriving (Eq, Show)

newtype State = State String deriving (Eq, Show)

charToCharacter :: [Char] -> Char -> Maybe Character
charToCharacter alphabet c =
  if c `elem` alphabet
    then Just (Character c)
    else Nothing

stringToWord :: [Char] -> String -> Maybe [Character]
stringToWord alphabet = mapM (charToCharacter alphabet)

testAlphabet :: [Char]
testAlphabet = "ab"

testDelta :: State -> Character -> State
testDelta (State "Even") (Character 'a') = State "Odd"
testDelta (State "Even") (Character _) = State "Even"
testDelta (State "Odd") (Character 'a') = State "Even"
testDelta (State "Odd") (Character _) = State "Odd"
testDelta dump _ = dump

testAutomata :: Automata State Character
testAutomata = Automata (map State ["Even", "Odd"]) (State "Even") (map State ["Even"]) testDelta

program :: String -> String
program input = case stringToWord testAlphabet input of
  Nothing -> "That word has characters not in the alphabet!"
  Just word ->
    ( if testInput testAutomata word
        then "This word is recognised by the DFA."
        else "This word is not recognised by the DFA."
    )

main :: IO ()
main = interact program
