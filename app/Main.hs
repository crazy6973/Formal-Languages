-- |
-- Module: App
-- Description: Demonstration of using an automata to test if words are in a
-- Language. Specifically, this program tests if input words (defined over the
-- alphabet {'a', 'b'}) have an even number of 'a's.
module Main where

import Automata (Automata (..), testInput)

-- | Defining the states of the automata. Can be alternatively done using
-- string labels.
data State
  = -- | Represents an even number of 'a's.
    SEven
  | -- | Represents an odd number of 'a's.
    SOdd
  deriving (Show, Eq)

-- | Predicate for restricting inputs to only be from an alphabet, if desired.
isValidString :: [Char] -> String -> Bool
isValidString alphabet = all (`elem` alphabet)

-- | An (optional) alphabet to restrict input to, {'a', 'b'}.
testAlphabet :: String
testAlphabet = "ab"

-- | The transition function for the example.
testDelta :: State -> Char -> [State]
testDelta SEven 'a' = [SOdd]
testDelta SEven _ = [SEven]
testDelta SOdd 'a' = [SEven]
testDelta SOdd _ = [SOdd]

-- | Defining the example automata with the states, starting state, accepting
-- states and transition function.
testAutomata :: Automata State Char
testAutomata =
  Automata
    [SEven, SOdd] -- All states
    SEven -- Start state
    [SEven] -- Accepting states
    testDelta -- Transition

program :: String -> String
program input
  | isValidString testAlphabet input =
      if testInput testAutomata input
        then "This word is recognised by the DFA."
        else "This word is not recognised by the DFA."
  | otherwise = "That word has characters not in the alphabet!"

-- | Utility function to check each line of input individually.
lineInteract :: (String -> String) -> IO ()
lineInteract f = interact (unlines . map f . lines)

main :: IO ()
main = lineInteract program
