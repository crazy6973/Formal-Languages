module Automata (Automata (..), runAutomata, testInput) where

import Data.Foldable (foldl')

-- (Q, q., F, delta)
data Automata a b = Automata
  { getAllStates :: ![a],
    getStartState :: !a,
    getAcceptingStates :: ![a],
    delta :: !(a -> b -> a)
  }

runAutomata :: (Eq a, Eq b, Traversable t) => Automata a b -> t b -> a
runAutomata automata = foldl' (delta automata) (getStartState automata)

testInput :: (Eq a, Eq b, Traversable t) => Automata a b -> t b -> Bool
testInput automata = flip elem (getAcceptingStates automata) . runAutomata automata
