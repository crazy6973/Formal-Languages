module Automata (Automata (..), runAutomata, testInput) where

import Data.Foldable (foldl')

-- (Q, q., F, delta)
data Automata a b = Automata
  { getAllStates :: ![a],
    getStartState :: !a,
    getAcceptingStates :: ![a],
    delta :: !(a -> b -> [a])
  }

deltaT :: (Foldable t) => Automata a b -> t a -> b -> [a]
deltaT automata states word = concatMap (flip (delta automata) word) states

runAutomata :: Automata a b -> a -> [b] -> [a]
runAutomata automata start word =
  let stepList = deltaT automata
   in foldl' stepList [start] word

testInput :: (Eq a) => Automata a b -> [b] -> Bool
testInput automata =
  let accepting = flip elem (getAcceptingStates automata)
   in any accepting . runAutomata automata (getStartState automata)
