-- |
-- Module: Lib
-- Description: Implementation of finite automata (NFA).
module Automata (Automata (..), runAutomata, testInput) where

import Data.Foldable (foldl')

-- | Automata's transition function, normally denoted with a greek delta.
-- Takes in a state and a symbol, and returns all transitions from that
-- state via that symbol.
data Automata a b = Automata
  { getAllStates :: ![a],
    getStartState :: !a,
    getAcceptingStates :: ![a],
    delta :: !(a -> b -> [a])
  }

-- | Generalised transition function that applies the transition to all input
-- states and returns the union of output states.
deltaT :: (Foldable t) => Automata a b -> t a -> b -> [a]
deltaT automata states word = concatMap (flip (delta automata) word) states

-- | Get all end states from a start state using a foldable of symbols.
runAutomata :: (Foldable t) => Automata a b -> a -> t b -> [a]
runAutomata automata start word =
  let stepList = deltaT automata
   in foldl' stepList [start] word

-- | Test whether an input foldable of symbols is accepted by an automaton.
testInput :: (Eq a, Foldable t) => Automata a b -> t b -> Bool
testInput automata =
  let accepting = flip elem (getAcceptingStates automata)
      start = getStartState automata
      runFrom = runAutomata automata
   in any accepting . runFrom start
