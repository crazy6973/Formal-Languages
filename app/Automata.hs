module Automata (Automata (..), runAutomata) where

import Data.Foldable (foldlM)

-- (Q, q., F, delta)
data Automata a b = Automata
  { getAllStates :: ![a],
    getStartState :: !a,
    getAcceptingStates :: ![a],
    getTransitions :: ![((a, b), a)]
  }

lookupAll :: (Eq a) => a -> [(a, b)] -> [b]
lookupAll t = map snd . filter ((t ==) . fst)

deltaG :: (Eq a, Eq b) => Automata a b -> a -> b -> [a]
deltaG automata p x = lookupAll (p, x) . getTransitions $ automata

runAutomata :: (Eq a, Eq b) => Automata a b -> [b] -> Bool
runAutomata automata word =
  let delta = deltaG automata
   in case foldlM delta (getStartState automata) word of
        [] -> False
        qs -> any (\q -> q `elem` getAcceptingStates automata) qs
