module Automata (Character (..), Word, State (..), Automata (..), runAutomata) where

import Data.Foldable (foldlM)
import Prelude hiding (Word)

newtype Character = Character Char deriving (Eq, Show)

type Word = [Character]

newtype State = State String deriving (Eq, Show)

-- (Q, q., F, delta)
data Automata = Automata
  { getAllStates :: ![State],
    getStartState :: !State,
    getAcceptingStates :: ![State],
    getTransitions :: ![((State, Character), State)]
  }

lookupAll :: (Eq a) => a -> [(a, b)] -> [b]
lookupAll t = map snd . filter ((t ==) . fst)

deltaG :: Automata -> State -> Character -> [State]
deltaG automata p x = lookupAll (p, x) . getTransitions $ automata

runAutomata :: Automata -> Word -> Bool
runAutomata automata word =
  let delta :: State -> Character -> [State]
      delta = deltaG automata
   in case foldlM delta (getStartState automata) word of
        [] -> False
        qs -> any (\q -> q `elem` getAcceptingStates automata) qs
