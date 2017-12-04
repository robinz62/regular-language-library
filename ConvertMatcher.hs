module ConvertMatcher where

import Data.List

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set
  

import DFA
import NFA
import Regex
import Types

-- data DFA a = D (Set Node, Set a, Map (Node, a) Node, Node, Set Node)
-- deriving (Eq, Show)

-- data NFA a = N (Set Node,
--                 Set a,
--                 (Map (Node, a) (Set Node), Map Node (Set Node)),
--                 Node,
--                 Set Node)

dfaToNFA :: DFA a -> NFA a
dfaToNFA dfa@(D (q, sigma, delta, q0, f)) =
  let newDelta = fmap (\next -> Set.singleton next) delta
  in N (q, sigma, (newDelta, Map.empty), q0, f)


construct :: Ord a => NFA a -> (Set (Set Node), Map (Set Node, a) (Set Node), Set Node) -> [Set Node] -> DFA a
construct nfa@(N (q, sigma, (d, de), q0, f)) (dfaQ, dfaD, dfaStart) [] =
  let indexedStates = Set.toList dfaQ
      deadState = fromJust $ elemIndex Set.empty indexedStates
      newStates = map (\s -> fromJust $ elemIndex s indexedStates) indexedStates
      newDeltaList = [ ((fromJust $ elemIndex s1 indexedStates, c), case Map.lookup (s1, c) dfaD of
                                                                      Nothing -> fromJust $ elemIndex Set.empty indexedStates
                                                                      Just s2 -> fromJust $ elemIndex s2 indexedStates)
                      | s1 <- indexedStates, c <- Set.toList sigma]
      newDelta = Map.fromList newDeltaList
      finalList = [ fromJust $ elemIndex s indexedStates | s <- indexedStates, not (Set.null (Set.intersection s f)) ]
  in D (Set.fromList newStates, sigma, newDelta, fromJust $ elemIndex dfaStart indexedStates, Set.fromList finalList)

construct nfa@(N (q, sigma, (d, de), q0, f)) (dfaQ, dfaD, dfaStart) (state : ss) =
  let (dfaQ', dfaD', newCreatedStates) = foldr (\c (accQ, accD, accNewStates) -> let states = Set.toList state
                                                                                     newState = Set.unions $ catMaybes [ Map.lookup (st, c) d | st <- states]
                                                                                     newStateClosed = epsilonClosure newState
                                                                                     newQ = Set.insert newStateClosed accQ
                                                                                     newD = Map.insert (state, c) newStateClosed accD
                                                                                 in if Set.member newStateClosed accQ
                                                                                   then (newQ, newD, accNewStates)
                                                                                   else (newQ, newD, Set.insert newStateClosed accNewStates)) (dfaQ, dfaD, Set.empty) sigma
  in construct nfa (dfaQ', dfaD', dfaStart) (ss ++ (Set.toList newCreatedStates)) where
    epsilonClosure :: Set Node -> Set Node
    epsilonClosure curr =
      let next = (Set.unions
                  . (:) curr
                  . catMaybes
                  . (fmap (\st -> Map.lookup st de))
                  . Set.toList) curr
      in if Set.size next == Set.size curr
        then curr
        else epsilonClosure next

nfaToDFA :: Ord a => NFA a -> DFA a
nfaToDFA nfa@(N (q, sigma, (d, de), q0, f)) =
  let start = epsilonClosure (Set.singleton q0) in
  construct nfa (Set.fromList [start, Set.empty], Map.empty, start) [start]
  where
    epsilonClosure :: Set Node -> Set Node
    epsilonClosure curr =
      let next = (Set.unions
                  . (:) curr
                  . catMaybes
                  . (fmap (\st -> Map.lookup st de))
                  . Set.toList) curr
      in if Set.size next == Set.size curr
        then curr
        else epsilonClosure next
  

regexToNFA :: RegexA a -> NFA a
regexToNFA = undefined

nfaToRegex :: NFA a -> RegexA a
nfaToRegex = undefined

-- equality???