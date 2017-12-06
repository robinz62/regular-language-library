module ConvertMatcher where

import Data.List

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Operations
import Types

dfaToNfa :: DFA a -> NFA a
dfaToNfa dfa@(D (q, sigma, delta, q0, f)) =
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
                                                                                     newStateClosed = epsilonClosure nfa newState
                                                                                     newQ = Set.insert newStateClosed accQ
                                                                                     newD = Map.insert (state, c) newStateClosed accD
                                                                                 in if Set.member newStateClosed accQ
                                                                                   then (newQ, newD, accNewStates)
                                                                                   else (newQ, newD, Set.insert newStateClosed accNewStates)) (dfaQ, dfaD, Set.empty) sigma
  in construct nfa (dfaQ', dfaD', dfaStart) (ss ++ (Set.toList newCreatedStates))

nfaToDfa :: Ord a => NFA a -> DFA a
nfaToDfa nfa@(N (q, sigma, (d, de), q0, f)) =
  let start = epsilonClosure nfa (Set.singleton q0) in
  construct nfa (Set.fromList [start, Set.empty], Map.empty, start) [start]
  
regexToNfa :: Ord a => RegexA a -> NFA a
regexToNfa (R (reg, alpha)) = regToNFA reg alpha where
  regToNFA :: Ord a => Regex a -> Set a -> NFA a
  regToNFA (Single chars) alpha = N (
    Set.fromList [0, 1],
    alpha,
    (Map.fromList (fmap (\c -> ((0, c), Set.singleton 1)) (Set.toList chars)),
     Map.empty),
    0,
    Set.singleton 1)
  regToNFA (Alt r1 r2) alpha = fromJust $ nfaUnion (regToNFA r1 alpha) (regToNFA r2 alpha)
  regToNFA (Seq r1 r2) alpha = fromJust $ nfaConcat (regToNFA r1 alpha) (regToNFA r2 alpha)
  regToNFA (Star r) alpha = fromJust $ nfaKStar (regToNFA r alpha)
  regToNFA Empty alpha = N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.singleton 0)
  regToNFA Void alpha = N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.empty)

nfaToRegex :: NFA a -> RegexA a
nfaToRegex = undefined
