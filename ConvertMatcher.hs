module ConvertMatcher(dfaToNfa, nfaToDfa, regexToNfa, nfaToRegex) where

import Control.Monad

import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set

import Operations
import Types

dfaToNfa :: DFA a -> Maybe (NFA a)
dfaToNfa dfa@(D (q, sigma, delta, q0, f)) =
  let newDelta = fmap (\next -> Set.singleton next) delta
  in Just $ N (q, sigma, (newDelta, Map.empty), q0, f)

nfaToDfa :: Ord a => NFA a -> Maybe (DFA a)
nfaToDfa nfa@(N (_, _, _, q0, _)) =
  let start = epsilonClosure nfa (Set.singleton q0) in
  construct nfa (Set.fromList [start, Set.empty], Map.empty, start) [start]
  
regexToNfa :: Ord a => RegexA a -> Maybe (NFA a)
regexToNfa (R (reg, alpha)) = regToNFA reg alpha where
  regToNFA :: Ord a => Regex a -> Set a -> Maybe (NFA a)
  regToNFA (Single chars) alpha = Just $ N (
    Set.fromList [0, 1],
    alpha,
    (Map.fromList (fmap (\c -> ((0, c), Set.singleton 1)) (Set.toList chars)),
     Map.empty),
    0,
    Set.singleton 1)
  regToNFA (Alt r1 r2) alpha = do nfa1 <- regToNFA r1 alpha
                                  nfa2 <- regToNFA r2 alpha
                                  nfaUnion nfa1 nfa2
  regToNFA (Seq r1 r2) alpha = do nfa1 <- regToNFA r1 alpha
                                  nfa2 <- regToNFA r2 alpha
                                  nfaConcat nfa1 nfa2
  regToNFA (Star r) alpha = do nfa <- regToNFA r alpha
                               nfaKStar nfa
  regToNFA Empty alpha = Just $ N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.singleton 0)
  regToNFA Void alpha = Just $ N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.empty)

nfaToRegex :: NFA a -> Maybe (RegexA a)
nfaToRegex = undefined

-------------
-- private --
-------------

-- subset construction of nfa to dfa
-- takes in
--   1) the NFA to construct from
--   2) the current "DFA-under-construction"
--   3) a list of subset-states that still need to have transitions added to
-- returns a DFA equivalent to the input NFA
construct :: Ord a => NFA a
                   -> (Set (Set Node), Map (Set Node, a) (Set Node), Set Node)
                   -> [Set Node]
                   -> Maybe (DFA a)
construct nfa@(N (_, sigma, _, _, f)) (dfaQ, dfaD, dfaStart) [] =
  do let states = Set.toList dfaQ
     deadState <- elemIndex Set.empty states
     let newStatesMaybe = map (\s -> elemIndex s states) states
     guard (all (/=Nothing) newStatesMaybe)
     let newStates = catMaybes newStatesMaybe
     let newDeltaLMaybe = [ do curr <- elemIndex s1 states
                               next <- case Map.lookup (s1, c) dfaD of
                                         Nothing -> elemIndex Set.empty states
                                         Just s2 -> elemIndex s2 states
                               return ((curr, c), next)
                          | s1 <- states, c <- Set.toList sigma]
     guard (all (/=Nothing) newDeltaLMaybe)
     let newDeltaL = catMaybes newDeltaLMaybe
     let newDelta = Map.fromList newDeltaL
     let finalListMaybe = [ elemIndex s states
                          | s <- states, not (Set.null (Set.intersection s f)) ]
     guard (all (/=Nothing) finalListMaybe)
     let finalList = catMaybes finalListMaybe
     startState <- elemIndex dfaStart states
     return $ D (Set.fromList newStates,
                 sigma,
                 newDelta,
                 startState,
                 Set.fromList finalList)
construct nfa@(N (_, sigma, (d, _), _, _)) (dfaQ, dfaD, dfaStart) (state : ss) =
  let
    (dfaQ', dfaD', createdStates) =
      foldr (\c (accQ, accD, accNewStates) ->
        let states = Set.toList state
            newState = Set.unions $ catMaybes [ Map.lookup (st, c) d
                                              | st <- states]
            newStateClosed = epsilonClosure nfa newState
            newQ = Set.insert newStateClosed accQ
            newD = Map.insert (state, c) newStateClosed accD
        in if Set.member newStateClosed accQ
          then (newQ, newD, accNewStates)
          else (newQ, newD, Set.insert newStateClosed accNewStates))
      (dfaQ, dfaD, Set.empty) sigma
  in construct nfa (dfaQ', dfaD', dfaStart) (ss ++ (Set.toList createdStates))