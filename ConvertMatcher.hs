module ConvertMatcher(dfaToNfa, nfaToDfa, regexToNfa, nfaToRegex) where

import Control.Monad

import qualified Data.List as L
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
  regToNFA Empty alpha = Just $ N (Set.singleton 0,
                                   alpha,
                                   (Map.empty, Map.empty),
                                   0,
                                   Set.singleton 0)
  regToNFA Void alpha  = Just $ N (Set.singleton 0,
                                   alpha,
                                   (Map.empty, Map.empty),
                                   0,
                                   Set.empty)


nfaToRegex :: Ord a => NFA a -> Maybe (RegexA a)
nfaToRegex nfa@(N (q, sigma, (d, de), q0, f)) =
  let preprocessed@(N (q', _, _, q0', f')) = makeSourceAndSink nfa
      transitions  = pairwiseRegex preprocessed in
  do regex <- buildRegex (q', transitions, q0', head $ Set.toList f')
     return $ R (regex, sigma)

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
     deadState <- L.elemIndex Set.empty states
     let newStatesMaybe = map (\s -> L.elemIndex s states) states
     guard (all (/=Nothing) newStatesMaybe)
     let newStates = catMaybes newStatesMaybe
     let newDeltaLMaybe = [ do curr <- L.elemIndex s1 states
                               next <- case Map.lookup (s1, c) dfaD of
                                         Nothing -> L.elemIndex Set.empty states
                                         Just s2 -> L.elemIndex s2 states
                               return ((curr, c), next)
                          | s1 <- states, c <- Set.toList sigma]
     guard (all (/=Nothing) newDeltaLMaybe)
     let newDeltaL = catMaybes newDeltaLMaybe
     let newDelta = Map.fromList newDeltaL
     let finalListMaybe = [ L.elemIndex s states
                          | s <- states, not (Set.null (Set.intersection s f)) ]
     guard (all (/=Nothing) finalListMaybe)
     let finalList = catMaybes finalListMaybe
     startState <- L.elemIndex dfaStart states
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


-- used for nfaToRegex
-- adds a new start state with no incoming transitions
-- adds a new final state with no outgoing transitions
makeSourceAndSink :: NFA a -> NFA a
makeSourceAndSink nfa@(N (q, sigma, (d, de), q0, f)) =
  let newStart = Set.size q
      newFinal = newStart + 1 in
  N (Set.insert newStart (Set.insert newFinal q),
     sigma,
     (d,
      Map.insert newStart (Set.singleton q0) (mapUnionSets (foldr (\x acc ->
        Map.insert x (Set.singleton newFinal) acc)
        Map.empty
        (Set.toList f)) de)
     ),
     newStart,
     Set.singleton newFinal
  )

-- used for nfaToRegex
-- reformats the NFA's transition table "Map (Node, a) Node" to be
-- "Map (Node, Node) (Set a)", not including epsilon transitions
pairwiseTransitions :: Ord a => NFA a -> Map (Node, Node) (Set a)
pairwiseTransitions nfa@(N (q, sigma, (d, de), q0, f)) =
  let states = Set.toList q
      alphabet = Set.toList sigma in
  foldr (\s1 accMap ->
    let mapEntries = foldr (\c accEntries ->
                        case Map.lookup (s1, c) d of
                          Nothing -> accEntries
                          Just s2Set ->
                            foldr (\s2 s2Acc ->
                              ((s1, s2), c) : s2Acc
                            )
                            accEntries
                            (Set.toList s2Set)
                      )
                      []
                      alphabet
    in accumulateEntries mapEntries accMap)
  Map.empty
  states where
    -- adds the list of map entries to the map
    accumulateEntries :: Ord a => [((Node, Node), a)]
                               -> Map (Node, Node) (Set a)
                               -> Map (Node, Node) (Set a)
    accumulateEntries list map =
      foldr (\((s1, s2), c) accMap ->
        case Map.lookup (s1, s2) accMap of
          Nothing -> Map.insert (s1, s2) (Set.singleton c) accMap
          Just set -> Map.insert (s1, s2) (Set.insert c set) accMap
      )
      map
      list

-- used for nfaToRegex
-- outputs the transition table of the NFA in the format
-- "Map (Node, Node) (Regex a)"
pairwiseRegex :: Ord a => NFA a -> Map (Node, Node) (Regex a)
pairwiseRegex nfa@(N (q, sigma, (d, de), q0, f)) =
  let states = Set.toList q
      pairTransitions = pairwiseTransitions nfa
      regexTransitions = fmap Single pairTransitions in
  foldr (\(s1, s2set) accMap ->
    foldr (\s2 accMap2 ->
      case Map.lookup (s1, s2) accMap of
        Nothing -> Map.insert (s1, s2) Empty accMap2
        Just (Single s) -> Map.insert (s1, s2) (Alt (Single s) Empty) accMap2
        _ -> accMap2  -- should never happen
    ) accMap (Set.toList s2set)
  ) regexTransitions (Map.toList de)

-- used for nfaToRegex
-- takes in information about the original NFA
-- (set of states, transitions, initial state, final state) and outputs
-- the equivalent regex
buildRegex :: Ord a => (Set Node, Map (Node, Node) (Regex a), Node, Node)
                    -> Maybe (Regex a)
buildRegex regexNfa@(states, table, q0, qf) =
  if Set.size states <= 2
    then Just $ Map.findWithDefault Void (q0, qf) table
    else
      let nodes = Set.toList states
      in do toRemove <- L.find (\s -> s /= q0 && s /= qf) nodes
            buildRegex (removeNode toRemove regexNfa)

-- used for nfaToRegex
-- removes a node from the "regex-NFA", updating transitions accordingly
-- known as "node elimination" (Gallier 77)
removeNode :: Ord a => Node
                    -> (Set Node, Map (Node, Node) (Regex a), Node, Node)
                    -> (Set Node, Map (Node, Node) (Regex a), Node, Node)
removeNode r (states, table, q0, qf) =
  let states' = Set.delete r states
      statesList' = Set.toList states'
      newTransitions = catMaybes
        [ case (Map.lookup (p, r) table,
                Map.lookup (r, r) table,
                Map.lookup (r, q) table,
                Map.lookup (p, q) table) of
            (Just pr, Just rr, Just rq, Just pq) ->
              Just $ ((p, q), rAlt pq (rSeq (rSeq pr (rStar rr)) rq))
            (Just pr, Nothing, Just rq, Just pq) ->
              Just $ ((p, q), rAlt pq (rSeq pr rq))
            (Just pr, Just rr, Just rq, Nothing) ->
              Just $ ((p, q), rSeq (rSeq pr (rStar rr)) rq)
            (Just pr, Nothing, Just rq, Nothing) ->
              Just $ ((p, q), rSeq pr rq)
            (_, _, _, Just pq) -> Just $ ((p, q), pq)
            (_, _, _, _)       -> Nothing
        | p <- statesList', q <- statesList', p /= r, q /= r ]
      transitionsWithoutR = Map.filterWithKey (\(s1, s2) _ ->
        s1 /= r && s2 /= r) table
      newTable = foldr (\((s1, s2), reg) accTable ->
        -- override previous (p -> q) transition with new one
        Map.insert (s1, s2) reg accTable) transitionsWithoutR newTransitions in
  (states', newTable, q0, qf)
