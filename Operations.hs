module Operations(dfaUnion, dfaIntersect, dfaMinus,
                  nfaUnion, nfaConcat, nfaKStar,
                  epsilonClosure, mapUnionSets, mapUnionsSets) where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Types

-- | Returns a DFA accepting the union of the languages specified by the input
--   DFAs. Uses cross product construction.
dfaUnion :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
dfaUnion dfa1 dfa2 =
  crossProductConstruct
    (\(q1, q2) (f1, f2) -> Set.member q1 f1 || Set.member q2 f2)
    dfa1
    dfa2

-- | Returns a DFA accepting the intersection of the languages specified by the
--   input DFAs. Uses cross product construction.
dfaIntersect :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
dfaIntersect dfa1 dfa2 =
  crossProductConstruct
  (\(q1, q2) (f1, f2) -> Set.member q1 f1 && Set.member q2 f2)
  dfa1
  dfa2

-- | Returns a DFA accepting the relative complement of the languages specified
--   by the input DFAs. Uses cross product construction.
dfaMinus :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
dfaMinus dfa1 dfa2 =
  crossProductConstruct
  (\(q1, q2) (f1, f2) -> Set.member q1 f1 && not (Set.member q2 f2))
  dfa1
  dfa2

-- | Returns an NFA accepting the union of the languages specified by the input
--   NFAs. Performs the union by creating a new node as start state with
--   epsilon transitions to the previous NFAs' start states.
nfaUnion :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
nfaUnion nfa1@(N (q1, s1, (d1, de1), q_01, f1)) nfa2 =
  let (N (q2, s2, (d2, de2), q_02, f2)) = offsetNfa nfa2 (Set.size q1)
      newNode = Set.size q1 + Set.size q2
  in if s1 == s2 then Just $ N (
    Set.unions [q1, q2, Set.singleton newNode],
    s1,
    (mapUnionSets d1 d2,
     mapUnionsSets [de1,
                    de2,
                    Map.singleton newNode (Set.fromList [q_01, q_02])]
    ),
    newNode,
    Set.union f1 f2
  ) else Nothing

-- | Returns an NFA accepting the concatenation of the languages specified by
--   the input NFAs. Performs the concatenation by adding epsilon transitions
--   from all of the first NFAs' final states to the second NFA's start state
nfaConcat :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
nfaConcat nfa1@(N (q1, s1, (d1, de1), q_01, f1)) nfa2 =
  let (N (q2, s2, (d2, de2), q_02, f2)) = offsetNfa nfa2 (Set.size q1)
  in if s1 == s2 then Just $ N (
    Set.union q1 q2,
    s1,
    (mapUnionSets d1 d2,
     mapUnionsSets [de1,
                    de2,
                    foldr (\qf map -> Map.insert qf (Set.singleton q_02) map)
                      Map.empty
                      (Set.toList f1)]
    ),
    q_01,
    f2
  ) else Nothing 

-- | Returns an NFA accepting the Kleene-star of the language specified by the
--   input NFA. Creates a new start state with an epsilon transition to the
--   original start state (which is now also a final state). Adds epsilon
--   transitions from all final states to the original start state.
nfaKStar :: Ord a => NFA a -> Maybe (NFA a)
nfaKStar nfa@(N (q, s, (d, de), q_0, f)) =
  let newNode = Set.size q
  in Just $ N (
    Set.insert newNode q,
    s,
    (d,
     mapUnionSets de
      (foldr (\qf map -> Map.insert qf (Set.singleton q_0) map)
        (Map.singleton newNode (Set.singleton q_0))
        (Set.toList f)
      )
    ),
    Set.size q,
    Set.insert (Set.size q) f
  )

--------------------------
-- misc utility methods --
--------------------------

-- | Computes the epsilon closure of the input nodes for the input NFA.
epsilonClosure :: Ord a => NFA a -> Set Node -> Set Node
epsilonClosure nfa@(N (q, s, (d, de), q_0, f)) curr =
  -- convert curr nodes to list, fmap them to their possible next states,
  -- remove Just/Nothing, add the set of states where we started (since
  -- not forced to take epsilon transition), then take union
  let next = (Set.unions
              . (:) curr
              . catMaybes
              . (fmap (\st -> Map.lookup st de))
              . Set.toList) curr
  in if Set.size next == Set.size curr
    then curr
    else epsilonClosure nfa next

-- | Reassigns all nodes' values in original NFA to (original value + n)
offsetNfa :: Ord a => NFA a -> Int -> NFA a
offsetNfa (N (q, s, (d, de), q_0, f)) n =
  N (
    relabelSetNodes q n,
    s,
    (Map.fromList $ fmap (\((u, x), vs) -> ((u + n, x), relabelSetNodes vs n)) (Map.toList d),
      Map.fromList $ fmap (\(u, vs) -> (u + n, relabelSetNodes vs n)) (Map.toList de)),
    q_0 + n,
    relabelSetNodes f n
  )

-- | Reassigns all nodes in set to (original value + n)
relabelSetNodes :: Set Node -> Int -> Set Node
relabelSetNodes nodes n = Set.fromList $ fmap (+n) (Set.toList nodes)

-- | Unions the two input maps, but if the maps share a same key, the
--   corresponding sets are unioned (instead of one replacing the other)
mapUnionSets :: (Ord a, Ord k) => Map k (Set a)
                               -> Map k (Set a)
                               -> Map k (Set a)
mapUnionSets m1 m2 =
  Map.foldrWithKey (\k v accMap ->
    let set = Map.findWithDefault Set.empty k accMap
    in Map.insert k (Set.union v set) accMap)
  m1
  m2

mapUnionsSets :: (Ord a, Ord k) => [Map k (Set a)] -> Map k (Set a)
mapUnionsSets = foldr mapUnionSets Map.empty

-- | Uses a bijection between pairs of ints to an int given an (m x n) table
--   and to transform a the input pair of ints to a single int.
pairToInt :: Int -> Int -> (Int, Int) -> Int
pairToInt m n (i1, i2) = n * i1 + i2

-- | Performs cross product construction on the input DFAs, using function pred
--   to determine the final states
crossProductConstruct :: Ord a => ((Node, Node) -> (Set Node, Set Node) -> Bool)
                               -> DFA a
                               -> DFA a
                               -> Maybe (DFA a)
crossProductConstruct
  pred dfa1@(D (q1, s1, d1, q_01, f1)) dfa2@(D (q2, s2, d2, q_02, f2)) =
    if s1 /= s2 then Nothing
    else
      let m = Set.size q1
          n = Set.size q2
          states1 = Set.toList q1
          states2 = Set.toList q2
          statePairs = [ (q_1, q_2) | q_1 <- states1, q_2 <- states2 ]
          newStates = Set.fromList $ map (\p -> pairToInt m n p) statePairs
          sigma = Set.toList s1
          newTableListMaybe =
            [ case (Map.lookup (s1, c) d1, Map.lookup (s2, c) d2) of
                (Just next1, Just next2) ->
                  Just $ ((pairToInt m n (s1, s2), c),
                           pairToInt m n (next1, next2))
                _                        -> Nothing
              | (s1, s2) <- statePairs, c <- sigma ]
          newTableList = foldr (\x acc -> case acc of
            Nothing -> Nothing
            Just tail -> case x of
              Nothing -> Nothing
              Just head -> Just (head : tail)) (Just []) newTableListMaybe
          newFinalStates = [ pairToInt m n (q_1, q_2)
                             | (q_1, q_2) <- statePairs,
                               pred (q_1, q_2) (f1, f2) ]
      in case newTableList of
        Nothing -> Nothing
        Just newTable ->
          Just $ D (newStates,
                    s1,
                    Map.fromList newTable,
                    pairToInt m n (q_01, q_02),
                    Set.fromList newFinalStates)
