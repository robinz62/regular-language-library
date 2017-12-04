module ConvertMatcher where

import Data.List

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Regex
import Types

dfaToNFA :: DFA a -> NFA a
dfaToNFA dfa@(D (q, sigma, delta, q0, f)) =
  let newDelta = fmap (\next -> Set.singleton next) delta
  in N (q, sigma, (newDelta, Map.empty), q0, f)


-- 
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
  

regexToNFA :: Ord a => RegexA a -> NFA a
regexToNFA (R (reg, alpha)) = regToNFA reg alpha where
  regToNFA :: Ord a => Regex a -> Set a -> NFA a
  regToNFA (Single chars) alpha = N (
    Set.fromList [0, 1],
    alpha,
    (Map.fromList (fmap (\c -> ((0, c), Set.singleton 1)) (Set.toList chars)),
     Map.empty),
    0,
    Set.singleton 1)
  regToNFA (Alt r1 r2) alpha = fromJust $ unionNFA (regToNFA r1 alpha) (regToNFA r2 alpha)
  regToNFA (Seq r1 r2) alpha = fromJust $ concatNFA (regToNFA r1 alpha) (regToNFA r2 alpha)
  regToNFA (Star r) alpha = fromJust $ starNFA (regToNFA r alpha)
  regToNFA Empty alpha = N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.singleton 0)
  regToNFA Void alpha = N (Set.singleton 0, alpha, (Map.empty, Map.empty), 0, Set.empty)

nfaToRegex :: NFA a -> RegexA a
nfaToRegex = undefined

-- data Regex a = Single (Set a)
-- | Alt (Regex a) (Regex a)
-- | Seq (Regex a) (Regex a)
-- | Star (Regex a)
-- | Empty
-- | Void
-- deriving (Show, Eq)

-- rip
starNFA :: Ord a => NFA a -> Maybe (NFA a)
starNFA nfa@(N (q, s, (d, de), q_0, f)) =
  Just $ N (
    Set.insert (Set.size q) q,
    s,
    (d,
     Map.union de (foldr (\qf map -> Map.insert qf (Set.singleton q_0) map) (Map.singleton (Set.size q) (Set.singleton q_0)) (Set.toList f))),
    Set.size q,
    Set.insert (Set.size q) f
  )
concatNFA = undefined
-- concatNFA :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
-- concatNFA nfa1@(N (q1, s1, (d1, de1), q_01, f1)) nfa2 =
--   let (N (q2, s2, (d2, de2), q_02, f2)) = offset nfa2 (Set.size q1)
--   in if s1 == s2 then Just $ N (
--     Set.union q1 q2,
--     s1,
--     (Map.union d1 d2,
--     -- next line is broken; dont overwrite existing epsilon transitions with new one
--      Map.unions [de1, de2, foldr (\f map -> if Map.member f map Map.adjust (\val -> Set.insert q_02 val) f map) Map.empty (Set.toList f1)]),
--     q_01,
--     f2
--   ) else Nothing where
--     -- reassigns all nodes' values to original value + n
--     offset :: Ord a => NFA a -> Int -> NFA a
--     offset (N (q, s, (d, de), q_0, f)) n =
--       N (
--         relabelSetNodes q n,
--         s,
--         (Map.fromList $ fmap (\((u, x), vs) -> ((u + n, x), relabelSetNodes vs n)) (Map.toList d),
--          Map.fromList $ fmap (\(u, vs) -> (u + n, relabelSetNodes vs n)) (Map.toList de)),
--         q_0 + n,
--         relabelSetNodes f n
--       )
--     -- increments all nodes in set by amount specified
--     relabelSetNodes :: Set Node -> Int -> Set Node
--     relabelSetNodes nodes n = Set.fromList $ fmap (+n) (Set.toList nodes)

unionNFA :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
unionNFA nfa1@(N (q1, s1, (d1, de1), q_01, f1)) nfa2 =
  let (N (q2, s2, (d2, de2), q_02, f2)) = offset nfa2 (Set.size q1)
      newNode = Set.size q1 + Set.size q2
  in if s1 == s2 then Just $ N (
    Set.unions [q1, q2, Set.singleton newNode],
    s1,
    (Map.union d1 d2,
     Map.unions [de1, de2, Map.singleton newNode (Set.fromList [q_01, q_02])]),
    newNode,
    Set.union f1 f2
  ) else Nothing where
    -- reassigns all nodes' values to original value + n
    offset :: Ord a => NFA a -> Int -> NFA a
    offset (N (q, s, (d, de), q_0, f)) n =
      N (
        relabelSetNodes q n,
        s,
        (Map.fromList $ fmap (\((u, x), vs) -> ((u + n, x), relabelSetNodes vs n)) (Map.toList d),
         Map.fromList $ fmap (\(u, vs) -> (u + n, relabelSetNodes vs n)) (Map.toList de)),
        q_0 + n,
        relabelSetNodes f n
      )
    -- increments all nodes in set by amount specified
    relabelSetNodes :: Set Node -> Int -> Set Node
    relabelSetNodes nodes n = Set.fromList $ fmap (+n) (Set.toList nodes)