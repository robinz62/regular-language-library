{-# LANGUAGE InstanceSigs #-}

module NFA where

import Data.List.Split

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Text.Read

import qualified DFA as DFA
import ConvertMatcher
import Matcher
import Types

alphabet :: NFA a -> Set a
alphabet (N (_, s, _, _, _)) = s

instance Matcher NFA where
  accept :: Ord a => NFA a -> [a] -> Maybe Bool
  accept nfa@(N (q, s, (d, de), q_0, f)) str =
    eval nfa (epsilonClosure (Set.singleton q_0)) str where
      eval :: Ord a => NFA a -> Set Node -> [a] -> Maybe Bool
      eval (N (_, _, _, _, f)) curr [] = Just $ any (\x -> Set.member x f) curr
      eval nfa@(N (q, s, (d, de), q_0, f)) curr (x:xs) =
        if not $ Set.member x s
          then Nothing
          else let next = ((Set.unions
                            . catMaybes
                            . (fmap (\st -> Map.lookup (st, x) d))
                            . Set.toList) curr)
              in eval nfa (epsilonClosure next) xs
      epsilonClosure :: Set Node -> Set Node
      epsilonClosure curr =
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
          else epsilonClosure next

  -- essentially creates a new node as start state with epsilon transitions to
  -- the previous nfa start states
  union :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  union nfa1@(N (q1, s1, (d1, de1), q_01, f1)) nfa2 =
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

  -- converts to a DFA to perform intersection, then converts back
  intersect :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  intersect n1 n2 = do dfa <- intersect (nfaToDFA n1) (nfaToDFA n2)
                       return $ dfaToNFA dfa

  minus :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  minus n1 n2 = do dfa <- minus (nfaToDFA n1) (nfaToDFA n2)
                   return $ dfaToNFA dfa

  fromString :: String -> Maybe (NFA Char)
  fromString s =
    let ls1 = lines s
    in do numNodes <- readMaybe (ls1 !! 0)
          let nodes = Set.fromList [0..(numNodes - 1)]
          let ls2 = drop 1 ls1
          let alphabet = readAlphabet (ls2 !! 0)
          let ls3 = drop 1 ls2
          numTransitions <- readMaybe (ls3 !! 0)
          let ls4 = drop 1 ls3
          transTable <- readNFATransitionTable $ take numTransitions ls4
          let ls5 = drop numTransitions ls4
          startState <- readMaybe (ls5 !! 0)
          let ls6 = drop 1 ls5
          finalStates <- readFinalStates (ls6 !! 0)
          return $ N (nodes, alphabet, transTable, startState, finalStates)

readAlphabet :: String -> Set Char
readAlphabet = Set.fromList

-- | reads in a correctly-formatted transition table
--   returns Nothing if transition table contains syntactic error
readNFATransitionTable :: [String] -> Maybe ((Map (Node, Char) (Set Node), Map Node (Set Node)))
readNFATransitionTable lines =
  foldr (\line map ->
    do (m, me) <- map
       case splitOn " " line of
         x : "ep" : ys ->
           do xInt <- readMaybe x
              ysInt <- strToIntStates ys
              return $ (m, Map.insert xInt (Set.fromList ysInt) me)
         x : [c] : ys ->
           do xInt <- readMaybe x
              ysInt <- strToIntStates ys
              return $ (Map.insert (xInt, c) (Set.fromList ysInt) m, me)
         _            -> Nothing)
    (Just (Map.empty, Map.empty))
    lines

-- | reads in string containing space-separated integers and returns
--   a Set of Nodes
--   returns Nothing if input has syntactic error
readFinalStates :: String -> Maybe (Set Node)
readFinalStates line =
  let strStates = splitOn " " line
  in do intStates <- strToIntStates strStates
        return (Set.fromList intStates)

-- | transforms a list of strings representing integers to a list of those
--   integers (as Node)
--   returns Nothing if parse error
strToIntStates :: [String] -> Maybe [Node]
strToIntStates list =
  foldr (\x acc ->
    if x == ""
      then acc
      else do i <- readMaybe x
              rest <- acc
              return (i : rest))
    (Just [])
    list