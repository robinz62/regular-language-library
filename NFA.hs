{-# LANGUAGE InstanceSigs #-}

module NFA where

import Data.List.Split

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Text.Read

import ConvertMatcher
import Matcher
import Operations
import Types

alphabet :: NFA a -> Set a
alphabet (N (_, s, _, _, _)) = s

eval :: Ord a => NFA a -> Set Node -> [a] -> Maybe Bool
eval (N (_, _, _, _, f)) curr [] = Just $ any (\x -> Set.member x f) curr
eval nfa@(N (q, s, (d, de), q_0, f)) curr (x:xs) =
  if not $ Set.member x s
    then Nothing
    else let next = ((Set.unions
                      . catMaybes
                      . (fmap (\st -> Map.lookup (st, x) d))
                      . Set.toList) curr)
        in eval nfa (epsilonClosure nfa next) xs

instance Matcher NFA where
  accept :: Ord a => NFA a -> [a] -> Maybe Bool
  accept nfa@(N (q, s, (d, de), q_0, f)) str =
    eval nfa (epsilonClosure nfa (Set.singleton q_0)) str

  union :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  union = nfaUnion

  -- converts to a DFA to perform intersection, then converts back
  intersect :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  intersect n1 n2 = do dfa <- dfaIntersect (nfaToDFA n1) (nfaToDFA n2)
                       return $ dfaToNfa dfa

  -- converts to a DFA to perform minus, then converts back
  minus :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  minus n1 n2 = do dfa <- dfaMinus (nfaToDFA n1) (nfaToDFA n2)
                   return $ dfaToNfa dfa

  concat :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  concat = nfaConcat

  kStar :: Ord a => NFA a -> Maybe (NFA a)
  kStar = nfaKStar

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