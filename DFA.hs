{-# LANGUAGE InstanceSigs #-}

module DFA where

import Data.List.Split

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Text.Read

import Matcher
import Operations
import Types

alphabet :: DFA a -> Set a
alphabet (D (_, s, _, _, _)) = s

eval :: Ord a => DFA a -> Node -> [a] -> Maybe Bool
eval (D (_, _, _, _, f)) curr [] = Just $ Set.member curr f
eval dfa@(D (q, s, d, q_0, f)) curr (x:xs) =
  do next <- Map.lookup (curr, x) d
     eval dfa next xs

instance Show a => Show (DFA a) where
  show :: DFA a -> String
  show (D (q, s, d, q_0, f)) =
    "states:    0-" ++ (show $ Set.size q) ++ "\n"
    ++ "alphabet: " ++ (show $ Set.toList s) ++ "\n"
    ++ "transition function:\n"
    ++ deltaToString d
    ++ "start state: " ++ show q_0 ++ "\n"
    ++ "final states: " ++ (show $ Set.toList f)
    where
      deltaToString :: Show a => Map (Node, a) Node -> String
      deltaToString = Map.foldrWithKey (\(u, c) v acc ->
        show u ++ " " ++ show c ++ " -> " ++ show v ++ "\n" ++ acc) ""

instance Matcher DFA where
  accept :: Ord a => DFA a -> [a] -> Maybe Bool
  accept dfa@(D (q, s, d, q_0, f)) str = eval dfa q_0 str

  union :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  union = dfaUnion

  intersect :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  intersect = dfaIntersect

  minus :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  minus = dfaMinus

  concat :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  concat = undefined

  kStar :: Ord a => DFA a -> Maybe (DFA a)
  kStar = undefined

  fromString :: String -> Maybe (DFA Char)
  fromString s =
    let ls1 = lines s
    in do numNodes <- readMaybe (ls1 !! 0)
          let nodes = Set.fromList [0..(numNodes - 1)]
          let ls2 = drop 1 ls1
          let alphabet = readAlphabet (ls2 !! 0)
          let ls3 = drop 1 ls2
          transTable <- readDFATransitionTable $ take (numNodes * (Set.size alphabet)) ls3
          let ls4 = drop (numNodes * (Set.size alphabet)) ls3
          startState <- readMaybe (ls4 !! 0)
          let ls5 = drop 1 ls4
          finalStates <- readFinalStates (ls5 !! 0)
          return $ D (nodes, alphabet, transTable, startState, finalStates)

readAlphabet :: String -> Set Char
readAlphabet = Set.fromList

-- | reads in a correctly-formatted transition table
--   returns Nothing if transition table contains syntactic error
readDFATransitionTable :: [String] -> Maybe (Map (Node, Char) Node)
readDFATransitionTable lines =
  foldr (\line map ->
    do m <- map
       case splitOn " " line of
         [x, [c], y] -> do xInt <- readMaybe x
                           yInt <- readMaybe y
                           return $ Map.insert (xInt, c) yInt m
         _         -> Nothing)
    (Just Map.empty)
    lines

-- | reads in string containing space-separated integers and returns
--   a Set of Nodes
--   returns Nothing if input has syntactic error
readFinalStates :: String -> Maybe (Set Node)
readFinalStates line =
  let strStates = splitOn " " line
  in do intStates <- strToIntStates strStates
        return (Set.fromList intStates)
  where
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
