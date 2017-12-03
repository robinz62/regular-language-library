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

type Node = Int

data DFA a = D (Set Node, Set a, Map (Node, a) Node, Node, Set Node)
  deriving (Eq, Show)

minimize :: DFA a -> DFA a
minimize = undefined

alphabet :: DFA a -> Set a
alphabet (D (_, s, _, _, _)) = s

-- bijection between pairs of ints to an int, given an (m x n) table of
-- numbers
pairToInt :: Int -> Int -> (Int, Int) -> Int
pairToInt m n (i1, i2) = n * i1 + i2

instance Matcher DFA where
  accept :: Ord a => DFA a -> [a] -> Maybe Bool
  accept dfa@(D (q, s, d, q_0, f)) str = eval dfa q_0 str where
    eval :: Ord a => DFA a -> Node -> [a] -> Maybe Bool
    eval (D (_, _, _, _, f)) curr [] = Just $ Set.member curr f
    eval dfa@(D (q, s, d, q_0, f)) curr (x:xs) =
      do next <- Map.lookup (curr, x) d
         eval dfa next xs

  -- TODO: error checking (commented below)
  --       make sure m and n are correct (not reversed)
  union :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  union dfa1@(D (q1, s1, d1, q_01, f1)) dfa2@(D (q2, s2, d2, q_02, f2)) =
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
                _ -> Nothing
              | (s1, s2) <- statePairs, c <- sigma ]
          newTableList = foldr (\x acc -> case acc of
            Nothing -> Nothing
            Just tail -> case x of
              Nothing -> Nothing
              Just head -> Just (head : tail)) (Just []) newTableListMaybe
      in case newTableList of
        Nothing -> Nothing
        Just newTable ->
          Just $ D (newStates,
                    s1,
                    Map.fromList newTable,
                    pairToInt m n (q_01, q_02),
                    Set.union f1 f2)


  intersect :: DFA a -> DFA a -> Maybe (DFA a)
  intersect = undefined

  minus :: DFA a -> DFA a -> Maybe (DFA a)
  minus = undefined

  fromString :: String -> Maybe (DFA Char)
  fromString s =
    let ls1 = lines s
    in do numNodes <- readMaybe (ls1 !! 0)
          let nodes = Set.fromList [0..(numNodes - 1)]
          let ls2 = drop 1 ls1
          let alphabet = readAlphabet (ls2 !! 0)
          let ls3 = drop 1 ls2
          transTable <- readTransitionTable $ take (numNodes * (Set.size alphabet)) ls3
          let ls4 = drop (numNodes * (Set.size alphabet)) ls3
          startState <- readMaybe (ls4 !! 0)
          let ls5 = drop 1 ls4
          finalStates <- readFinalStates (ls5 !! 0)
          return $ D (nodes, alphabet, transTable, startState, finalStates)

readAlphabet :: String -> Set Char
readAlphabet = Set.fromList

-- | reads in a correctly-formatted transition table
--   returns Nothing if transition table contains syntactic error
readTransitionTable :: [String] -> Maybe (Map (Node, Char) Node)
readTransitionTable lines =
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