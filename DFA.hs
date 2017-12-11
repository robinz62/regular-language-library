{-# LANGUAGE InstanceSigs #-}

module DFA where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Text.Read

import Matcher
import MatcherParsers
import Operations
import Parser
import Types

alphabet :: DFA a -> Set a
alphabet (D (_, s, _, _, _)) = s

eval :: Ord a => DFA a -> Node -> [a] -> Maybe Bool
eval (D (_, _, _, _, f)) curr [] = Just $ Set.member curr f
eval dfa@(D (q, s, d, q_0, f)) curr (x:xs) =
  do next <- Map.lookup (curr, x) d
     eval dfa next xs

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
  fromString s = case doParse dfaP s of
    []           -> Nothing
    (res, ""):xs -> let D (q, sigma, delta, q_0, f) = res in
                    if Set.size q * Set.size sigma /= Map.size delta
                      then Nothing
                      else Just res
    _            -> Nothing