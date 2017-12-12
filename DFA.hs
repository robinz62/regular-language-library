{-# LANGUAGE InstanceSigs #-}

module DFA () where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Text.Read

import ConvertMatcher
import Matcher
import MatcherParsers
import Operations
import Parser
import Types

eval :: Ord a => DFA a -> Node -> [a] -> Maybe Bool
eval (D (_, _, _, _, f)) curr [] = Just $ Set.member curr f
eval dfa@(D (q, s, d, q_0, f)) curr (x:xs) =
  do next <- Map.lookup (curr, x) d
     eval dfa next xs

instance Matcher DFA where
  alphabet :: Ord a => DFA a -> Set a
  alphabet (D (_, s, _, _, _)) = s

  accept :: Ord a => DFA a -> [a] -> Maybe Bool
  accept dfa@(D (q, s, d, q_0, f)) str = eval dfa q_0 str

  union :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  union = dfaUnion

  intersect :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  intersect = dfaIntersect

  minus :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  minus = dfaMinus

  concat :: Ord a => DFA a -> DFA a -> Maybe (DFA a)
  concat dfa1 dfa2 = do nfa1 <- dfaToNfa dfa1
                        nfa2 <- dfaToNfa dfa2
                        nfa' <- nfaConcat nfa1 nfa2
                        nfaToDfa nfa'

  kStar :: Ord a => DFA a -> Maybe (DFA a)
  kStar dfa = do nfa  <- dfaToNfa dfa
                 nfa' <- nfaKStar nfa
                 nfaToDfa nfa'

  fromString :: String -> Maybe (DFA Char)
  fromString s = case doParse dfaP s of
    []           -> Nothing
    (res, ""):xs -> let D (q, sigma, delta, q_0, f) = res in
                    if Set.size q * Set.size sigma /= Map.size delta
                      then Nothing
                      else Just res
    _            -> Nothing