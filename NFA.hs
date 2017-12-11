{-# LANGUAGE InstanceSigs #-}

module NFA where

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
  alphabet :: Ord a => NFA a -> Set a
  alphabet (N (_, s, _, _, _)) = s

  accept :: Ord a => NFA a -> [a] -> Maybe Bool
  accept nfa@(N (q, s, (d, de), q_0, f)) str =
    eval nfa (epsilonClosure nfa (Set.singleton q_0)) str

  union :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  union = nfaUnion

  -- converts to a DFA to perform intersection, then converts back
  intersect :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  intersect n1 n2 = do dfa1 <- nfaToDfa n1
                       dfa2 <- nfaToDfa n2
                       dfa <- dfaIntersect dfa1 dfa2
                       dfaToNfa dfa

  -- converts to a DFA to perform minus, then converts back
  minus :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  minus n1 n2 = do dfa1 <- nfaToDfa n1
                   dfa2 <- nfaToDfa n2
                   dfa <- dfaMinus dfa1 dfa2
                   dfaToNfa dfa

  concat :: Ord a => NFA a -> NFA a -> Maybe (NFA a)
  concat = nfaConcat

  kStar :: Ord a => NFA a -> Maybe (NFA a)
  kStar = nfaKStar

  fromString :: String -> Maybe (NFA Char)
  fromString s = case doParse nfaP s of
    []           -> Nothing
    (res, ""):xs -> Just res
    _            -> Nothing