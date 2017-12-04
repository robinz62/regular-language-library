-- comments
-- go
-- here

{-# LANGUAGE InstanceSigs #-}

module Regex where

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Matcher

data Regex a = Single (Set a)
             | Alt (Regex a) (Regex a)
             | Seq (Regex a) (Regex a)
             | Star (Regex a)
             | Empty
             | Void
  deriving (Show, Eq)

single :: Ord a => a -> Regex a
single = Single . Set.singleton

singles :: Ord a => [a] -> Regex a
singles = Single . Set.fromList

-- all decompositions of a string into two pieces
splitPair :: [a] -> [([a], [a])]
splitPair list = do i <- [0..(length list)]
                    return (splitAt i list)

-- all decompositions of a string into multi-part (nonempty) pieces
parts :: [a] -> [[[a]]]
parts [] = [[]]
parts (h : t) = do ans <- parts t
                   case ans of
                     [] -> [[[h]]]
                     a : as -> [[h] : ans, ((h : a) : as)]

acceptRegex :: Ord a => Regex a -> [a] -> Bool
acceptRegex (Single set) s = case s of
  [c] -> Set.member c set
  _ -> False
acceptRegex (Alt r1 r2) s = acceptRegex r1 s || acceptRegex r2 s
acceptRegex (Seq r1 r2) s = any (\(l, r) -> acceptRegex r1 l && acceptRegex r2 r) (splitPair s)
acceptRegex (Star r) s = any (\try -> all (\substr -> acceptRegex r substr) try) (parts s)
acceptRegex Empty s = s == []
acceptRegex Void _ = False

-- regex with an associated alphabet
data RegexA a = R (Regex a, Set a) deriving (Eq, Show)

instance Matcher RegexA where
  accept :: Ord a => RegexA a -> [a] -> Maybe Bool
  accept (R (regex, alphabet)) str =
    if foldr (\x acc -> Set.member x alphabet && acc) True str
      then Just $ acceptRegex regex str
      else Nothing

  union :: Ord a => RegexA a -> RegexA a -> Maybe (RegexA a)
  union (R (r1, alpha1)) (R (r2, alpha2)) =
    if alpha1 /= alpha2
      then Nothing
      else Just $ R (Alt r1 r2, alpha1)

  intersect :: Ord a => RegexA a -> RegexA a -> Maybe (RegexA a)
  intersect r1 r2 = undefined -- Just $ nfaToRegex (intersect (regexToNFA r1) (regexToNFA r2))

  minus :: Ord a => RegexA a -> RegexA a -> Maybe (RegexA a)
  minus = undefined

  fromString :: String -> Maybe (RegexA Char)
  fromString = undefined