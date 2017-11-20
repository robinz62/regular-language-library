-- comments
-- go
-- here

module Regex where

import Data.Set (Set)
import qualified Data.Set as Set

import Matcher

data Regex = Char (Set Char)
           | Alt Regex Regex
           | Seq Regex Regex
           | Star Regex
           | Empty
           | Void
  deriving (Show, Eq)

char :: Char -> Regex
char = Char . Set.singleton

chars :: String -> Regex
chars = Char . Set.fromList





instance Matcher Regex where
  accept :: Regex -> String -> Bool
  accept = undefined

  union :: Regex -> Regex -> Regex
  union = undefined

  intersect :: Regex -> Regex -> Regex
  intersect = undefined

  minus :: Regex -> Regex -> Regex
  minus = undefined

  toNFA :: Regex -> NFA
  toNFA = undefined

  fromNFA :: NFA -> Regex
  fromNFA = undefined

  fromString :: String -> Regex
  fromString = undefined