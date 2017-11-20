module NFA where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Set(Set)
import qualified Data.Set as Set

import Matcher

type Node = Int

data NFA = N (Set Node, Set Char, Map (Node, Char) (Set Node), Node, Set Node)

minimize :: NFA -> NFA
minimize = undefined

instance Matcher NFA where
  accept :: NFA -> String -> Bool
  accept = undefined

  union :: NFA -> NFA -> NFA
  union = undefined

  intersect :: NFA -> NFA -> NFA
  intersect = undefined

  minus :: NFA -> NFA -> NFA
  minus = undefined

  toNFA :: NFA -> NFA
  toNFA = undefined

  fromNFA :: NFA -> NFA
  fromNFA = undefined

  fromString :: String -> NFA
  fromString = undefined