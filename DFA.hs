module DFA where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Set(Set)
import qualified Data.Set as Set

import Matcher

type Node = Int

data DFA = D (Set Node, Set Char, Map (Node, Char) Node, Node, Set Node)

minimize :: DFA -> DFA
minimize = undefined

instance Matcher DFA where
  accept :: DFA -> String -> Bool
  accept = undefined

  union :: DFA -> DFA -> DFA
  union = undefined

  intersect :: DFA -> DFA -> DFA
  intersect = undefined

  minus :: DFA -> DFA -> DFA
  minus = undefined

  toNFA :: DFA -> NFA
  toNFA = undefined

  fromNFA :: NFA -> DFA
  fromNFA = undefined

  fromString :: String -> DFA
  fromString = undefined