{-# LANGUAGE InstanceSigs #-}

module DFA where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Matcher

type Node = Int

data DFA = D (Set Node, Set Char, Map (Node, Char) Node, Node, Set Node)
  deriving (Eq)

minimize :: DFA -> DFA
minimize = undefined

alphabet :: DFA -> Set Char
alphabet (D (_, s, _, _, _)) = s

instance Matcher DFA where
  accept :: DFA -> String -> Maybe Bool
  accept = undefined

  union :: DFA -> DFA -> Maybe DFA
  union = undefined

  intersect :: DFA -> DFA -> Maybe DFA
  intersect = undefined

  minus :: DFA -> DFA -> Maybe DFA
  minus = undefined

  fromString :: String -> Maybe DFA
  fromString = undefined