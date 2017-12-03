{-# LANGUAGE InstanceSigs #-}

module NFA where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Matcher

type Node = Int

data NFA = N (Set Node, Set Char, Map (Node, Char) (Set Node), Node, Set Node)
  deriving (Eq)

alphabet :: NFA -> Set a
alphabet (N (_, s, _, _, _)) = s

instance Matcher NFA where
  accept :: NFA -> [a] -> Maybe Bool
  accept = undefined

  union :: NFA -> NFA -> Maybe NFA
  union = undefined

  intersect :: NFA -> NFA -> Maybe NFA
  intersect = undefined

  minus :: NFA -> NFA -> Maybe NFA
  minus = undefined

  fromString :: String -> Maybe NFA
  fromString = undefined