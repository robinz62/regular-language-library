{-# LANGUAGE InstanceSigs #-}

module Types where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Text.Read

import Matcher

type Node = Int

data NFA a = N (Set Node,
                Set a,
                (Map (Node, a) (Set Node), Map Node (Set Node)),
                Node,
                Set Node)
  deriving (Eq, Show)

data DFA a = D (Set Node, Set a, Map (Node, a) Node, Node, Set Node)
  deriving (Eq, Show)

data Regex a = Single (Set a)
             | Alt (Regex a) (Regex a)
             | Seq (Regex a) (Regex a)
             | Star (Regex a)
             | Empty
             | Void
  deriving (Show, Eq)

-- regex with an associated alphabet
data RegexA a = R (Regex a, Set a) deriving (Eq, Show)