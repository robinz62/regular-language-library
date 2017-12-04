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