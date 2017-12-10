{-# LANGUAGE InstanceSigs #-}

module Types where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Text.Read

type Node = Int

data DFA a = D (Set Node, Set a, Map (Node, a) Node, Node, Set Node)
  deriving (Eq)

data NFA a = N (Set Node,
                Set a,
                (Map (Node, a) (Set Node), Map Node (Set Node)),
                Node,
                Set Node)
  deriving (Eq)

data Regex a = Single (Set a)
             | Alt (Regex a) (Regex a)
             | Seq (Regex a) (Regex a)
             | Star (Regex a)
             | Empty
             | Void
  deriving (Show, Eq)

-- regex with an associated alphabet
data RegexA a = R (Regex a, Set a) deriving (Eq, Show)

instance Show a => Show (DFA a) where
  show :: DFA a -> String
  show (D (q, s, d, q_0, f)) =
    "states:    0-" ++ (show $ (Set.size q - 1)) ++ "\n"
    ++ "alphabet: " ++ (show $ Set.toList s) ++ "\n"
    ++ "transition function:\n"
    ++ deltaToString d
    ++ "start state: " ++ show q_0 ++ "\n"
    ++ "final states: " ++ (show $ Set.toList f)
    where
      deltaToString :: Show a => Map (Node, a) Node -> String
      deltaToString = Map.foldrWithKey (\(u, c) v acc ->
        show u ++ " " ++ show c ++ " -> " ++ show v ++ "\n" ++ acc) ""

instance Show a => Show (NFA a) where
  show :: NFA a -> String
  show (N (q, s, (d, de), q_0, f)) =
    "states:    0-" ++ (show $ (Set.size q - 1)) ++ "\n"
    ++ "alphabet: " ++ (show $ Set.toList s) ++ "\n"
    ++ "transition function:\n"
    ++ deltaToString d
    ++ deltaEpToString de
    ++ "start state: " ++ show q_0 ++ "\n"
    ++ "final states: " ++ (show $ Set.toList f)
    where
      deltaToString :: Show a => Map (Node, a) (Set Node) -> String
      deltaToString = Map.foldrWithKey (\(u, c) vs acc ->
        show u ++ " " ++ show c ++ " -> " ++ show (Set.toList vs) ++ "\n" ++ acc) ""
      deltaEpToString :: Map Node (Set Node) -> String
      deltaEpToString = Map.foldrWithKey (\u vs acc ->
        show u ++ " " ++ "ep" ++ " -> " ++ show (Set.toList vs) ++ "\n" ++ acc) ""