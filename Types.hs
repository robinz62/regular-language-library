{-# LANGUAGE InstanceSigs #-}

module Types where

import qualified Data.List as L
import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.QuickCheck

import Text.Read

-----------------
-- for testing --
-----------------

-- | Wrapper around Char for purposes of generating arbitrary strings
--   restricted to the characters a-c
newtype ABC = ABC Char deriving (Eq, Ord, Show, Read)

instance Arbitrary ABC where
  arbitrary = elements (fmap ABC ['a', 'b', 'c'])

-----------
-- types --
-----------

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
  deriving (Eq)

-- regex with an associated alphabet
data RegexA a = R (Regex a, Set a) deriving (Show, Eq)

-- more efficient constructors
rStar :: Regex a -> Regex a
rStar (Star x) = Star x
rStar Empty    = Empty
rStar Void     = Empty
rStar r        = Star r

rSeq :: Regex a -> Regex a -> Regex a
rSeq r Empty = r
rSeq Empty r = r
rSeq r Void = Void
rSeq Void r = Void
rSeq r1 r2 = Seq r1 r2

rAlt :: Regex a -> Regex a -> Regex a
rAlt r Void = r
rAlt Void r = r
rAlt r1 r2 = Alt r1 r2

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

instance Show a => Show (Regex a) where
  show :: Regex a -> String
  show (Single s) = L.concat $ L.intersperse "|" (map show (Set.toList s))
  show (Alt r1 r2) = "(" ++ show r1 ++ ")|(" ++ show r2 ++ ")"
  show (Seq r1 r2) = "(" ++ show r1 ++ ").(" ++ show r2 ++ ")"
  show (Star r) = "(" ++ show r ++ ")*"
  show Empty = "Empty"
  show Void = "Void"