module Tests where

import DFA
import NFA
import Regex

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

-- DFA Tests

-- only accepts strings with no b's from alphabet {a, b}
dfa1 :: DFA
dfa1 = D (Set.fromList [0, 1],
  Set.fromList "ab",
  Map.fromList [((0, 'a'), 0), ((0, 'b'), 1), ((1, a), 1), ((1, b), 1)],
  0,
  Set.singleton 1)

acceptTest :: Test
acceptTest = TestList [
  
  ]