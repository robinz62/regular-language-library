module ConvertMatcherTest where
    
import Data.Map(Map)
import qualified Data.Map as Map

import Matcher
import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

import Types

import ConvertMatcher

---------------
-- some dfas --
---------------

-- L = {} using alphabet {a, b, c}
dfaEmpty :: DFA Char
dfaEmpty = D (
  Set.singleton 0,
  Set.fromList ['a', 'b', 'c'],
  Map.fromList [((0, 'a'), 0), ((0, 'b'), 0), ((0, 'c'), 0)],
  0,
  Set.empty)

-- L = {a^n | n >= 0} using alphabet {a, b, c}
dfa1 :: DFA Char
dfa1 = D (
  Set.fromList [0, 1],
  Set.fromList ['a', 'b', 'c'],
  Map.fromList [((0, 'a'), 0), ((0, 'b'), 1), ((0, 'c'), 1),
                ((1, 'a'), 1), ((1, 'b'), 1), ((1, 'c'), 1)],
  0,
  Set.singleton 0)

-- L = {(abc)^n | n >= 0} using alphabet {a, b, c}
dfa2 :: DFA Char
dfa2 = D (
  Set.fromList [0..3],
  Set.fromList ['a', 'b', 'c'],
  Map.fromList [((0, 'a'), 1), ((0, 'b'), 3), ((0, 'c'), 3),
                ((1, 'a'), 3), ((1, 'b'), 2), ((1, 'c'), 3),
                ((2, 'a'), 3), ((2, 'b'), 3), ((2, 'c'), 0),
                ((3, 'a'), 3), ((3, 'b'), 3), ((3, 'c'), 3)],
  0,
  Set.singleton 0)

-- L = {""} with alphabet {a}
-- for testing epsilon transitions
nfa2 :: NFA Char
nfa2 = N (
  Set.fromList [0, 1],
  Set.fromList ['a'],
  (Map.empty,
   Map.singleton 0 (Set.singleton 1)),
  0,
  Set.singleton 1)

-- L = {{a, b}^n a | n >= 0}
-- for testing nondeterminism
nfa3 :: NFA Char
nfa3 = N (
  Set.fromList [0, 1],
  Set.fromList ['a', 'b'],
  (Map.fromList [((0, 'a'), Set.fromList [0, 1]), ((0, 'b'), Set.singleton 0)],
   Map.empty),
  0,
  Set.singleton 1)

-----------
-- tests --
-----------

newtype ABCDE = ABCDE Char deriving (Eq, Ord, Show, Read)

instance Arbitrary ABCDE where
  arbitrary = elements (fmap ABCDE ['a', 'b', 'c'])

nfaToDFATest :: Test
nfaToDFATest = 

-- prop: d1 accepts s <==> d2 accepts s
-- dfas need to have the same alphabet
prop_dfaNFAEquals :: DFA Char -> NFA Char -> [ABCDE] -> Bool
prop_dfaNFAEquals d1 d2 str =
  let s = fmap (\(ABCDE c) -> c) str in
    accept d1 s == accept d2 s