module SampleMatchers where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set

import Matcher
import DFA
import NFA
import Regex
import Types

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


---------------
-- some nfas --
---------------

-- L = {s | s has 2 a's} using alphabet {a, b, c}
-- note that this is also a dfa
nfa1 :: NFA Char
nfa1 = N (
  Set.fromList [0, 1, 2],
  Set.fromList ['a', 'b', 'c'],
  (Map.fromList [((0, 'a'), Set.singleton 1), ((0, 'b'), Set.singleton 0),
                 ((1, 'a'), Set.singleton 2), ((1, 'b'), Set.singleton 1),
                 ((2, 'a'), Set.singleton 2), ((2, 'b'), Set.singleton 2)],
   Map.empty),
  0,
  Set.singleton 2)

-- L = {""} with alphabet {a, b, c}
-- for testing epsilon transitions
nfa2 :: NFA Char
nfa2 = N (
  Set.fromList [0, 1],
  Set.fromList ['a', 'b', 'c'],
  (Map.empty,
   Map.singleton 0 (Set.singleton 1)),
  0,
  Set.singleton 1)

-- L = {{a, b}^n a | n >= 0}
-- for testing nondeterminism
nfa3 :: NFA Char
nfa3 = N (
  Set.fromList [0, 1],
  Set.fromList ['a', 'b', 'c'],
  (Map.fromList [((0, 'a'), Set.fromList [0, 1]), ((0, 'b'), Set.singleton 0)],
   Map.empty),
  0,
  Set.singleton 1)

-- L = (abc)* | {abc}*a
nfa4 :: NFA Char
nfa4 = N (
  Set.fromList [0..5],
  Set.fromList ['a', 'b', 'c'],
  (Map.fromList
    [((1, 'a'), Set.singleton 2), ((2, 'b'), Set.singleton 3),
     ((3, 'c'), Set.singleton 1), ((4, 'a'), Set.fromList [4, 5]),
     ((4, 'b'), Set.singleton 4), ((4, 'c'), Set.singleton 4)],
   Map.singleton 0 (Set.fromList [1, 4])),
  0,
  Set.fromList [1, 5])

------------------
-- some regexes --
------------------

-- only accepts single characters
reg1 :: RegexA Char
reg1 = R (singles "abc", Set.fromList "abc")

-- accepts anything
reg2 :: RegexA Char
reg2 = R (Star $ singles "abc", Set.fromList "abc")

-- accepts anything that ends with an 'a'
reg3 :: RegexA Char
reg3 = R (Seq (Star $ singles "abc") (single 'a'), Set.fromList "abc")

-- accepts single characters or strings that end with 'a'
reg4 :: RegexA Char
reg4 = R (Alt (singles "abc") (Seq (Star $ singles "abc") (single 'a')),
          Set.fromList "abc")