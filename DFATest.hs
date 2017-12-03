module DFATest where
    
import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

import Matcher
import DFA

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

-----------
-- tests --
-----------

-- accept tests for dfa1
dfaAcceptTest1 :: Test
dfaAcceptTest1 = TestList
  [
    accept dfa1 "" ~?= Just True,
    accept dfa1 "a" ~?= Just True,
    accept dfa1 "aaa" ~?= Just True,
    accept dfa1 "b" ~?= Just False,
    accept dfa1 "ab" ~?= Just False,
    accept dfa1 "aaba" ~?= Just False,
    accept dfa1 "aac" ~?= Just False,
    accept dfa1 "z" ~?= Nothing,
    accept dfa1 "aaz" ~?= Nothing
  ]

-- accept tests for dfa2
dfaAcceptTest2 :: Test
dfaAcceptTest2 = TestList
  [
    accept dfa2 "" ~?= Just True,
    accept dfa2 "abc" ~?= Just True,
    accept dfa2 "abcabc" ~?= Just True,
    accept dfa2 "a" ~?= Just False,
    accept dfa2 "abca" ~?= Just False,
    accept dfa2 "d" ~?= Nothing
  ]

-- union dfa1 with empty language
dfaUnionTest1 :: Test
dfaUnionTest1 = undefined

-- prop: d1 accepts s || d2 accepts s <==> union d1 d2 accepts s
-- dfas need to have the same alphabet
prop_dfaUnion1 :: DFA Char -> DFA Char -> String -> Bool
prop_dfaUnion1 d1 d2 s = case union d1 d2 of
  Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
  Just d3 -> case (accept d1 s, accept d2 s) of
    (Just True, _)  -> accept d3 s == Just True
    (_, Just True)  -> accept d3 s == Just True
    (Just False, _) -> accept d3 s == Just False
    (_, Just False) -> accept d3 s == Just False
    _               -> isNothing (accept d3 s)

-- intersect dfa1 with empty language
dfaIntersectTest1 :: Test
dfaIntersectTest1 = undefined

-- prop: d1 accepts s && d2 accepts s <==> intersect d1 d2 accepts s
-- dfas need to have the same alphabet
prop_dfaIntersect1 :: DFA Char -> DFA Char -> String -> Bool
prop_dfaIntersect1 d1 d2 s = case intersect d1 d2 of
  Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
  Just d3 -> case (accept d1 s, accept d2 s) of
    (Just True, Just True)   -> accept d3 s == Just True
    (Just False, Just True)  -> accept d3 s == Just False
    (Just True, Just False)  -> accept d3 s == Just False
    (Just False, Just False) -> accept d3 s == Just False
    _                        -> isNothing (accept d3 s)

-- test here about set minus
dfaMinusTest1 :: Test
dfaMinusTest1 = undefined

-- prop: d1 accepts s && not (d2 accepts s) <==> minus d1 d2 accepts s
-- dfas need to have the same alphabet
prop_dfaMinus1 :: DFA Char -> DFA Char -> String -> Bool
prop_dfaMinus1 d1 d2 s = case minus d1 d2 of
  Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
  Just d3 -> case (accept d1 s, accept d2 s) of
    (Just True, Just True)   -> accept d3 s == Just False
    (Just False, Just True)  -> accept d3 s == Just False
    (Just True, Just False)  -> accept d3 s == Just True
    (Just False, Just False) -> accept d3 s == Just False
    _                        -> isNothing (accept d3 s)

dfaFromStringTest :: Test
dfaFromStringTest = TestList [
  fromString "1\nabc\n0 a 0\n0 b 0\n0 c 0\n0\n\n" ~?= Just dfaEmpty,
  fromString "2\nabc\n0 a 0\n0 b 1\n0 c 1\n1 a 1\n1 b 1\n1 c 1\n0\n0" ~?= Just dfa1,
  fromString "4\nabc\n0 a 1\n0 b 3\n0 c 3\n1 a 3\n1 b 2\n1 c 3\n2 a 3\n2 b 3\n2 c 0\n3 a 3\n3 b 3\n3 c 3\n0\n0" ~?= Just dfa2,
  fromString "a\nabc\n0 a 0\n0 b 0\n0 c 0\n0\n\n" ~?= (Nothing :: Maybe (DFA Char)),
  fromString "1\nabc\n0 a z\n0 b 0\n0 c 0\n0\n\n" ~?= (Nothing :: Maybe (DFA Char)),
  fromString "1\nabc\n0 a 0\n0 b 0\n0 c 0\nz\n\n" ~?= (Nothing :: Maybe (DFA Char))
  ]