module NFATest where
  
import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

import Matcher
import NFA

-- L = {s | s has 2 a's} using alphabet {a, b}
-- note that this is also a dfa
nfa1 :: NFA
nfa1 = N (
  Set.fromList [0, 1],
  Set.fromList ['a', 'b'],
  Map.fromList [((0, 'a'), Set.singleton 1), ((0, 'b'), Set.singleton 0),
                ((1, 'a'), Set.singleton 2), ((1, 'b'), Set.singleton 0),
                ((2, 'a'), Set.singleton 2), ((2, 'b'), Set.singleton 2)],
  0,
  Set.singleton 2)

-- L = {""} with alphabet {a}
-- for testing epsilon transitions
nfa2 :: NFA
nfa2 = undefined

-- L = {{a, b}^n a | n >= 0}
-- for testing nondeterminism
nfa3 :: NFA
nfa3 = undefined

-- accept tests for nfa1
nfaAcceptTest1 :: Test
nfaAcceptTest1 = TestList
  [
    accept nfa1 "" ~?= Just False,
    accept nfa1 "a" ~?= Just False,
    accept nfa1 "ab" ~?= Just False,
    accept nfa1 "abb" ~?= Just False,
    accept nfa1 "aa" ~?= Just True,
    accept nfa1 "baba" ~?= Just True,
    accept nfa1 "babab" ~?= Just True,
    accept nfa1 "aaaa" ~?= Just True,
    accept nfa1 "bbbb" ~?= Just False,
    accept nfa1 "aac" ~?= Nothing
  ]

-- accept tests for nfa2
nfaAcceptTest2 :: Test
nfaAcceptTest2 = undefined

-- accept tests for nfa3
nfaAcceptTest3 :: Test
nfaAcceptTest3 = undefined

-- quickcheck property for union of 2 nfas
-- nfas need to have the same alphabet
prop_nfaUnion1 :: NFA -> NFA -> String -> Bool
prop_nfaUnion1 = undefined

-- intersect nfa1 with empty language
nfaIntersectTest1 :: Test
nfaIntersectTest1 = undefined

-- prop: n1 accepts s && n2 accepts s <==> intersect n1 n2 accepts s
-- nfas must have the same alphabet
prop_nfaIntersect1 :: NFA -> NFA -> String -> Bool
prop_nfaIntersect1 = undefined

-- test here about set minus
nfaMinusTest1 :: Test
nfaMinusTest1 = undefined

-- prop: n1 accepts s && !(n2 accepts s) <==> minus n1 n2 accepts s
-- nfas must have the same alphabet
prop_nfaMinus1 :: NFA -> NFA -> String -> Bool
prop_nfaMinus1 = undefined

nfaFromStringTest1 :: Test
nfaFromStringTest1 = undefined

nfaFromStringTest2 :: Test
nfaFromStringTest2 = undefined