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

-- | Wrapper around Char for purposes of generating arbitrary strings
--   restricted to the characters a-e
newtype ABCDE = ABCDE Char deriving (Eq, Ord, Show, Read)

instance Arbitrary ABCDE where
  arbitrary = elements (fmap ABCDE ['a', 'b', 'c'])

---------------
-- some nfas --
---------------

-- L = {s | s has 2 a's} using alphabet {a, b}
-- note that this is also a dfa
nfa1 :: NFA Char
nfa1 = N (
  Set.fromList [0, 1, 2],
  Set.fromList ['a', 'b'],
  (Map.fromList [((0, 'a'), Set.singleton 1), ((0, 'b'), Set.singleton 0),
                 ((1, 'a'), Set.singleton 2), ((1, 'b'), Set.singleton 1),
                 ((2, 'a'), Set.singleton 2), ((2, 'b'), Set.singleton 2)],
   Map.empty),
  0,
  Set.singleton 2)

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
nfaAcceptTest2 = TestList
  [
    accept nfa2 "" ~?= Just True,
    accept nfa2 "a" ~?= Just False
  ]

nfaAcceptTest3 :: Test
nfaAcceptTest3 = TestList
  [
    accept nfa3 "" ~?= Just False,
    accept nfa3 "a" ~?= Just True,
    accept nfa3 "ba" ~?= Just True,
    accept nfa3 "aba" ~?= Just True,
    accept nfa3 "ab" ~?= Just False,
    accept nfa3 "aaba" ~?= Just True,
    accept nfa3 "abba" ~?= Just True,
    accept nfa3 "c" ~?= Nothing
  ]

-- quickcheck property for union of 2 nfas
-- nfas need to have the same alphabet
prop_nfaUnion1 :: NFA Char -> NFA Char -> [ABCDE] -> Bool
prop_nfaUnion1 n1 n2 str =
  let s = fmap (\(ABCDE c) -> c) str in
    case union n1 n2 of
      Nothing -> NFA.alphabet n1 /= NFA.alphabet n2
      Just n3 -> case (accept n1 s, accept n2 s) of
        (Just True, _)  -> accept n3 s == Just True
        (_, Just True)  -> accept n3 s == Just True
        (Just False, _) -> accept n3 s == Just False
        (_, Just False) -> accept n3 s == Just False
        _               -> isNothing (accept n3 s)

-- intersect nfa1 with empty language
prop_nfaIntersectTest1 :: NFA Char -> NFA Char -> [ABCDE] -> Bool
prop_nfaIntersectTest1 n1 n2 str =
  let s = fmap (\(ABCDE c) -> c) str in
    case intersect n1 n2 of
      Nothing -> NFA.alphabet n1 /= NFA.alphabet n2
      Just n3 -> case (accept n1 s, accept n2 s) of
        (Just True, _)  -> accept n3 s == Just True
        (_, Just True)  -> accept n3 s == Just True
        (Just False, _) -> accept n3 s == Just False
        (_, Just False) -> accept n3 s == Just False
        _               -> isNothing (accept n3 s)

-- prop: n1 accepts s && !(n2 accepts s) <==> minus n1 n2 accepts s
-- nfas must have the same alphabet
prop_nfaMinus1 :: NFA Char -> NFA Char -> [ABCDE] -> Bool
prop_nfaMinus1 n1 n2 str =
  let s = fmap (\(ABCDE c) -> c) str in
    case minus n1 n2 of
      Nothing -> NFA.alphabet n1 /= NFA.alphabet n2
      Just n3 -> case (accept n1 s, accept n2 s) of
        (Just True, _)  -> accept n3 s == Just True
        (_, Just True)  -> accept n3 s == Just True
        (Just False, _) -> accept n3 s == Just False
        (_, Just False) -> accept n3 s == Just False
        _               -> isNothing (accept n3 s)

nfaFromStringTest :: Test
nfaFromStringTest = TestList
  [
    fromString "3\nab\n6\n0 a 1\n0 b 0\n1 a 2\n1 b 1\n2 a 2\n2 b 2\n0\n2" ~?= Just nfa1,
    fromString "2\na\n1\n0 ep 1\n0\n1" ~?= Just nfa2
  ]