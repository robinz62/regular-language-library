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

-- | Wrapper around Char for purposes of generating arbitrary strings
--   restricted to the characters a-e
newtype ABC = ABC Char deriving (Eq, Ord, Show, Read)

instance Arbitrary ABC where
  arbitrary = elements (fmap ABC ['a', 'b', 'c'])

runDFATests :: IO ()
runDFATests = do runTestTT $ TestList [dfaAcceptTest1, dfaAcceptTest2]
                 quickCheck (withMaxSuccess 500 (prop_dfaUnion1 dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 500 (prop_dfaUnion1 dfaEmpty dfa2))
                 quickCheck (withMaxSuccess 500 (prop_dfaUnion1 dfa1 dfa2))
                 return ()

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

-- prop: (accept d1 s) || (accept d2 s) <==> accept (union d1 d2) s
prop_dfaUnion1 :: DFA Char -> DFA Char -> [ABC] -> Bool
prop_dfaUnion1 d1 d2 str =
  let s = fmap (\(ABC c) -> c) str in
    case union d1 d2 of
      Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
      Just d3 -> case (accept d1 s, accept d2 s) of
        (Just True, _)  -> accept d3 s == Just True
        (_, Just True)  -> accept d3 s == Just True
        (Just False, _) -> accept d3 s == Just False
        (_, Just False) -> accept d3 s == Just False
        _               -> isNothing (accept d3 s)

-- prop: (accept d1 s) || (accept d2 s) <==> accept (intersect d1 d2) s
prop_dfaIntersect1 :: DFA Char -> DFA Char -> [ABC] -> Bool
prop_dfaIntersect1 d1 d2 str =
  let s = fmap (\(ABC c) -> c) str in
    case intersect d1 d2 of
      Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
      Just d3 -> case (accept d1 s, accept d2 s) of
        (Just True, Just True)   -> accept d3 s == Just True
        (Just False, Just True)  -> accept d3 s == Just False
        (Just True, Just False)  -> accept d3 s == Just False
        (Just False, Just False) -> accept d3 s == Just False
        _                        -> isNothing (accept d3 s)

-- prop: (accept d1 s) && (not (accept d2 s)) <==> accept (minus d1 d2) s
prop_dfaMinus1 :: DFA Char -> DFA Char -> [ABC] -> Bool
prop_dfaMinus1 d1 d2 str =
  let s = fmap (\(ABC c) -> c) str in
    case minus d1 d2 of
      Nothing -> DFA.alphabet d1 /= DFA.alphabet d2
      Just d3 -> case (accept d1 s, accept d2 s) of
        (Just True, Just True)   -> accept d3 s == Just False
        (Just False, Just True)  -> accept d3 s == Just False
        (Just True, Just False)  -> accept d3 s == Just True
        (Just False, Just False) -> accept d3 s == Just False
        _                        -> isNothing (accept d3 s)

dfaFromStringTest :: Test
dfaFromStringTest = TestList
  [
    fromString "1\nabc\n0 a 0\n0 b 0\n0 c 0\n0\n\n" ~?= Just dfaEmpty,
    fromString "2\nabc\n0 a 0\n0 b 1\n0 c 1\n1 a 1\n1 b 1\n1 c 1\n0\n0" ~?= Just dfa1,
    fromString "4\nabc\n0 a 1\n0 b 3\n0 c 3\n1 a 3\n1 b 2\n1 c 3\n2 a 3\n2 b 3\n2 c 0\n3 a 3\n3 b 3\n3 c 3\n0\n0" ~?= Just dfa2,
    fromString "a\nabc\n0 a 0\n0 b 0\n0 c 0\n0\n\n" ~?= (Nothing :: Maybe (DFA Char)),
    fromString "1\nabc\n0 a z\n0 b 0\n0 c 0\n0\n\n" ~?= (Nothing :: Maybe (DFA Char)),
    fromString "1\nabc\n0 a 0\n0 b 0\n0 c 0\nz\n\n" ~?= (Nothing :: Maybe (DFA Char))
  ]
