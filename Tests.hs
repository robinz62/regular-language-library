{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

module Tests where

import qualified Data.List as L

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

import ConvertMatcher
import Matcher
import DFA
import NFA
import Regex
import Types
import SampleMatchers

-- TODO: compile all test cases
runAllTests :: IO ()
runAllTests = do runDFATests
                 runNFATests
                 runRegexTests

-- DFA tests
runDFATests :: IO ()
runDFATests = do runTestTT $ TestList [dfaAcceptTest1,
                                       dfaAcceptTest2,
                                       dfaFromStringTest]
                 quickCheck (withMaxSuccess 500 (prop_union dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 500 (prop_union dfaEmpty dfa2))
                 quickCheck (withMaxSuccess 500 (prop_union dfa1 dfa2))
                 quickCheck (withMaxSuccess 500 (prop_intersect dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 500 (prop_intersect dfa1 dfa1))
                 quickCheck (withMaxSuccess 500 (prop_intersect dfa1 dfa2))
                 quickCheck (withMaxSuccess 500 (prop_minus dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 500 (prop_minus dfa1 dfaEmpty))
                 quickCheck (withMaxSuccess 500 (prop_minus dfa2 dfa1))
                 return ()

-- NFA tests
runNFATests :: IO ()
runNFATests = do runTestTT $ TestList [nfaAcceptTest1,
                                       nfaAcceptTest2,
                                       nfaFromStringTest]
                 quickCheck (withMaxSuccess 500 (prop_union nfa1 nfa2))
                 quickCheck (withMaxSuccess 500 (prop_union nfa1 nfa3))
                 quickCheck (withMaxSuccess 500 (prop_union nfa3 nfa4))
                 quickCheck (withMaxSuccess 500 (prop_intersect nfa1 nfa2))
                 quickCheck (withMaxSuccess 500 (prop_intersect nfa1 nfa3))
                 quickCheck (withMaxSuccess 500 (prop_intersect nfa3 nfa4))
                 quickCheck (withMaxSuccess 500 (prop_minus nfa4 nfa2))
                 quickCheck (withMaxSuccess 500 (prop_minus nfa4 nfa3))
                 quickCheck (withMaxSuccess 500 (prop_minus nfa4 nfa1))
                 return ()

-- Regex tests
runRegexTests :: IO ()
runRegexTests = do runTestTT $ TestList [testSplitPair,
                                         testParts,
                                         testRegexAccept]
                   quickCheck (withMaxSuccess 100 (prop_union reg1 reg2))
                   quickCheck (withMaxSuccess 100 (prop_union reg1 reg3))
                   quickCheck (withMaxSuccess 100 (prop_intersect reg1 reg2))
                   quickCheck (withMaxSuccess 100 (prop_intersect reg1 reg3))
                  --  quickCheck (withMaxSuccess 100 (prop_minus reg4 reg1))
                  --  quickCheck (withMaxSuccess 100 (prop_minus reg4 reg3))
                   quickCheck (withMaxSuccess  15 (prop_minus reg3 reg1))
                   quickCheck (withMaxSuccess  15 (prop_minus reg1 reg3))                  
                   quickCheck (withMaxSuccess  15 (prop_minus reg2 reg1))
                   quickCheck (withMaxSuccess  15 (prop_minus reg1 reg2))
                   return ()

-- common tests for all matchers --

-- prop: accept m1 s <==> accept m2 s
prop_matcherEquals :: forall m n. (Matcher m, Matcher n) => m Char -> n Char -> [ABC] -> Bool
prop_matcherEquals m1 m2 str =
  let s = fmap (\(ABC c) -> c) str in
    accept m1 s == accept m2 s

-- prop: (accept m1 s) || (accept m2 s) <==> accept (union m1 m2) s
prop_union :: Matcher m => m Char -> m Char -> [ABC] -> Bool
prop_union m1 m2 str =
  let s = fmap (\(ABC a) -> a) str in
    case union m1 m2 of
      Nothing -> alphabet m1 /= alphabet m2
      Just d3 -> case (accept m1 s, accept m2 s) of
        (Just True, _)  -> accept d3 s == Just True
        (_, Just True)  -> accept d3 s == Just True
        (Just False, _) -> accept d3 s == Just False
        (_, Just False) -> accept d3 s == Just False
        _               -> isNothing (accept d3 s)

-- prop: (accept m1 s) || (accept m2 s) <==> accept (intersect m1 m2) s
prop_intersect :: Matcher m => m Char -> m Char -> [ABC] -> Bool
prop_intersect m1 m2 str =
  let s = fmap (\(ABC a) -> a) str in
    case intersect m1 m2 of
      Nothing -> alphabet m1 /= alphabet m2
      Just d3 -> case (accept m1 s, accept m2 s) of
        (Just True, Just True)   -> accept d3 s == Just True
        (Just False, Just True)  -> accept d3 s == Just False
        (Just True, Just False)  -> accept d3 s == Just False
        (Just False, Just False) -> accept d3 s == Just False
        _                        -> isNothing (accept d3 s)

-- prop: (accept m1 s) && (not (accept m2 s)) <==> accept (minus m1 m2) s
prop_minus :: Matcher m => m Char -> m Char -> [ABC] -> Bool
prop_minus m1 m2 str =
  let s = fmap (\(ABC a) -> a) str in
    case minus m1 m2 of
      Nothing -> alphabet m1 /= alphabet m2
      Just d3 -> case (accept m1 s, accept m2 s) of
        (Just True, Just True)   -> accept d3 s == Just False
        (Just False, Just True)  -> accept d3 s == Just False
        (Just True, Just False)  -> accept d3 s == Just True
        (Just False, Just False) -> accept d3 s == Just False
        _                        -> isNothing (accept d3 s)

---------------
-- DFA tests --
---------------

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

---------------
-- NFA tests --
---------------

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
    accept nfa1 "aad" ~?= Nothing
  ]

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
    accept nfa3 "d" ~?= Nothing
  ]

nfaFromStringTest :: Test
nfaFromStringTest = TestList
  [
    fromString "3\nabc\n6\n0 a 1\n0 b 0\n1 a 2\n1 b 1\n2 a 2\n2 b 2\n0\n2" ~?= Just nfa1,
    fromString "2\nabc\n1\n0 ep 1\n0\n1" ~?= Just nfa2
  ]

-----------------
-- regex tests --
-----------------

testRegexAccept :: Test
testRegexAccept =
  TestList [
    accept reg1 "a" ~?= Just True,
    accept reg1 "b" ~?= Just True,
    accept reg1 "aa" ~?= Just False,
    accept reg1 "d" ~?= Nothing,
    accept reg2 "" ~?= Just True,
    accept reg2 "c" ~?= Just True,
    accept reg2 "abc" ~?= Just True,
    accept reg2 "d" ~?= Nothing,
    accept reg3 "a" ~?= Just True,
    accept reg3 "ab" ~?= Just False,
    accept reg3 "cca" ~?= Just True,
    accept reg3 "" ~?= Just False,
    accept reg4 "a" ~?= Just True,
    accept reg4 "b" ~?= Just True,
    accept reg4 "ab" ~?= Just False,
    accept reg4 "abc" ~?= Just False,
    accept reg4 "abca" ~?= Just True
  ]

testFromString :: Test
testFromString =
  TestList [

  ]


-- utility functions tests

testSplitPair :: Test
testSplitPair =
  TestList [
    (Set.fromList $ splitPair "") ~?= (Set.fromList [("", "")]),
    (Set.fromList $ splitPair "a") ~?= (Set.fromList [("", "a"), ("a", "")]),
    (Set.fromList $ splitPair "ab") ~?=
      (Set.fromList [("", "ab"), ("a", "b"), ("ab", "")]),
    (Set.fromList $ splitPair "abc") ~?=
      (Set.fromList [("","abc"),("a","bc"),("ab","c"),("abc","")]),
    (Set.fromList $ splitPair "abcd") ~?=
      (Set.fromList [("", "abcd"), ("a", "bcd"), ("ab", "cd"),
      ("abc", "d"), ("abcd", "")])
  ]

testParts :: Test
testParts =
  TestList [
    (Set.fromList $ parts "") ~?= (Set.fromList [[]]),
    (Set.fromList $ parts "a") ~?= (Set.fromList [["a"]]),
    (Set.fromList $ parts "ab") ~?= (Set.fromList [["ab"], ["a", "b"]]),
    (Set.fromList $ parts "abc") ~?=
      (Set.fromList [["abc"], ["a", "bc"], ["ab", "c"], ["a", "b", "c"]]),
    (Set.fromList $ parts "abcd") ~?=
      (Set.fromList [["abcd"], ["a", "bcd"], ["a", "b", "cd"],
      ["a", "b", "c", "d"], ["ab", "cd"], ["ab", "c", "d"],
      ["abc", "d"], ["a", "bc", "d"]])
  ]