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


-- | Wrapper around Char for purposes of generating arbitrary strings
--   restricted to the characters a-c
newtype ABC = ABC Char deriving (Eq, Ord, Show, Read)

instance Arbitrary ABC where
  arbitrary = elements (fmap ABC ['a', 'b', 'c'])

-- all tests compiled
runAllTests :: IO ()
runAllTests = do runDFATests
                 runNFATests
                 runRegexTests
                 runConvertTests

-- DFA tests
runDFATests :: IO ()
runDFATests = do runTestTT $ TestList [dfaAcceptTest1,
                                       dfaAcceptTest2,
                                       dfaConcatTest,
                                       dfaKStarTest,
                                       dfaFromStringTest]
                 quickCheck (withMaxSuccess 100 (prop_union dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 100 (prop_union dfaEmpty dfa2))
                 quickCheck (withMaxSuccess 100 (prop_union dfa1 dfa2))
                 quickCheck (withMaxSuccess 100 (prop_intersect dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 100 (prop_intersect dfa1 dfa1))
                 quickCheck (withMaxSuccess 100 (prop_intersect dfa1 dfa2))
                 quickCheck (withMaxSuccess 100 (prop_minus dfaEmpty dfa1))
                 quickCheck (withMaxSuccess 100 (prop_minus dfa1 dfaEmpty))
                 quickCheck (withMaxSuccess 100 (prop_minus dfa2 dfa1))
                 return ()

-- NFA tests
runNFATests :: IO ()
runNFATests = do runTestTT $ TestList [nfaAcceptTest1,
                                       nfaAcceptTest2,
                                       nfaConcatTest,
                                       nfaKStarTest,
                                       nfaFromStringTest]
                 quickCheck (withMaxSuccess 100 (prop_union nfa1 nfa2))
                 quickCheck (withMaxSuccess 100 (prop_union nfa1 nfa3))
                 quickCheck (withMaxSuccess 100 (prop_union nfa3 nfa4))
                 quickCheck (withMaxSuccess 100 (prop_intersect nfa1 nfa2))
                 quickCheck (withMaxSuccess 100 (prop_intersect nfa1 nfa3))
                 quickCheck (withMaxSuccess 100 (prop_intersect nfa3 nfa4))
                 quickCheck (withMaxSuccess 100 (prop_minus nfa4 nfa2))
                 quickCheck (withMaxSuccess 100 (prop_minus nfa4 nfa3))
                 quickCheck (withMaxSuccess 100 (prop_minus nfa4 nfa1))
                 return ()

-- Regex tests
runRegexTests :: IO ()
runRegexTests = do runTestTT $ TestList [testSplitPair,
                                         testParts,
                                         regexAcceptTest,
                                         regexConcatTest,
                                         regexKStarTest,
                                         regexFromStringTest]
                   quickCheck (withMaxSuccess 100 (prop_union reg1 reg2))
                   quickCheck (withMaxSuccess 100 (prop_union reg1 reg3))
                   quickCheck (withMaxSuccess 100 (prop_intersect reg1 reg2))
                   quickCheck (withMaxSuccess 100 (prop_intersect reg1 reg3))
                   quickCheck (withMaxSuccess  15 (prop_minus reg4 reg1))
                   quickCheck (withMaxSuccess  15 (prop_minus reg4 reg3))
                   quickCheck (withMaxSuccess  15 (prop_minus reg3 reg1))
                   quickCheck (withMaxSuccess  15 (prop_minus reg1 reg3))                  
                   quickCheck (withMaxSuccess  15 (prop_minus reg2 reg1))
                   quickCheck (withMaxSuccess  15 (prop_minus reg1 reg2))
                   return ()

runConvertTests :: IO ()
runConvertTests = do quickCheck $ prop_dfaToNfa dfa2
                     quickCheck $ prop_dfaToNfa dfa3
                     quickCheck $ prop_nfaToDfa nfa2
                     quickCheck $ prop_nfaToDfa nfa3
                     quickCheck $ prop_regexToNfa reg1
                     quickCheck $ prop_regexToNfa reg4
                     quickCheck $ withMaxSuccess 15 (prop_nfaToRegex nfa2)
                     quickCheck $ withMaxSuccess 15 (prop_nfaToRegex nfa3)

-- common tests for all matchers --

-- prop: accept m1 s <==> accept m2 s
prop_matcherEquals :: forall m n. (Matcher m, Matcher n) => m Char
                                                         -> n Char
                                                         -> [ABC]
                                                         -> Bool
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

-- below properties: converting between matcher instances should not change the
-- accepted language
-- note that Nothing -> False probably should not actually be always False in
-- the case the matcher cannot actually be converted
-- for our purposes, we will always provide valid inputs

prop_dfaToNfa :: DFA Char -> [ABC] -> Bool
prop_dfaToNfa dfa str = case dfaToNfa dfa of
  Nothing  -> False
  Just nfa -> prop_matcherEquals dfa nfa str

prop_nfaToDfa :: NFA Char -> [ABC] -> Bool
prop_nfaToDfa nfa str = case nfaToDfa nfa of
  Nothing  -> False
  Just dfa -> prop_matcherEquals dfa nfa str

prop_nfaToRegex :: NFA Char -> [ABC] -> Bool
prop_nfaToRegex nfa str = case nfaToRegex nfa of
  Nothing  -> False
  Just reg -> prop_matcherEquals nfa reg str

prop_regexToNfa :: RegexA Char -> [ABC] -> Bool
prop_regexToNfa reg str = case regexToNfa reg of
  Nothing  -> False
  Just nfa -> prop_matcherEquals nfa reg str

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

dfaConcatTest :: Test
dfaConcatTest =
  case Matcher.concat dfa1 dfa2 of
    Nothing -> True ~?= False
    Just dfa' ->
      TestList [
        accept dfa' "" ~?= Just True,
        accept dfa' "abc" ~?= Just True,
        accept dfa' "aaa" ~?= Just True,
        accept dfa' "aabc" ~?= Just True,
        accept dfa' "aaabc" ~?= Just True,
        accept dfa' "aaabcabc" ~?= Just True,
        accept dfa' "abcabc" ~?= Just True,
        accept dfa' "aaab" ~?= Just False,
        accept dfa' "aaabca" ~?= Just False
      ]

dfaKStarTest :: Test
dfaKStarTest =
  case Matcher.kStar dfa3 of
    Nothing -> True ~?= False
    Just dfa' ->
      TestList [
        accept dfa' "" ~?= Just True,
        accept dfa' "ab" ~?= Just True,
        accept dfa' "ac" ~?= Just True,
        accept dfa' "aa" ~?= Just False,
        accept dfa' "abac" ~?= Just True,
        accept dfa' "abab" ~?= Just True,
        accept dfa' "acac" ~?= Just True,
        accept dfa' "abaca" ~?= Just False,
        accept dfa' "abacab" ~?= Just True
      ]

dfaFromStringTest :: Test
dfaFromStringTest = TestList
  [
    fromString "DFA\nN 1\nA [abc]\nTRANSITION\n0 a 0\n0 b 0\n0 c 0\nSTART 0\nF" ~?= Just dfaEmpty,
    fromString "DFA\nN 2\nA [abc]\nTRANSITION\n0 a 0\n0 b 1\n0 c 1\n1 a 1\n1 b 1\n1 c 1\nSTART 0\nF 0" ~?= Just dfa1,
    fromString "DFA\nN 4\nA [abc]\nTRANSITION\n0 a 1\n0 b 3\n0 c 3\n1 a 3\n1 b 2\n1 c 3\n2 a 3\n2 b 3\n2 c 0\n3 a 3\n3 b 3\n3 c 3\nSTART 0\nF 0" ~?= Just dfa2,
    fromString "DFA\nN a\nA [abc]\nTRANSITION\n0 a 0\n0 b 0\n0 c 0\nSTART 0\nF \n" ~?= (Nothing :: Maybe (DFA Char)),
    fromString "DFA\nN 1\nA [abc]\nTRANSITION\n0 a z\n0 b 0\n0 c 0\nSTART 0\nF \n" ~?= (Nothing :: Maybe (DFA Char)),
    fromString "DFA\nN 1\nA [abc]\nTRANSITION\n0 a 0\n0 b 0\n0 c 0\nSTART z\nF \n" ~?= (Nothing :: Maybe (DFA Char)),
    fromString "NFA\nN 1\nA [abc]\nTRANSITION\n0 a 0\n0 b 0\n0 c 0\nSTART z\nF \n" ~?= (Nothing :: Maybe (DFA Char))    
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

nfaConcatTest :: Test
nfaConcatTest = case Matcher.concat nfa3 nfa4 of
  Nothing   -> True ~?= False
  Just nfa' ->
    TestList [
      accept nfa' "" ~?= Just False,
      accept nfa' "a" ~?= Just True,
      accept nfa' "ac" ~?= Just False,
      accept nfa' "aabc" ~?= Just True,
      accept nfa' "baabc" ~?= Just True,
      accept nfa' "bcaabc" ~?= Just False,
      accept nfa' "bacba" ~?= Just True,
      accept nfa' "bbabbaabcabc" ~?= Just True,
      accept nfa' "c" ~?= Just False,
      accept nfa' "ccca" ~?= Just False
    ]

nfaKStarTest :: Test
nfaKStarTest = case kStar nfa4 of
  Nothing   -> True ~?= False
  Just nfa' ->
    TestList [
      accept nfa' "" ~?= Just True,
      accept nfa' "a" ~?= Just True,
      accept nfa' "abba" ~?= Just True,
      accept nfa' "abc" ~?= Just True,
      accept nfa' "aaa" ~?= Just True,
      accept nfa' "aaaabc" ~?= Just True,
      accept nfa' "cccbbbaabcabc" ~?= Just True
    ]

nfaFromStringTest :: Test
nfaFromStringTest = TestList
  [
    fromString "NFA\nN 3\nA [abc]\nTRANSITION\n0 a 1\n0 b 0\n1 a 2\n1 b 1\n2 a 2\n2 b 2\nEP-TRANSITION\nSTART 0\nF 2" ~?= Just nfa1,
    fromString "NFA\nN 2\nA [abc]\nTRANSITION\nEP-TRANSITION\n0 1\nSTART 0\nF 1" ~?= Just nfa2,
    fromString "DFA\nN 2\nA [abc]\nTRANSITION\nEP-TRANSITION\n0 1\nSTART 0\nF 1" ~?= (Nothing :: Maybe (NFA Char))
  ]

-----------------
-- regex tests --
-----------------

regexAcceptTest :: Test
regexAcceptTest =
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

regexConcatTest :: Test
regexConcatTest = case Matcher.concat reg1 reg1 of
  Nothing   -> True ~?= False
  Just reg' ->
    TestList [
      accept reg' "a" ~?= Just False,
      accept reg' "b" ~?= Just False,
      accept reg' "aa" ~?= Just True,
      accept reg' "ab" ~?= Just True,
      accept reg' "bb" ~?= Just True,
      accept reg' "bc" ~?= Just True,
      accept reg' "cc" ~?= Just True,
      accept reg' "ca" ~?= Just True,
      accept reg' "cca" ~?= Just False,
      accept reg' "aaa" ~?= Just False,
      accept reg' "" ~?= Just False
    ]

regexKStarTest :: Test
regexKStarTest = case kStar reg1 of
  Nothing   -> True ~?= False
  Just reg' ->
    TestList [
      accept reg' "a" ~?= Just True,
      accept reg' "b" ~?= Just True,
      accept reg' "aa" ~?= Just True,
      accept reg' "ab" ~?= Just True,
      accept reg' "bb" ~?= Just True,
      accept reg' "bc" ~?= Just True,
      accept reg' "cc" ~?= Just True,
      accept reg' "ca" ~?= Just True,
      accept reg' "cca" ~?= Just True,
      accept reg' "aaa" ~?= Just True,
      accept reg' "" ~?= Just True,
      accept reg' "abcba" ~?= Just True,
      accept reg' "bbbbba" ~?= Just True
    ]

regexFromStringTest :: Test
regexFromStringTest =
  TestList [
    fromString "" ~?= Just (R (Empty, Set.empty)),
    fromString "a" ~?= Just (R (Single (Set.singleton 'a'), Set.singleton 'a')),
    fromString "a|b" ~?= Just (R (Alt (Single (Set.fromList "a")) (Single (Set.fromList "b")), Set.fromList "ab")),
    fromString "abc" ~?= Just (R (Seq (Single (Set.fromList "a")) (Seq (Single (Set.fromList "b")) (Single (Set.fromList "c"))), Set.fromList "abc")),
    fromString "a*" ~?= Just (R (Star (Single (Set.fromList "a")), Set.fromList "a")),
    fromString "(ab)*" ~?= Just (R (Star (Seq (Single (Set.fromList "a")) (Single (Set.fromList "b"))), Set.fromList "ab")),
    fromString "a.b" ~?= Just (R (Seq (Single (Set.fromList "a")) (Single (Set.fromList "b")), Set.fromList "ab")),
    fromString "(ab)*.(x|z)" ~?= Just (R (Seq (Star (Seq (Single (Set.fromList "a")) (Single (Set.fromList "b")))) (Alt (Single (Set.fromList "x")) (Single (Set.fromList "z"))), Set.fromList "abxz")),
    fromString "ab*." ~?= (Nothing :: Maybe (RegexA Char))
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