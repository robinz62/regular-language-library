module RegexTest where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Set(Set)
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck

import Matcher
import Regex

-- basically same tests as dfa and nfa

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