-- comments
-- go
-- here

{-# LANGUAGE InstanceSigs #-}

module Regex where

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Matcher

data Regex = Char (Set Char)
           | Alt Regex Regex
           | Seq Regex Regex
           | Star Regex
           | Empty
           | Void
  deriving (Show, Eq)

char :: Char -> Regex
char = Char . Set.singleton

chars :: String -> Regex
chars = Char . Set.fromList





instance Matcher Regex where
  accept :: Regex -> String -> Maybe Bool
  accept = undefined

  union :: Regex -> Regex -> Maybe Regex
  union = undefined

  intersect :: Regex -> Regex -> Maybe Regex
  intersect = undefined

  minus :: Regex -> Regex -> Maybe Regex
  minus = undefined

  fromString :: String -> Maybe Regex
  fromString = undefined