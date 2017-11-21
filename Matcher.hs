module Matcher where

import Data.Maybe

class Matcher m where
  -- returns whether or not the matcher accepts the input string
  -- returns Nothing if input string has character not specified in the
  -- matcher's alphabet
  accept     :: m -> String -> Maybe Bool

  -- returns a matcher that accepts the union of the input matchers' languages
  -- returns Nothing if the input matchers do not have equal alphabets
  union      :: m -> m -> Maybe m

  -- returns a matcher that accepts the intersection of the input matchers'
  -- languages
  -- returns Nothing if the input matchers do not have equal alphabets
  intersect  :: m -> m -> Maybe m

  -- returns a matcher that accepts the first matcher's language set minus the
  -- second matcher's language
  -- returns Nothing if the input matchers do not have equal alphabets
  minus      :: m -> m -> Maybe m

  -- parses the input string into a matcher
  -- returns Nothing if string is not parseable
  fromString :: String -> Maybe m
