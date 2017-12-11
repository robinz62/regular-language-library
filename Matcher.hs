module Matcher where

import Data.Maybe
import Data.Set (Set)

class Matcher m where
  -- returns the matcher's alphabet
  alphabet   :: Ord a => m a -> Set a

  -- returns whether or not the matcher accepts the input string
  -- returns Nothing if input string has character not specified in the
  -- matcher's alphabet
  accept     :: Ord a => m a -> [a] -> Maybe Bool

  -- returns a matcher that accepts the union of the input matchers' languages
  -- returns Nothing if the input matchers do not have equal alphabets
  union      :: Ord a => m a -> m a -> Maybe (m a)

  -- returns a matcher that accepts the intersection of the input matchers'
  -- languages
  -- returns Nothing if the input matchers do not have equal alphabets
  intersect  :: Ord a => m a -> m a -> Maybe (m a)

  -- returns a matcher that accepts the first matcher's language set minus the
  -- second matcher's language
  -- returns Nothing if the input matchers do not have equal alphabets
  minus      :: Ord a => m a -> m a -> Maybe (m a)

  -- returns a matcher that accepts the langauge containing strings formed by
  -- concatenating a string from the first with one from the second
  concat     :: Ord a => m a -> m a -> Maybe (m a)

  -- returns a matcher that accepts the kleene-star of the input language
  kStar      :: Ord a => m a -> Maybe (m a)

  -- parses the input string into a matcher
  -- returns Nothing if string is not parseable
  fromString :: String -> Maybe (m Char)
