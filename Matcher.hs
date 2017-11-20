module Matcher where

import NFA

class Matcher m where
  accept     :: m -> String -> Bool
  union      :: m -> m -> m
  intersect  :: m -> m -> m
  minus      :: m -> m -> m
  toNFA      :: m -> NFA
  fromNFA    :: NFA -> m
  fromString :: String -> m
