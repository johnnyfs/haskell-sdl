module Fluency where

import Data.Maybe

whenExists :: Maybe a -> b -> Maybe b
whenExists mx y = case mx of
  Just x  -> Just y
  Nothing -> Nothing

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
