{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( mapWithIndex
  , pair
  ) where

import RIO

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = 
  mapWithIndexRec f 1 xs
  where
    mapWithIndexRec f i [] = []
    mapWithIndexRec f i (x:xs) = 
      f i x : mapWithIndexRec f (i + 1) xs

pair :: a -> (a, a)
pair !x = (x, x)