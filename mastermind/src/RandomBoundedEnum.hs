{-# LANGUAGE FlexibleInstances, UndecidableInstances,ScopedTypeVariables, OverlappingInstances #-}
module RandomBoundedEnum where

import System.Random

class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a
instance BoundedEnum a => Random a where
  random = randomR (minBound :: a, maxBound :: a)
  randomR (f, t) gen =
    (toEnum r :: a, nextGen)
    where
      (rnd, nextGen) = next gen
      r = fromEnum f + (rnd `mod` length [f..t])