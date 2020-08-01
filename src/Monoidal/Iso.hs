{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Monoidal.Iso where

import Data.Kind

data Iso :: (k -> k -> Type) -> (k -> k -> Type) where
  Iso :: c x y -> c y x -> Iso c x y

from :: Iso c x y -> c x y
from (Iso f _) = f
{-# INLINE from #-}

to :: Iso c x y -> c y x
to (Iso _ g) = g
{-# INLINE to #-}
