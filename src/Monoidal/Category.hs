{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Monoidal.Category where

import Data.Kind
import Data.Singletons

import Monoidal.Iso

class Category (c :: k -> k -> Type) where
  type Obj c (x :: k) :: Constraint
  id :: Obj c x => c x x
  (∘) :: Objs c [x,y,z] => c y z -> c x y -> c x z

type family Objs c (l :: [k]) :: Constraint where
  Objs c '[] = ()
  Objs c (x ': ys) = (Obj c x, Objs c ys)

class Category c => Reflect c where
  dom :: c x y -> Sing x
  cod :: c x y -> Sing y

class Category c => Monoidal c where
  type (⨂)  :: k -> k -> k
  type I    :: k
  (⨂)       :: Objs c [x1,x2,y1,y2] => c x1 y1 -> c x2 y2 -> c (x1 ⨂ x2) (y1 ⨂ y2)
  assoc     :: Objs c [x,y,z] => Iso c (x ⨂ (y ⨂ z)) ((x ⨂ y) ⨂ z)
  leftUnit  :: Obj c x => Iso c (x ⨂ I) x
  rightUnit :: Obj c x => Iso c (I ⨂ x) x
