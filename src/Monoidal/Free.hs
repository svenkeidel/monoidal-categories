{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Monoidal.Free where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

import Diagrams.Prelude hiding (Iso,Product,connect,width,heigth)
import Diagrams.Backend.SVG

import Monoidal.Iso
import Monoidal.Category

data FreeObj where
  UnitObj :: FreeObj
  NumObj :: FreeObj
  ProductObj :: FreeObj -> FreeObj -> FreeObj
genSingletons [''FreeObj]

data Free :: (FreeObj -> FreeObj -> Type) where
  Id         :: SFreeObj x -> Free x x
  Compose    :: Free y z -> Free x y -> Free x z
  Product    :: Free x1 y1 -> Free x2 y2 -> Free (x1 ⨂ x2) (y1 ⨂ y2)
  Assoc1     :: SFreeObj x -> SFreeObj y -> SFreeObj z -> Free (x⨂(y⨂z)) ((x⨂y)⨂z)
  Assoc2     :: SFreeObj x -> SFreeObj y -> SFreeObj z -> Free ((x⨂y)⨂z) (x⨂(y⨂z))
  LeftUnit1  :: SFreeObj x -> Free (x⨂I) x
  LeftUnit2  :: SFreeObj x -> Free x (x⨂I)
  RightUnit1 :: SFreeObj x -> Free (I⨂x) x
  RightUnit2 :: SFreeObj x -> Free x (I⨂x)

instance Category Free where
  type Obj Free x = SingI x
  id = Id sing
  (∘) = Compose

instance Reflect Free where
  dom (Id x) = x
  dom (Compose _ g) = dom g
  dom (Product f g) = SProductObj (dom f) (dom g)
  dom (Assoc1 x y z) = SProductObj x (SProductObj y z)
  dom (Assoc2 x y z) = SProductObj (SProductObj x y) z
  dom (LeftUnit1 x) = SProductObj x SUnitObj
  dom (LeftUnit2 x) = x
  dom (RightUnit1 x) = SProductObj SUnitObj x
  dom (RightUnit2 x) = x

  cod (Id x) = x
  cod (Compose f _) = cod f
  cod (Product f g) = SProductObj (cod f) (cod g)
  cod (Assoc1 x y z) = SProductObj (SProductObj x y) z
  cod (Assoc2 x y z) = SProductObj x (SProductObj y z)
  cod (LeftUnit1 x) = x
  cod (LeftUnit2 x) = SProductObj x SUnitObj
  cod (RightUnit1 x) = x
  cod (RightUnit2 x) = SProductObj SUnitObj x

instance Monoidal Free where
  type (⨂) = 'ProductObj
  type I = 'UnitObj
  (⨂) = Product
  assoc = Iso (Assoc1 sing sing sing) (Assoc2 sing sing sing)
  leftUnit = Iso (LeftUnit1 sing) (LeftUnit2 sing)
  rightUnit = Iso (RightUnit1 sing) (RightUnit2 sing)

type Width = Int

width :: Free x y -> Width
width e0 = case e0 of
  Product f g -> max (width f) (width g)
  Compose f g -> length f + length g
  _ -> 1

height :: Free x y -> Int
height e0 = case e0 of
  Id SUnitObj -> 0
  Id SNumObj -> 1
  Id (SProductObj x y) -> height (Id x) + height (Id y)
  Compose f g -> max (height f) (height g)
  Product f g -> heigth f + height g
  Assoc1 x y z -> height (Id x) + height (Id y) + height (Id z)
  Assoc2 x y z -> height (Id x) + height (Id y) + height (Id z)
  LeftUnit1 x -> height (Id x)
  LeftUnit2 x -> height (Id x)
  RightUnit1 x -> height (Id x)
  RightUnit2 x -> height (Id x)

data Connector a where
  UnitConnect :: Connector 'UnitObj
  NumConnect :: Trail V2 Double -> Connector 'NumObj
  ProductConnect :: Connector x -> Connector y -> Connector ('ProductObj x y)

connect :: Width -> Free x y -> Connector x -> Connector y
connect w e0 c0 = case (e0,c0) of
  (Id {}, UnitConnect) -> UnitConnect
  (Id {}, NumConnect t) -> NumConnect (t <> hrule w)
  (Id (SProductObj a b), ProductConnect c1 c2) ->
    ProductConnect (connect w (Id a) c1) (connect w (Id b) c2)
  (Compose f g, c) ->
    connect (width f) f (connect (width g) g c)
  (Product f g, ProductConnect c1 c2) ->
    ProductConnect (connect w f c1) (connect w g c2)
  (Assoc1 x y z, ProductConnect c1 (ProductConnect c2 c3)) ->
    ProductConnect (ProductConnect (connect w (Id x) c1) (connect w (Id y) c2)) (connect w (Id z) c3)
  (Assoc2 x y z, ProductConnect (ProductConnect c1 c2) c3) ->
    ProductConnect (connect w (Id x) c1) (ProductConnect (connect w (Id y) c2) (connect w (Id z) c3))
  (LeftUnit1 x, ProductConnect c UnitConnect) -> connect w (Id x) c

-- toDiagram :: Free x y -> Diagram B
-- toDiagram e0 = case e0 of
--   Id x -> case x of
--     SUnitObj -> square 1 # opacity 0
--     SNumObj  -> square 1 # opacity 0 <> hrule 1
--     SProductObj a b -> toDiagram (Product (Id a) (Id b))
--   Compose f g -> toDiagram f ||| toDiagram g
--   Product f g -> toDiagram f === toDiagram g
--   Assoc1 {} -> toDiagram (Id (dom e0))
--   Assoc2 {} -> toDiagram (Id (dom e0))
