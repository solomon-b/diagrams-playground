{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Data.Dynamic
import Data.Function
import Data.Proxy
import Data.Monoid hiding (Product)

import Diagrams.Prelude hiding (Product)
import Diagrams.Backend.Rasterific.CmdLine

import GHC.TypeLits

infixl 7 <.>
a <.> b = center (a ||| b)

infixl 6 <+>
a <+> b = pad 1.5 a === pad 1.5 b


data Schematic (k :: Nat) where
  String  :: Schematic k
  Slot    :: Schematic k
  AndThen :: (KnownNat i, KnownNat j) => Schematic i -> Schematic j -> Schematic (i+j)
  (:*:) :: Schematic k -> Schematic k -> Schematic k


runSchematic :: forall k. KnownNat k => Schematic k -> Diagram B
runSchematic = \case
  String -> foldr (|||) mempty (replicate n (hrule 3))
  Slot | n == 0 -> runSchematic (String @1)
  Slot   -> foldr (|||) mempty (replicate n (hrule 1 ||| square 1 ||| hrule 1))
  AndThen x y -> runSchematic x ||| runSchematic y
  x :*: y -> runSchematic x <+> runSchematic y
  where n = fromIntegral $ natVal $ Proxy @k

{-

Slot 0:
String 1

Slot 1:
-[]-

Slot 2:
-[]--[]-

AndThen (Slot 1) (Slot 1):
-[]--[]-

String 0:
mempty

String 1:
---

String 2:
------

AndThen (Slot 1) (String 1) 2
-[]----
-}

slot0 :: Schematic 0
slot0 = Slot

slot1 :: Schematic 1
slot1 = Slot

slot2 :: Schematic 2
slot2 = Slot

slot2' :: Schematic 2
slot2' = AndThen (Slot @1) (Slot @1)

string0 :: Schematic 0
string0 = String

string1 :: Schematic 1
string1 = String

string2 :: Schematic 2
string2 = String

andThen2 :: Schematic 2
andThen2 = AndThen (Slot @1) String

--cSmoosh :: forall j k. (KnownNat j, KnownNat k) => Chain j -> Chain k -> Chain (j+k)
--cSmoosh _ _ = Chain

main :: IO ()
main = mainWith $ square 15 <> center (runSchematic ((Slot @3) :*: ((Slot @1) `AndThen` String)))
