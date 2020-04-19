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
import Data.Monoid

import Diagrams.Prelude
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


runSchematic :: forall k. KnownNat k => Schematic k -> Diagram B
runSchematic schematic = runSchematic' schematic ||| hrule 1
  where
    runSchematic' :: forall k. KnownNat k => Schematic k -> Diagram B
    runSchematic' = \case
      String -> foldr (|||) mempty (replicate n (hrule 2))
      Slot   -> foldr (|||) mempty (replicate n (hrule 1 ||| square 1))
      AndThen x y -> runSchematic' x ||| runSchematic' y
      where n = fromIntegral $ natVal $ Proxy @k

{-

Slot 1:
-[]-

Slot 2:
-[]-[]-

AndThen (Slot 1) (Slot 1):
-[]-[]-

-}

slot1 :: Schematic 1
slot1 = Slot

slot2 :: Schematic 2
slot2 = Slot

slot2' :: Schematic 2
slot2' = AndThen (Slot @1) (Slot @1)

--cSmoosh :: forall j k. (KnownNat j, KnownNat k) => Chain j -> Chain k -> Chain (j+k)
--cSmoosh _ _ = Chain

main :: IO ()
main = mainWith $ square 10 <> center (runSchematic slot2')
