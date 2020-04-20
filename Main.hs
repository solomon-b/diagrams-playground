{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Control.Monad.Free

import Data.Dynamic
import Data.Function
import Data.Foldable
import Data.Proxy
import Data.Monoid hiding (Product)

import Diagrams.Prelude hiding (Product)
import Diagrams.Backend.Rasterific.CmdLine

import GHC.TypeLits

infixl 7 <.>
a <.> b = center (a ||| b)

infixl 6 <+>
a <+> b = pad 1.5 a === pad 1.5 b

--data Schematic (k :: Nat) where
--  String  :: Schematic k
--  Slot    :: Schematic 1
--  (:>>)   :: (KnownNat i, KnownNat j) => Schematic i -> Schematic j -> Schematic (i+j)
--  (:*:)   :: Schematic k -> Schematic k -> Schematic k
--
--runSchematic :: forall k. KnownNat k => Schematic k -> Diagram B
--runSchematic = \case
--  String -> center $ foldr (|||) mempty (replicate n (hrule 3))
--  Slot | n == 0 -> runSchematic (String @1)
--  Slot   -> center $ foldr (|||) mempty (replicate n (hrule 1 ||| square 1 ||| hrule 1))
--  x :>> y -> center $ runSchematic x ||| runSchematic y
--  x :*: y -> runSchematic x <+> runSchematic y
--  where n = fromIntegral $ natVal $ Proxy @k

--cSmoosh :: forall j k. (KnownNat j, KnownNat k) => Chain j -> Chain k -> Chain (j+k)
--cSmoosh _ _ = Chain


data SchematicF f a = Slot' a | String' a | AndThen' (f a) | Product' (f (f a))
  deriving Functor

chain1 :: Free (SchematicF []) (Diagram B)
chain1 = Free (Slot' (Pure (hrule 1 ||| square 1 ||| hrule 1)))

chain2 :: Free (SchematicF []) (Diagram B)
chain2 = Free (AndThen' [chain1, chain1])

product2 :: Free (SchematicF []) (Diagram B)
product2 = Free (Product' [pure chain2, pure chain2, pure chain2])

homomorphism :: SchematicF [] (Diagram B) -> Diagram B
homomorphism (Slot' d) = d
homomorphism (String' d) = d
homomorphism (AndThen' d) = foldr (|||) mempty d
homomorphism (Product' t) =
  let chains = fmap (foldr (|||) mempty) t in foldr (<+>) mempty (center chains)

result :: Free (SchematicF []) (Diagram B) -> Diagram B
result = iter homomorphism


main :: IO ()
main =
  let d = result product2
      --s = runSchematic $ Slot :>> Slot :>> Slot :*: (Slot :>> String)
  in mainWith $ center d <> (square 15 # fc white)
