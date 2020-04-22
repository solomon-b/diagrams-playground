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

import Data.Bifunctor
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


{-
category c:
  data:
    id : a -> a
    (.) : (a `c` b) -> (b `c` c) -> (a `c` c)
  laws:
    f . id = f
    id . f = f
    f . (g . h) = (f . g) . h

category c <= monoidal c:
  data:
    :*: : Bifunctor f => a -> b -> a `f` b
    I : a

monoidal c <= braided c:
  data:
    swap : a # b -> b # a

braided c <= symmetric c:
  laws:
    swap . swap = id

symmetric c <= cartesian c:
  data:
    copy : a -> a # a
    discard : a -> I

symmetric c <= cocartesian c:
  data:
    fold : a # a -> a
    create : I -> a

cartesian c, cocartesian c <= bicartesian c:
-}


class Category c where
  id :: a `c` a
  (.) :: (x `c` y) -> (y `c` z) -> (x `c` z)

--instance Category (Schematic k) where
--  id :: Schematic k a a
--  id = String
--  (.) :: forall k a b c. KnownNat k => Schematic k a b -> Schematic k b c -> Schematic k a c
--  (.) = (:>>)

data Schematic (a :: *) (b :: *) where
  String  :: Schematic a a
  Slot    :: Schematic a b
  (:>>)   :: Schematic a b -> Schematic b c -> Schematic a c
  (:*:)   :: Schematic a a' -> Schematic b b' -> Schematic (a , a') (b , b')
  --(:*:)   :: Bifunctor f => Schematic a a' -> Schematic b b' -> Schematic (a `f` a') (b `f` b')
  --Swap    :: KnownNat n => Int -> Schematic k  -> Schematic k

instance Show (Schematic a b) where
  show String = "String"
  show Slot = "Slot"
  show (a :>> b) = show a ++ " >>> " ++ show b
  show (a :*: b) = show a ++ '\n' : show b

depth :: Schematic a b -> Int
depth (a :>> b) = depth a + depth b
depth (a :*: b) = max (depth a) (depth b)
depth String = 1
depth Slot = 1

stretch :: Schematic a b -> Schematic a b
stretch (a :*: b) =
  let (i, j) = (depth a, depth b)
      extendo n s = foldl' (:>>) s $ replicate n String
  in if i > j then Main.stretch a :*: extendo (i-j) (Main.stretch b) else extendo (j-1) (Main.stretch a) :*: Main.stretch b
stretch schematic = schematic


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


--weird :: Free (SchematicF ((,) (Diagram B))) (Diagram B)
--weird = Free $ AndThen' (square1 , Free $ AndThen' (line1, Pure square1))

square1 :: Diagram B
square1 = hrule 1 ||| square 1 ||| hrule 1

line1 :: Diagram B
line1 = hrule 3

data SchematicF f a = Slot' a | String' a | AndThen' (f a) | Product' (f (f a))
  deriving Functor

chain1 :: Free (SchematicF []) (Diagram B)
chain1 = Free (Slot' (Pure square1))

chain2 :: Free (SchematicF []) (Diagram B)
chain2 = Free (AndThen' [chain1, chain1])

string1 :: Free (SchematicF []) (Diagram B)
string1 = Free (String' $ Pure line1)

product2 :: Free (SchematicF []) (Diagram B)
product2 = Free (Product' [[chain1, string1], pure chain2, pure chain2])

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
      --s = runSchematic $ Slot :>> Slot :*: (Slot :>> String)
  in mainWith $ center d <> (square 15 # fc white)
