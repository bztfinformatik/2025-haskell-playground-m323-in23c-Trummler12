-- Vektor-Modul; Die Übung sieht vor, dass wir stets 2-dimensionale Vektoren verwenden, ich möchte die Dimensionalität jedoch nach oben offen halten;
-- Zudem will ich keinen bereits vorgefertigten Datentyp "Vektor" verwenden, sondern einen eigenen definieren mit Namen "vec"
-- Mögliche Eingaben: "vprint vsub (Vec 2 4 7) (Vec 1 2 3)" => "[1 2 4]"; "vprint vlength (Vec 3 4)" => "5.0"; "vprint vscale 2 (Vec 1 2 3)" => "[2 4 6]"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Geometry.Vector where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Length-indexed vector.
--   The length is tracked at the type level (Nat).
data Vec :: Nat -> Type -> Type where
  VNil :: Vec 0 a
  (:>) :: a -> Vec n a -> Vec (n + 1) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

--------------------------------------------------------------------------------
-- Pattern synonyms for nice construction / display

type V2 a = Vec 2 a
type V3 a = Vec 3 a
type V4 a = Vec 4 a
type V5 a = Vec 5 a
type V6 a = Vec 6 a
type V7 a = Vec 7 a
type V8 a = Vec 8 a
type V9 a = Vec 9 a
type V10 a = Vec 10 a

pattern V2 :: a -> a -> V2 a
pattern V2 x y = x :> y :> VNil
pattern V3 :: a -> a -> a -> V3 a
pattern V3 x y z = x :> y :> z :> VNil
pattern V4 :: a -> a -> a -> a -> V4 a
pattern V4 a b c d = a :> b :> c :> d :> VNil
pattern V5 :: a -> a -> a -> a -> a -> V5 a
pattern V5 a b c d e = a :> b :> c :> d :> e :> VNil
pattern V6 :: a -> a -> a -> a -> a -> a -> V6 a
pattern V6 a b c d e f = a :> b :> c :> d :> e :> f :> VNil
pattern V7 :: a -> a -> a -> a -> a -> a -> a -> V7 a
pattern V7 a b c d e f g = a :> b :> c :> d :> e :> f :> g :> VNil
pattern V8 :: a -> a -> a -> a -> a -> a -> a -> a -> V8 a
pattern V8 a b c d e f g h = a :> b :> c :> d :> e :> f :> g :> h :> VNil
pattern V9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> V9 a
pattern V9 a b c d e f g h i = a :> b :> c :> d :> e :> f :> g :> h :> i :> VNil
pattern V10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V10 a
pattern V10 a b c d e f g h i j = a :> b :> c :> d :> e :> f :> g :> h :> i :> j :> VNil

{-# COMPLETE VNil, (:>) #-}
{-# COMPLETE V2 #-}
{-# COMPLETE V3 #-}
{-# COMPLETE V4 #-}
{-# COMPLETE V5 #-}
{-# COMPLETE V6 #-}
{-# COMPLETE V7 #-}
{-# COMPLETE V8 #-}
{-# COMPLETE V9 #-}
{-# COMPLETE V10 #-}

--------------------------------------------------------------------------------
-- Core combinators

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ VNil      = VNil
vmap f (x :> xs) = f x :> vmap f xs

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ VNil      VNil      = VNil
vzipWith _ VNil      (_ :> _)  = error "vzipWith: impossible"
vzipWith _ (_ :> _)  VNil      = error "vzipWith: impossible"
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs (unsafeCoerce ys)

--------------------------------------------------------------------------------
-- Numeric operations

vadd :: Num a => Vec n a -> Vec n a -> Vec n a
vadd = vzipWith (+)

vsub :: Num a => Vec n a -> Vec n a -> Vec n a
vsub = vzipWith (-)

vdot :: Num a => Vec n a -> Vec n a -> a
vdot VNil      VNil      = 0
vdot VNil      (_ :> _)  = error "vdot: impossible"
vdot (_ :> _)  VNil      = error "vdot: impossible"
vdot (x :> xs) (y :> ys) = x * y + vdot xs (unsafeCoerce ys)

vlength :: Floating a => Vec n a -> a
vlength v = sqrt (vdot v v)

-- | Unit vector.
--   Uses 'Either' instead of 'error' so callers must handle the zero-vector case.
vunit :: (Eq a, Floating a) => Vec n a -> Either String (Vec n a)
vunit v
  | len == 0  = Left "vunit: zero vector"
  | otherwise = Right (vmap (/ len) v)
  where
    len = vlength v
