-- Vektor-Modul; Die Übung sieht vor, dass wir stets 2-dimensionale Vektoren verwenden, ich möchte die Dimensionalität jedoch nach oben offen halten;
-- Zudem will ich keinen bereits vorgefertigten Datentyp "Vektor" verwenden, sondern einen eigenen definieren mit Namen "vec"
-- Mögliche Eingaben: "vprint vsub (Vec 2 4 7) (Vec 1 2 3)" => "[1 2 4]"; "vprint vlength (Vec 3 4)" => "5.0"; "vprint vscale 2 (Vec 1 2 3)" => "[2 4 6]"
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Geometry.Vector where

import GHC.Exts     (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Time (scaleCalendarDiffDays)

data V2 where
  V2 :: Double -> Double -> V2
  deriving (Show)

vadd :: V2 -> V2 -> V2
vadd (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

vsub :: V2 -> V2 -> V2
vsub (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

vlength :: V2 -> Double
vlength (V2 x y) = sqrt (x^2 + y^2)

vunit :: V2 -> V2
vunit (V2 x y)
  | len == 0  = error "vunit: zero vector"
  | otherwise = V2 (x/len) (y/len)
  where len = vlength (V2 x y)

-- =========================================================
-- vmult in beiden Argument-Reihenfolgen
-- =========================================================

-- Nutzer soll entweder vmult :: Double -> V2 -> V2 ODER vmult :: V2 -> Double -> V2 eingeben dürfen:
type family VmultOK a b :: Constraint where
  VmultOK Double V2 = ()
  VmultOK V2     Double = ()
  VmultOK a b =
    TypeError ('Text "vmult: unsupported argument types: "
               ':<>: 'ShowType a ':<>: 'Text ", "
               ':<>: 'ShowType b ':$$:
               'Text "Use (Double,V2) or (V2,Double).")

class VMult a b where
  vmult :: VmultOK a b => a -> b -> V2

instance VMult Double V2 where
  vmult scalar (V2 x y) = V2 (scalar*x) (scalar*y)
instance VMult V2 Double where
  vmult (V2 x y) scalar = vmult scalar (V2 x y)

-- =========================================================
-- vunitmult in beiden Reihenfolgen
-- =========================================================

type family VUMOK a b :: Constraint where
  VUMOK Double V2 = ()
  VUMOK V2     Double = ()
  VUMOK a b =
    TypeError ('Text "vunitmult: unsupported argument types: "
               ':<>: 'ShowType a ':<>: 'Text ", "
               ':<>: 'ShowType b ':$$:
               'Text "Use (Double,V2) or (V2,Double).")

class VUnitMult a b where
  vunitmult :: VUMOK a b => a -> b -> V2

instance VUnitMult Double V2 where
  vunitmult s v = vmult s (vunit v)
instance VUnitMult V2 Double where
  vunitmult v s = vunitmult s v

-- =========================================================
-- vrotate in beiden Reihenfolgen (Winkel in Grad)
-- =========================================================

type family VROK a b :: Constraint where
  VROK Double V2 = ()
  VROK V2     Double = ()
  VROK a b =
    TypeError ('Text "vrotate: unsupported argument types: "
               ':<>: 'ShowType a ':<>: 'Text ", "
               ':<>: 'ShowType b ':$$:
               'Text "Use (Double,V2) or (V2,Double).")

class VRotate a b where
  vrotate :: VROK a b => a -> b -> V2

instance VRotate Double V2 where
  vrotate angle (V2 x y) =
    let rad = angle * (pi / 180)
        c   = cos rad
        s   = sin rad
    in V2 (x*c - y*s) (x*s + y*c)

instance VRotate V2 Double where
  vrotate v angle = vrotate angle v



data Square where
  Square :: V2 -> V2 -> Square
  deriving (Show)

data Rectangle where
  Rectangle :: V2 -> V2 -> Double -> Rectangle
  deriving (Show)

data TriangleEquilateral where
  TriangleEquilateral :: V2 -> V2 -> TriangleEquilateral
  deriving (Show)


