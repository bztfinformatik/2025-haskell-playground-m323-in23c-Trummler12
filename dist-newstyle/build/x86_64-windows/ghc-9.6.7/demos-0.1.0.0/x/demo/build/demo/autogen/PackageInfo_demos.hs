{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_demos (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "demos"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Demo exercises for M323-Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
