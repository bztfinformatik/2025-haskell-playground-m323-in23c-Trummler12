{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_geometry (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "geometry"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Vector mathematics and geometric shapes"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
