{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_numlang (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "numlang"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Multi-language number-to-words conversion"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
