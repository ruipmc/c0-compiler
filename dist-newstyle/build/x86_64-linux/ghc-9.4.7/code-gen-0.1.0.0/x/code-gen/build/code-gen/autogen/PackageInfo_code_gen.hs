{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_code_gen (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "code_gen"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Compilador b\225sico para c0"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
