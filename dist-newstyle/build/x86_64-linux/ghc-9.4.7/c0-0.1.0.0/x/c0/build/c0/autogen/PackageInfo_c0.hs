{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_c0 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "c0"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Trabalho de Compiladores (CC3001)"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
