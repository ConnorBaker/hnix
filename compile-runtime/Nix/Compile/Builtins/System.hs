{-# LANGUAGE NoStrict #-}

-- | System-related built-in functions for the compiled Nix runtime.
--
-- This module provides builtins for detecting system information:
-- - `currentSystem`: Returns the current system in Nix format (arch-os)
-- - `langVersion`: Returns the Nix language version
-- - `nixVersion`: Returns the compatible Nix version
-- - `storeDir`: Returns the Nix store directory path
module Nix.Compile.Builtins.System
  ( builtinCurrentSystem
  , builtinLangVersion
  , builtinNixVersion
  , builtinStoreDir
  , detectCurrentSystem
  ) where

import Relude
import qualified System.Info
import Nix.Compile.Value

-- | builtins.currentSystem - detect the current system
builtinCurrentSystem :: NixValue
builtinCurrentSystem = VString detectCurrentSystem emptyContext
{-# NOINLINE builtinCurrentSystem #-}

-- | Detect the current system in Nix format (arch-os)
detectCurrentSystem :: Text
detectCurrentSystem = toText $ arch <> "-" <> os
  where
    -- Normalize architecture names to Nix conventions
    arch :: String
    arch = case System.Info.arch of
      "x86_64"  -> "x86_64"
      "aarch64" -> "aarch64"
      "arm64"   -> "aarch64"  -- macOS reports arm64
      "i386"    -> "i686"
      "i686"    -> "i686"
      a         -> a  -- Pass through unknown architectures

    -- Normalize OS names to Nix conventions
    os :: String
    os = case System.Info.os of
      "linux"   -> "linux"
      "darwin"  -> "darwin"
      "mingw32" -> "windows"
      "freebsd" -> "freebsd"
      "netbsd"  -> "netbsd"
      "openbsd" -> "openbsd"
      o         -> o  -- Pass through unknown OS

builtinLangVersion :: NixValue
builtinLangVersion = VInt 6  -- Nix language version
{-# NOINLINE builtinLangVersion #-}

builtinNixVersion :: NixValue
builtinNixVersion = VString "2.18.0" emptyContext  -- Compatibility target
{-# NOINLINE builtinNixVersion #-}

-- | builtins.storeDir - path to the Nix store
builtinStoreDir :: NixValue
builtinStoreDir = VString "/nix/store" emptyContext
{-# NOINLINE builtinStoreDir #-}
