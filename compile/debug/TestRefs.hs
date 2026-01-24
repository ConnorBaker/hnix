{-# LANGUAGE RecordWildCards #-}

module Main where

import Relude
import GHC
import GHC.Paths (libdir)
import GHC.Driver.Session
import GHC.Driver.DynFlags (PackageFlag(..), PackageArg(..), ModRenaming(..))
import GHC.Unit.Types (stringToUnit)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))

import Nix.Compile.Refs

main :: IO ()
main = do
  putStrLn "=== Testing runtime refs loading ==="

  -- Find GHC environment file or Cabal's package database
  mEnvFile <- findGhcEnvironmentFile
  putStrLn $ "GHC environment file: " <> show mEnvFile

  mPkgDb <- findCabalPackageDb
  putStrLn $ "Cabal package DB: " <> show mPkgDb

  mNixPkgDbs <- lookupEnv "HNIX_PACKAGE_DBS"
  putStrLn $ "HNIX_PACKAGE_DBS: " <> show mNixPkgDbs

  mNixLibDir <- lookupEnv "NIX_GHC_LIBDIR"
  putStrLn $ "NIX_GHC_LIBDIR: " <> show mNixLibDir

  let ghcLibDir = fromMaybe libdir mNixLibDir

  -- Initialize GHC
  hsc <- runGhc (Just ghcLibDir) $ do
    -- Get default flags
    dflags <- getSessionDynFlags

    -- Configure for bytecode interpretation
    let baseDflags = dflags
          { ghcLink = LinkInMemory
          , verbosity = 1  -- Some verbosity to see what's happening
          }

    -- Package database configuration:
    -- We don't use packageEnv (environment file) because GHC API doesn't properly
    -- process it. Instead, we manually configure package DBs and expose packages.
    let cabalPkgDbFlags = case mPkgDb of
          Just pkgDb -> [PackageDB (PkgDbPath pkgDb)]
          Nothing -> []
        nixPkgDbFlags = case mNixPkgDbs of
          Just nixDbs ->
            [PackageDB (PkgDbPath db) | db <- splitOnColon nixDbs, not (null db)]
          Nothing -> []
        -- Expose the main hnix package which makes sublibraries' modules visible.
        -- Exposing sublibraries directly by unit ID doesn't work because GHC's
        -- module visibility is controlled by the top-level package name.
        exposeFlags =
          [ ExposePackage "-package hnix"
              (PackageArg "hnix")
              (ModRenaming True [])
          ]
        dflags' = baseDflags
          { packageDBFlags = packageDBFlags baseDflags ++ cabalPkgDbFlags ++ nixPkgDbFlags
          , packageFlags = packageFlags baseDflags ++ exposeFlags
          }

    liftIO $ do
      putStrLn $ "Final pkg DBs: " ++ show (packageDBFlags dflags')
      putStrLn $ "Final pkg flags: " ++ show (packageFlags dflags')

    -- setSessionDynFlags reinitializes the package state
    (newDflags, _, _) <- setSessionDynFlags dflags'

    -- Check what GHC knows about the unit state
    liftIO $ do
      putStrLn $ "After setSessionDynFlags, checking unit state..."
      let unitEnv = hsc_unit_env <$> getSession
      putStrLn $ "Getting session to check units..."

    -- Try to import the runtime modules into the session context.
    let runtimeImports =
          [ IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Value")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins")
          , IIDecl $ simpleImportDecl (mkModuleName "Data.Vector")
          ]
    -- Attempt to set context, catching any errors
    handleSourceError (\e -> liftIO $ putStrLn $ "Import error: " <> show e) $ setContext runtimeImports

    getSession

  putStrLn "\n=== Running debugLoadRuntimeRefs ==="
  debugLoadRuntimeRefs hsc

  putStrLn "\n=== Trying to load all refs ==="
  mRefs <- loadRuntimeRefs hsc
  case mRefs of
    Just _ -> putStrLn "SUCCESS: All refs loaded!"
    Nothing -> putStrLn "FAIL: loadRuntimeRefs returned Nothing"

-- | Find Cabal's package database directory.
findCabalPackageDb :: IO (Maybe FilePath)
findCabalPackageDb = do
  let distDir = "dist-newstyle"
  exists <- doesDirectoryExist distDir
  if not exists
    then pure Nothing
    else do
      let pkgDbBase = distDir </> "packagedb"
      pkgDbExists <- doesDirectoryExist pkgDbBase
      if not pkgDbExists
        then pure Nothing
        else do
          contents <- listDirectory pkgDbBase
          let ghcDirs = filter (\d -> "ghc-" `isPrefixOf` d) contents
          case ghcDirs of
            [] -> pure Nothing
            (d:_) -> do
              let pkgDb = pkgDbBase </> d
              isDir <- doesDirectoryExist pkgDb
              pure $ if isDir then Just pkgDb else Nothing

-- | Find GHC environment file created by Cabal.
findGhcEnvironmentFile :: IO (Maybe FilePath)
findGhcEnvironmentFile = do
  cwd <- getCurrentDirectory
  contents <- listDirectory cwd
  let envFiles = filter (".ghc.environment." `isPrefixOf`) contents
  case envFiles of
    [] -> pure Nothing
    (f:_) -> pure (Just (cwd </> f))

-- | Split a string on colons.
splitOnColon :: String -> [String]
splitOnColon "" = []
splitOnColon s = case break (== ':') s of
  (before, "") -> [before]
  (before, _:rest) -> before : splitOnColon rest
