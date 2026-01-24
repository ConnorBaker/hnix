{-# LANGUAGE OverloadedStrings #-}
module Main where

import Nix.Compile.Driver
import Nix.Compile.Refs
import GHC
import GHC.Paths (libdir)
import GHC.Driver.Session
import GHC.Driver.DynFlags (PackageFlag(..), PackageArg(..), ModRenaming(..))
import GHC.Unit.Types (stringToUnit)
import Relude
import qualified Data.List as L
import Data.List (isSuffixOf, isInfixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  putStrLn "=== Test 1: Check environment variables ==="
  mNixLibDir <- lookupEnv "NIX_GHC_LIBDIR"
  mNixPkgDbs <- lookupEnv "HNIX_PACKAGE_DBS"
  putStrLn $ "NIX_GHC_LIBDIR = " ++ show mNixLibDir
  putStrLn $ "HNIX_PACKAGE_DBS = " ++ show mNixPkgDbs
  putStrLn $ "ghc-paths libdir = " ++ libdir

  -- Use Nix-configured GHC if available
  let ghcLibDir = fromMaybe libdir mNixLibDir

  -- Parse colon-separated package DBs
  let splitOnColon [] = []
      splitOnColon xs = case L.break (== ':') xs of
        (chunk, []) -> [chunk]
        (chunk, _:rest) -> chunk : splitOnColon rest
      pkgDbs = case mNixPkgDbs of
        Just dbs -> filter (not . null) $ splitOnColon dbs
        Nothing -> []

  putStrLn ""
  putStrLn "=== Test 2: Create GHC session with package DBs ==="
  putStrLn $ "Adding " ++ show (length pkgDbs) ++ " package DBs"
  -- Find the unit ID for compile-runtime
  mRuntimeUnitId <- findCompileRuntimeUnitId pkgDbs
  putStrLn $ "Runtime unit ID: " ++ show mRuntimeUnitId
  hsc <- runGhc (Just ghcLibDir) $ do
    dflags <- getSessionDynFlags
    -- IMPORTANT: Disable automatic environment file loading
    -- Append our package DBs to the defaults (don't clear)
    let exposeFlags = case mRuntimeUnitId of
          Just unitId -> [ExposePackage ("-package-id " <> unitId)
                            (UnitIdArg (stringToUnit unitId))
                            (ModRenaming True [])]
          Nothing -> []
        dflags' = dflags
          { packageEnv = Just "-"  -- Disable .ghc.environment file
          , packageDBFlags = packageDBFlags dflags ++ map (PackageDB . PkgDbPath) pkgDbs
          , packageFlags = packageFlags dflags ++ exposeFlags
          }
    _ <- setSessionDynFlags dflags'
    getSession
  putStrLn "GHC session created"

  putStrLn ""
  putStrLn "=== Test 3: Debug runtime refs ==="
  debugLoadRuntimeRefs hsc

  putStrLn ""
  putStrLn "=== Test 4: Full initSession ==="
  result <- initSession
  case result of
    Left err -> putStrLn $ "FAILED: " ++ show err
    Right session -> do
      putStrLn "initSession: SUCCESS!"
      putStrLn ""
      putStrLn "=== Test 5: evalNixText \"1 + 2\" ==="
      evalResult <- evalNixText session "1 + 2"
      case evalResult of
        Left err -> putStrLn $ "Eval FAILED: " ++ show err
        Right val -> putStrLn $ "Eval SUCCESS: " ++ show val
      closeSession session

-- | Find the unit ID for hnix-compile-runtime from package DB directories.
findCompileRuntimeUnitId :: [FilePath] -> IO (Maybe String)
findCompileRuntimeUnitId [] = pure Nothing
findCompileRuntimeUnitId (dbPath:rest) = do
  exists <- doesDirectoryExist dbPath
  if not exists
    then findCompileRuntimeUnitId rest
    else do
      files <- listDirectory dbPath
      let confFiles = filter (".conf" `isSuffixOf`) files
      mUnitId <- findInFiles dbPath confFiles
      case mUnitId of
        Just uid -> pure (Just uid)
        Nothing -> findCompileRuntimeUnitId rest
  where
    findInFiles _ [] = pure Nothing
    findInFiles dir (f:fs)
      | "compile-runtime" `isInfixOf` f = do
          contents <- readFileBS (dir </> f)
          let contentStr = decodeUtf8 contents :: Text
              idLine = L.find ("id:" `isPrefixOf`) (map toString (Relude.lines contentStr))
          case idLine of
            Just line -> pure (Just (dropWhile (== ' ') (drop 3 line)))
            Nothing -> findInFiles dir fs
      | otherwise = findInFiles dir fs
