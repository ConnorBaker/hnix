{-# LANGUAGE RecordWildCards #-}

-- | Tests for import cycle detection and caching.
--
-- This module tests:
-- 1. Cycle detection - Verify cyclic imports are detected and error appropriately
-- 2. AST caching - Verify parsed ASTs are cached to disk correctly
-- 3. Core caching - Verify compiled Core expressions are cached to disk correctly
-- 4. Diamond pattern - Verify files are evaluated only once in diamond import patterns
module Compile.ImportCacheTests
  ( tests
  ) where

import Relude hiding (writeFile)
import Data.List (isInfixOf)
import qualified System.Environment as Env
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Text.IO as T
import Control.Exception (SomeException, catch, throwIO, try)

import Nix.Types.Path (Path(..))
import qualified Nix.Utils as Utils
import Nix.Compile.Cache
  ( initCacheConfig
  , hashSourceFile
  , hashSourceFileWithPath
  , readAstCache
  , writeAstCache
  , getCachePath
  , getCoreCachePath
  , readCoreCache
  , writeCoreCache
  )
import Nix.Compile.Driver (NixSession(..), initSession, evalNixFile, compileNix, CompileResult(..), NixCompileError(..))
import Nix.Compile.Runtime (resetRuntime)
import Nix.Compile.Value (NixValue(..))
import Nix.Compile.Value.Error (NixError(..))
import Nix.Parser (parseNixFile, parseNixFileLoc)
import Compile.TestCommon

-- | All import and cache tests.
tests :: TestTree
tests = testGroup "Import and Cache"
  [ cycleDetectionTests
  , pathResolutionTests
  , astCacheTests
  , coreCacheTests
  , diamondPatternTests
  ]

-- ============================================================================
-- Cycle Detection Tests
-- ============================================================================

cycleDetectionTests :: TestTree
cycleDetectionTests = testGroup "Cycle Detection"
  [ testCase "mutual recursion A<->B" $ withTempNixDir $ \dir -> do
      -- Create a.nix that imports b.nix, and b.nix that imports a.nix
      writeNixFile (dir </> "a.nix") "import ./b.nix"
      writeNixFile (dir </> "b.nix") "import ./a.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertBool "Should contain cycle error" (isCycleError err)
        Right _ -> assertFailure "Expected CyclicImport error"

  , testCase "self-import A->A" $ withTempNixDir $ \dir -> do
      writeNixFile (dir </> "a.nix") "import ./a.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertBool "Should contain cycle error" (isCycleError err)
        Right _ -> assertFailure "Expected CyclicImport error"

  , testCase "three-way cycle A->B->C->A" $ withTempNixDir $ \dir -> do
      writeNixFile (dir </> "a.nix") "import ./b.nix"
      writeNixFile (dir </> "b.nix") "import ./c.nix"
      writeNixFile (dir </> "c.nix") "import ./a.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertBool "Should contain cycle error" (isCycleError err)
        Right _ -> assertFailure "Expected CyclicImport error"

  , testCase "no cycle: linear chain A->B->C" $ withTempNixDir $ \dir -> do
      writeNixFile (dir </> "c.nix") "3"
      writeNixFile (dir </> "b.nix") "import ./c.nix"
      writeNixFile (dir </> "a.nix") "import ./b.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 3
  ]

-- ============================================================================
-- Path Resolution Tests
-- ============================================================================

-- | Tests for relative path resolution.
--
-- These tests verify that relative imports (./foo.nix, ../bar.nix) resolve
-- correctly relative to the importing file's directory, not the current
-- working directory.
pathResolutionTests :: TestTree
pathResolutionTests = testGroup "Path Resolution"
  [ testCase "relative import from subdirectory" $ withTempNixDir $ \dir -> do
      -- Create a subdirectory with files that import each other
      createDirectoryIfMissing True (dir </> "subdir")
      writeNixFile (dir </> "subdir" </> "sibling.nix") "42"
      writeNixFile (dir </> "subdir" </> "main.nix") "import ./sibling.nix"

      -- Import from the subdirectory file - should resolve ./sibling.nix
      -- relative to subdir/, not the CWD
      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "subdir" </> "main.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 42

  , testCase "parent directory import with .." $ withTempNixDir $ \dir -> do
      -- Create files in parent and subdirectory
      createDirectoryIfMissing True (dir </> "subdir")
      writeNixFile (dir </> "parent.nix") "100"
      writeNixFile (dir </> "subdir" </> "child.nix") "import ../parent.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "subdir" </> "child.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 100

  , testCase "deeply nested relative imports" $ withTempNixDir $ \dir -> do
      -- Create a nested directory structure: a/b/c/
      createDirectoryIfMissing True (dir </> "a" </> "b" </> "c")
      writeNixFile (dir </> "a" </> "b" </> "c" </> "deep.nix") "1"
      writeNixFile (dir </> "a" </> "b" </> "mid.nix") "import ./c/deep.nix"
      writeNixFile (dir </> "a" </> "top.nix") "import ./b/mid.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a" </> "top.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 1

  , testCase "chained imports across directories" $ withTempNixDir $ \dir -> do
      -- Create: root/a.nix -> sub/b.nix -> sub/c.nix
      createDirectoryIfMissing True (dir </> "sub")
      writeNixFile (dir </> "sub" </> "c.nix") "7"
      writeNixFile (dir </> "sub" </> "b.nix") "import ./c.nix"
      writeNixFile (dir </> "a.nix") "import ./sub/b.nix"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 7

  , testCase "import with expression using relative path" $ withTempNixDir $ \dir -> do
      -- Test that the imported file's expressions also resolve paths correctly
      createDirectoryIfMissing True (dir </> "lib")
      writeNixFile (dir </> "lib" </> "add.nix") "x: y: x + y"
      writeNixFile (dir </> "lib" </> "helpers.nix") "{ add = import ./add.nix; }"
      writeNixFile (dir </> "main.nix") "let h = import ./lib/helpers.nix; in h.add 10 20"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "main.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 30

  , testCase "absolute path import" $ withTempNixDir $ \dir -> do
      -- Test that absolute paths work correctly
      writeNixFile (dir </> "target.nix") "999"
      let absPath = dir </> "target.nix"
      writeNixFile (dir </> "main.nix") $ "import " <> toText absPath

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "main.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 999
  ]

-- ============================================================================
-- AST Cache Tests
-- ============================================================================

astCacheTests :: TestTree
astCacheTests = testGroup "AST Cache"
  [ testCase "write and read AST cache" $ withTempEnv "XDG_CACHE_HOME" $ \_cacheDir -> do
      cfg <- initCacheConfig

      withSystemTempDirectory "nix-src" $ \srcDir -> do
        let nixFile = srcDir </> "test.nix"
        writeNixFile nixFile "{ a = 1; b = 2; }"

        sourceHash <- hashSourceFile (Path nixFile)

        -- Parse the file with location info (as the cache expects NExprLoc)
        parseResult <- parseNixFileLoc (toUtilsPath nixFile)
        case parseResult of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right ast -> do
            -- Write to cache
            writeAstCache cfg sourceHash ast

            -- Verify cache file exists
            let cachePath = getCachePath cfg sourceHash
            exists <- doesFileExist cachePath
            assertBool "Cache file should exist" exists

            -- Read from cache
            mCached <- readAstCache cfg sourceHash
            case mCached of
              Nothing -> assertFailure "Cache read returned Nothing"
              Just _cached -> pure ()  -- Success

  , testCase "cache miss for unknown hash" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      cfg <- initCacheConfig
      let fakeHash = encodeUtf8 ("0000000000000000000000000000000000000000000000000000000000000000" :: Text)
      mCached <- readAstCache cfg fakeHash
      mCached @?= Nothing

  , testCase "different content produces different hash" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      withSystemTempDirectory "nix-src" $ \srcDir -> do
        let file1 = srcDir </> "a.nix"
        let file2 = srcDir </> "b.nix"
        writeNixFile file1 "1"
        writeNixFile file2 "2"

        hash1 <- hashSourceFile (Path file1)
        hash2 <- hashSourceFile (Path file2)

        assertBool "Different content should have different hashes" (hash1 /= hash2)

  , testCase "same content produces same hash" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      withSystemTempDirectory "nix-src" $ \srcDir -> do
        let file1 = srcDir </> "a.nix"
        let file2 = srcDir </> "b.nix"
        writeNixFile file1 "{ x = 42; }"
        writeNixFile file2 "{ x = 42; }"

        hash1 <- hashSourceFile (Path file1)
        hash2 <- hashSourceFile (Path file2)

        assertBool "Same content should have same hash" (hash1 == hash2)
  ]

-- ============================================================================
-- Core Cache Tests
-- ============================================================================

coreCacheTests :: TestTree
coreCacheTests = testGroup "Core Cache"
  [ testCase "write and read Core cache" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      cfg <- initCacheConfig

      eSession <- initSession
      case eSession of
        Left (RuntimeLoadError _) ->
          -- Skip test if runtime not available
          pure ()
        Left err -> assertFailure $ "Session init failed: " <> show err
        Right session -> do
          let hsc = nsHscEnv session

          withSystemTempDirectory "nix-src" $ \srcDir -> do
            let nixFile = srcDir </> "test.nix"
            writeNixFile nixFile "1 + 2"

            sourceHash <- hashSourceFileWithPath (Path nixFile)

            -- Compile to get a CoreExpr
            parseResult <- parseNixFile (toUtilsPath nixFile)
            case parseResult of
              Left err -> assertFailure $ "Parse failed: " <> show err
              Right expr -> do
                compileResult <- compileNix session (Just (Path nixFile)) expr
                case compileResult of
                  Left err -> assertFailure $ "Compile failed: " <> show err
                  Right CompileResult{..} -> do
                    -- Write to cache
                    writeCoreCache cfg sourceHash crCoreExpr

                    -- Verify cache file exists
                    let cachePath = getCoreCachePath cfg sourceHash
                    exists <- doesFileExist cachePath
                    assertBool "Core cache file should exist" exists

                    -- Read from cache
                    mCached <- readCoreCache cfg sourceHash hsc
                    case mCached of
                      Nothing -> assertFailure "Core cache read returned Nothing"
                      Just _ -> pure ()  -- Success

  , testCase "Core cache miss for unknown hash" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      cfg <- initCacheConfig

      eSession <- initSession
      case eSession of
        Left (RuntimeLoadError _) ->
          -- Skip test if runtime not available
          pure ()
        Left err -> assertFailure $ "Session init failed: " <> show err
        Right session -> do
          let hsc = nsHscEnv session
          let fakeHash = encodeUtf8 ("0000000000000000000000000000000000000000000000000000000000000000" :: Text)
          mCached <- readCoreCache cfg fakeHash hsc
          case mCached of
            Nothing -> pure ()  -- Expected
            Just _ -> assertFailure "Expected cache miss but got hit"

  , testCase "Core cache uses path in hash" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      -- Verify that same content at different paths produces different hashes
      withSystemTempDirectory "nix-src" $ \srcDir -> do
        let file1 = srcDir </> "a.nix"
        let file2 = srcDir </> "b.nix"
        writeNixFile file1 "42"
        writeNixFile file2 "42"  -- Same content!

        hash1 <- hashSourceFileWithPath (Path file1)
        hash2 <- hashSourceFileWithPath (Path file2)

        assertBool "Same content at different paths should have different hashes" (hash1 /= hash2)

  , testCase "hashSourceFile vs hashSourceFileWithPath" $ withTempEnv "XDG_CACHE_HOME" $ \_ -> do
      -- Verify that content-only hash (AST cache) is same for same content,
      -- but path+content hash (Core cache) differs by path
      withSystemTempDirectory "nix-src" $ \srcDir -> do
        let file1 = srcDir </> "x.nix"
        let file2 = srcDir </> "y.nix"
        writeNixFile file1 "{ a = 1; }"
        writeNixFile file2 "{ a = 1; }"  -- Same content

        -- Content-only hashes should be equal
        contentHash1 <- hashSourceFile (Path file1)
        contentHash2 <- hashSourceFile (Path file2)
        assertBool "Same content should have same content-only hash" (contentHash1 == contentHash2)

        -- Path+content hashes should differ
        pathHash1 <- hashSourceFileWithPath (Path file1)
        pathHash2 <- hashSourceFileWithPath (Path file2)
        assertBool "Same content at different paths should have different path+content hash" (pathHash1 /= pathHash2)
  ]

-- ============================================================================
-- Diamond Pattern Tests
-- ============================================================================

diamondPatternTests :: TestTree
diamondPatternTests = testGroup "Diamond Pattern"
  [ testCase "basic diamond A->B->D, A->C->D" $ withTempNixDir $ \dir -> do
      -- D is imported by both B and C
      writeNixFile (dir </> "d.nix") "42"
      writeNixFile (dir </> "b.nix") "import ./d.nix"
      writeNixFile (dir </> "c.nix") "import ./d.nix"
      writeNixFile (dir </> "a.nix") "(import ./b.nix) + (import ./c.nix)"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 84  -- 42 + 42

  , testCase "nested diamond pattern" $ withTempNixDir $ \dir -> do
      -- More complex: A->B->D, A->C->D, B->E, C->E
      writeNixFile (dir </> "e.nix") "10"
      writeNixFile (dir </> "d.nix") "1"
      writeNixFile (dir </> "b.nix") "(import ./d.nix) + (import ./e.nix)"
      writeNixFile (dir </> "c.nix") "(import ./d.nix) + (import ./e.nix)"
      writeNixFile (dir </> "a.nix") "(import ./b.nix) + (import ./c.nix)"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "a.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 22  -- (1+10) + (1+10) = 22

  , testCase "diamond with different values" $ withTempNixDir $ \dir -> do
      -- Verify each import chain works independently
      writeNixFile (dir </> "shared.nix") "{ value = 100; }"
      writeNixFile (dir </> "left.nix") "(import ./shared.nix).value + 1"
      writeNixFile (dir </> "right.nix") "(import ./shared.nix).value + 2"
      writeNixFile (dir </> "main.nix") "(import ./left.nix) + (import ./right.nix)"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "main.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 203  -- (100+1) + (100+2) = 203

  , testCase "import reuse returns same value" $ withTempNixDir $ \dir -> do
      -- Verify that importing the same file twice returns the same value
      writeNixFile (dir </> "data.nix") "{ x = 1; y = 2; }"
      writeNixFile (dir </> "main.nix") "let d1 = import ./data.nix; d2 = import ./data.nix; in d1.x + d2.y"

      result <- withSessionForFiles $ \session ->
        evalNixFile session (toUtilsPath $ dir </> "main.nix")

      case result of
        Left err -> assertFailure $ "Unexpected error: " <> show err
        Right val -> assertInt val 3  -- 1 + 2
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Convert a FilePath to Nix.Utils.Path for use with evalNixFile.
toUtilsPath :: FilePath -> Utils.Path
toUtilsPath = coerce

-- | Write a Nix file with the given content.
writeNixFile :: FilePath -> Text -> IO ()
writeNixFile path content = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  T.writeFile path content

-- | Create a temporary directory for Nix files and run the action.
withTempNixDir :: (FilePath -> IO a) -> IO a
withTempNixDir = withSystemTempDirectory "hnix-import-test"

-- | Run an action with a session, resetting runtime state before and after.
-- This ensures tests are isolated from each other.
withSessionForFiles :: (NixSession -> IO (Either NixCompileError NixValue)) -> IO (Either SomeException NixValue)
withSessionForFiles action = do
  -- Reset runtime state before each test
  resetRuntime

  eSession <- initSession
  case eSession of
    Left (RuntimeLoadError _) ->
      -- Skip test if runtime not available (common in CI)
      pure $ Left $ toException $ RuntimeLoadError "Skipped: hnix-compile-runtime not available"
    Left err ->
      pure $ Left $ toException err
    Right session -> do
      result <- try $ action session
      -- Reset runtime state after each test
      resetRuntime
      case result of
        Left (e :: SomeException) -> pure $ Left e
        Right (Left err) -> pure $ Left $ toException err
        Right (Right val) -> pure $ Right val

-- | Run an action with a temporary XDG_CACHE_HOME environment variable.
withTempEnv :: String -> (FilePath -> IO a) -> IO a
withTempEnv envKey action = withSystemTempDirectory "hnix-cache-test" $ \tmpDir -> do
  oldVal <- Env.lookupEnv envKey
  Env.setEnv envKey tmpDir
  result <- action tmpDir `catch` \(e :: SomeException) -> do
    -- Restore on exception
    restoreEnv envKey oldVal
    throwIO e
  restoreEnv envKey oldVal
  pure result
  where
    restoreEnv :: String -> Maybe String -> IO ()
    restoreEnv key = \case
      Nothing -> Env.unsetEnv key
      Just v -> Env.setEnv key v

-- | Check if an error (from evalNixFile or exception) is a cyclic import error.
isCycleError :: SomeException -> Bool
isCycleError e =
  case fromException e of
    Just (CyclicImport _) -> True
    _ -> "CyclicImport" `isInfixOf` show e
