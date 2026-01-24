{-# LANGUAGE RecordWildCards #-}

-- | GHC session management and evaluation driver.
--
-- This module provides the high-level API for compiling and evaluating
-- Nix expressions using the GHC Core compilation pipeline:
--
-- 1. Initialize a GHC session with the runtime library loaded
-- 2. Compile NExpr to GHC Core
-- 3. Use hscCompileCoreExpr to generate bytecode
-- 4. Execute the bytecode and extract the NixValue result
--
-- Usage:
--
-- @
-- main = do
--   session <- initSession
--   result <- evalNix session (parseNixText "1 + 2 * 3")
--   print result  -- VInt 7
-- @
module Nix.Compile.Driver
  ( -- * GHC Session
    NixSession(..)
  , initSession
  , closeSession
  , withSession
    -- * Evaluation
  , evalNix
  , evalNixFile
  , evalNixText
    -- * Compilation (lower-level)
  , compileNix
  , CompileResult(..)
    -- * Error types
  , NixCompileError(..)
  ) where

import Relude hiding (lookupEnv)
import Control.Exception (bracket, throwIO)
import qualified Control.Monad.Trans.Except as E
import GHC hiding (compileExpr)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Core
import GHC.Core.Make (mkCoreApps, mkStringExprFSWith)
import GHC.Data.FastString (mkFastString)
import GHC.Driver.Env
import GHC.Driver.Main (hscCompileCoreExpr)
import GHC.Driver.Session
import GHC.Unit.Types (stringToUnit)
import GHC.Paths (libdir)
import GHC.Runtime.Interpreter (wormhole)
import GHC.Core.Type
import GHC.Types.Id (mkLocalId)
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Unique.Supply
import System.Directory (doesDirectoryExist, doesPathExist, getCurrentDirectory, listDirectory, pathIsSymbolicLink)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn)

import Nix.Expr.Types (NExpr)
import Nix.Parser (parseNixFile, parseNixText)
import Nix.Utils (Path)
import qualified Nix.Utils as Utils
import qualified Nix.Types.Path as TypesPath
import Nix.Compile.Cache (CacheConfig, initCacheConfig, hashSourceFileWithPath, readCoreCache, writeCoreCache)
import Nix.Compile.Expr (compileExpr)
import Nix.Compile.Monad
import Nix.Compile.Refs
import Nix.Compile.Runtime (initRuntime)
import Nix.Compile.Value (NixValue)
import Nix.Compile.Value.IO (IOHandlers(..), FileType(..))

-- * Session Management

-- | An initialized GHC session with runtime library loaded.
data NixSession = NixSession
  { nsHscEnv :: !HscEnv
    -- ^ GHC session environment
  , nsRefs :: !RuntimeRefs
    -- ^ Pre-loaded runtime references
  , nsUniqSupply :: !UniqSupply
    -- ^ Unique supply for fresh names
  , nsCacheConfig :: !CacheConfig
    -- ^ Disk cache configuration for AST/Core caching
  }

-- | Errors that can occur during compilation.
data NixCompileError
  = ParseError !Text
    -- ^ Failed to parse Nix expression
  | CompileError !Text
    -- ^ Failed to compile to Core
  | RuntimeLoadError !Text
    -- ^ Failed to load runtime library
  | BytecodeError !Text
    -- ^ Failed to compile Core to bytecode
  | ExecutionError !Text
    -- ^ Error during execution
  deriving (Show, Eq)

instance Exception NixCompileError

-- | Initialize a GHC session for Nix evaluation.
--
-- This starts GHC in interpreted mode. The runtime modules are provided
-- by the hnix-compile-runtime package which should be installed in GHC's
-- package database.
--
-- IMPORTANT: For this to work, hnix-compile-runtime must be installed in
-- a package database that GHC can access. In a Nix environment, this requires
-- the package to be built and registered by Nix, not just Cabal.
--
-- The session should be closed with 'closeSession' when done.
initSession :: IO (Either NixCompileError NixSession)
initSession = runCompileM $ do
  -- Find GHC environment file or Cabal's package database
  mEnvFile <- liftIO findGhcEnvironmentFile
  mPkgDb <- liftIO findCabalPackageDb
  -- Check for Nix-provided package DBs (colon-separated list)
  mNixPkgDbs <- liftIO $ lookupEnv "HNIX_PACKAGE_DBS"
  -- Check for Nix-provided GHC library directory (overrides ghc-paths)
  -- This is necessary because ghc-paths is compiled at build time with a
  -- hard-coded path, but at runtime we may be using a different GHC.
  mNixLibDir <- liftIO $ lookupEnv "NIX_GHC_LIBDIR"

  -- Use NIX_GHC_LIBDIR if available, otherwise fall back to ghc-paths libdir.
  -- NIX_GHC_LIBDIR should point to the shell's GHC which is used for Cabal builds.
  let ghcLibDir = fromMaybe libdir mNixLibDir

  -- Parse environment file to get package database paths and package expose flags.
  -- We skip global-package-db because it contains Nix-built packages that have different
  -- unit IDs than the Cabal-built packages, which causes ABI conflicts.
  (envDbFlags, _envPkgFlags) <- case mEnvFile of
    Just envPath -> liftIO $ parseEnvFile envPath
    Nothing -> pure ([], [])

  -- Initialize GHC
  hsc <- liftIO $ runGhc (Just ghcLibDir) $ do
    -- Get default flags
    dflags <- getSessionDynFlags

    -- Configure for bytecode interpretation
    --
    -- Package resolution strategy (in priority order):
    -- 1. If a GHC environment file exists (.ghc.environment.*), parse it manually
    --    and set up package DBs and flags. This is the preferred method when building with Cabal.
    -- 2. Otherwise, use HNIX_PACKAGE_DBS (Nix-built packages) + manual config
    --
    -- Note: We can't rely on packageEnv in DynFlags because it doesn't seem to work
    -- correctly with the GHC API. Instead, we parse the environment file ourselves.

    let baseDflags = dflags
          { ghcLink = LinkInMemory
          , verbosity = 1  -- Some verbosity for debugging
          }

    -- Build package DB flags:
    -- 1. Cabal store DB (from env file)
    -- 2. Local build DB (from env file)
    -- NOTE: Don't use ClearPackageDBs - let GHC's default DBs be searched
    --       Don't add GlobalPkgDb again - it's already in GHC's defaults
    let buildDbFlags = [flag | flag@(PackageDB (PkgDbPath _)) <- envDbFlags]

    -- Expose just the compile-runtime package by unit ID
    let compileRuntimeFlag = ExposePackage "-package-id hnix-0.17.0-inplace-hnix-compile-runtime"
                               (UnitIdArg (stringToUnit "hnix-0.17.0-inplace-hnix-compile-runtime"))
                               (ModRenaming True [])

    let dflags' = case mEnvFile of
          Just _ ->
            -- When using environment file:
            -- - Add extra package DBs from env file
            -- - Just expose the compile-runtime package we need
            baseDflags { packageDBFlags = buildDbFlags, packageFlags = [compileRuntimeFlag] }
          Nothing ->
            -- Manual package database configuration:
            --
            -- Strategy: Use HNIX_PACKAGE_DBS (Nix-built packages) + GlobalPkgDb.
            -- The Nix-built packages depend on packages in GlobalPkgDb (aeson, hashable, etc).
            -- Do NOT add Cabal package DB when using Nix packages to avoid conflicts
            -- (both contain hnix-types/hnix-compile-runtime with different unit IDs).
            let nixPkgDbPaths = maybe [] (filter (not . null) . splitOnColon) mNixPkgDbs
                nixPkgDbFlags = map (PackageDB . PkgDbPath) nixPkgDbPaths
                -- Only use Cabal package DB if no Nix package DBs are available
                cabalPkgDbFlags = if null nixPkgDbFlags
                                  then maybe [] (\db -> [PackageDB (PkgDbPath db)]) mPkgDb
                                  else []
                -- Include GlobalPkgDb AFTER our Nix packages so our packages take priority
                -- This is important because GlobalPkgDb might have conflicting modules
                manualDbFlags = nixPkgDbFlags ++ [PackageDB GlobalPkgDb] ++ cabalPkgDbFlags
            in baseDflags { packageDBFlags = manualDbFlags }

    _ <- setSessionDynFlags dflags'

    -- Import the runtime modules into the session context.
    --
    -- IMPORTANT: Every module that 'Nix.Compile.Refs.loadRuntimeRefs' looks up
    -- identifiers from MUST be listed here. Importing a module via setContext
    -- causes GHC to load its interface file, which is required for lookupType
    -- to find the module's identifiers.
    --
    -- Without importing here, findModuleIO will succeed (module is discoverable
    -- in the package DB), but lookupType will return Nothing because the
    -- interface file was never loaded into the session.
    --
    -- If you add a new module to loadRuntimeRefs, add it here too!
    -- Use debugLoadRuntimeRefs to diagnose lookup failures.
    let runtimeImports =
          [ IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Value")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Value.Context")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.IO")
          -- Primops submodules (needed for lookupId to find definitions)
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Coerce")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Arithmetic")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Comparison")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Logical")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.String")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Collection")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Primops.Control")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins")
          -- Builtins submodules (needed for lookupId to find definitions)
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins.List")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins.Control")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins.AttrSet")
          , IIDecl $ simpleImportDecl (mkModuleName "Nix.Compile.Builtins.IO")
          , IIDecl $ simpleImportDecl (mkModuleName "Data.Vector")
          -- GHC.CString contains unpackCString# needed for string literal construction
          , IIDecl $ simpleImportDecl (mkModuleName "GHC.CString")
          ]
    -- Attempt to set context, logging any errors (don't swallow them silently)
    handleSourceError (\e -> liftIO $ hPutStrLn stderr $
      "Warning: setContext failed: " <> show e) $ setContext runtimeImports

    getSession

  -- Load runtime references (types and functions from hnix-compile-runtime)
  mRefs <- liftIO $ loadRuntimeRefs hsc
  refs <- case mRefs of
    Just r -> pure r
    Nothing -> throwError $ RuntimeLoadError $ mconcat
      [ "Failed to load runtime references.\n"
      , "This typically happens because hnix-compile-runtime is not installed in GHC's package database.\n\n"
      , "Package source configuration:\n"
      , case mEnvFile of
          Just env -> "  Using GHC environment file: " <> toText env <> "\n"
                   <> "  (HNIX_PACKAGE_DBS is ignored when environment file exists)\n"
          Nothing -> "  No GHC environment file found.\n"
                  <> case mNixPkgDbs of
                       Just dbs -> "  Using HNIX_PACKAGE_DBS: " <> toText dbs <> "\n"
                       Nothing -> "  HNIX_PACKAGE_DBS not set.\n"
      , case mPkgDb of
          Just db -> "  Cabal package DB: " <> toText db <> "\n"
          Nothing -> "  No Cabal package DB found.\n"
      , "\nTroubleshooting:\n"
      , "  1. Run: cabal build --write-ghc-environment-files=always\n"
      , "  2. Ensure hnix-compile-runtime is listed in the .ghc.environment.* file\n"
      , "  3. If in a pure Nix build, ensure HNIX_PACKAGE_DBS points to valid package DBs\n"
      ]

  -- Create unique supply
  supply <- liftIO $ mkSplitUniqSupply 'n'

  -- Initialize disk cache configuration
  cacheConfig <- liftIO initCacheConfig

  pure NixSession
    { nsHscEnv = hsc
    , nsRefs = refs
    , nsUniqSupply = supply
    , nsCacheConfig = cacheConfig
    }

-- | Find Cabal's package database directory.
-- Looks for dist-newstyle/packagedb/ghc-X.Y.Z/
-- Returns an absolute path.
findCabalPackageDb :: IO (Maybe FilePath)
findCabalPackageDb = do
  cwd <- getCurrentDirectory
  let distDir = cwd </> "dist-newstyle"
  exists <- doesDirectoryExist distDir
  if not exists
    then pure Nothing
    else do
      let pkgDbBase = distDir </> "packagedb"
      pkgDbExists <- doesDirectoryExist pkgDbBase
      if not pkgDbExists
        then pure Nothing
        else do
          -- Find ghc-X.Y.Z directories
          contents <- listDirectory pkgDbBase
          let ghcDirs = filter (\d -> "ghc-" `isPrefixOf` d) contents
          case ghcDirs of
            [] -> pure Nothing
            (d:_) -> do
              let pkgDb = pkgDbBase </> d
              isDir <- doesDirectoryExist pkgDb
              pure $ if isDir then Just pkgDb else Nothing

-- | Find GHC environment file created by Cabal.
-- Looks for .ghc.environment.* files in the current directory.
-- Returns absolute path since GHC needs it.
findGhcEnvironmentFile :: IO (Maybe FilePath)
findGhcEnvironmentFile = do
  cwd <- getCurrentDirectory
  contents <- listDirectory cwd
  let envFiles = filter (".ghc.environment." `isPrefixOf`) contents
  case envFiles of
    [] -> pure Nothing
    (f:_) -> pure (Just (cwd </> f))

-- | Split a string on colons (for HNIX_PACKAGE_DBS parsing).
splitOnColon :: String -> [String]
splitOnColon "" = []
splitOnColon s = case break (== ':') s of
  (before, "") -> [before]
  (before, _:rest) -> before : splitOnColon rest

-- | Parse a GHC environment file and extract package DB flags and package flags.
-- This manually parses the environment file instead of relying on GHC's packageEnv
-- mechanism, which doesn't seem to work correctly with the GHC API.
parseEnvFile :: FilePath -> IO ([PackageDBFlag], [PackageFlag])
parseEnvFile envPath = do
  contents <- readFileBS envPath
  let envDir = takeDirectory envPath
      ls = map toString $ Relude.lines $ decodeUtf8 contents
      -- Parse lines and accumulate flags
      (dbs, pkgs) = foldl' (parseLine envDir) ([], []) ls
  -- Reverse because we accumulated in reverse order
  pure (reverse dbs, reverse pkgs)
  where
    parseLine :: FilePath -> ([PackageDBFlag], [PackageFlag]) -> String -> ([PackageDBFlag], [PackageFlag])
    parseLine dir (dbs, pkgs) line
      | "--" `isPrefixOf` line = (dbs, pkgs)  -- Skip comments
      | null (dropWhile (== ' ') line) = (dbs, pkgs)  -- Skip empty lines
      -- Clear the package DB stack to start fresh
      | "clear-package-db" `isPrefixOf` line = ([ClearPackageDBs], pkgs)
      -- Skip global-package-db - we'll add it explicitly with the correct path
      -- based on the GHC lib directory we're using
      | "global-package-db" `isPrefixOf` line = (dbs, pkgs)
      | "user-package-db" `isPrefixOf` line = (PackageDB UserPkgDb : dbs, pkgs)
      | "package-db " `isPrefixOf` line =
          let path = drop 11 line
              absPath = if isAbsolute path then path else dir </> path
          in (PackageDB (PkgDbPath absPath) : dbs, pkgs)
      | "package-id " `isPrefixOf` line =
          let pkgId = drop 11 line
              -- Create an ExposePackage flag for each package-id
              flag = ExposePackage ("-package-id " <> pkgId)
                       (UnitIdArg (stringToUnit pkgId))
                       (ModRenaming True [])
          in (dbs, flag : pkgs)
      | otherwise = (dbs, pkgs)  -- Skip unknown lines

    -- Use simple string manipulation for path operations
    -- to avoid issues with relude's non-empty list functions
    takeDirectory :: FilePath -> FilePath
    takeDirectory "" = "."
    takeDirectory path =
      case lastIndexOf '/' path of
        Nothing -> "."
        Just 0 -> "/"
        Just i -> Relude.take i path

    lastIndexOf :: Char -> String -> Maybe Int
    lastIndexOf c s = go Nothing 0 s
      where
        go acc _ [] = acc
        go acc i (x:xs)
          | x == c = go (Just i) (i+1) xs
          | otherwise = go acc (i+1) xs

    isAbsolute :: FilePath -> Bool
    isAbsolute ('/':_) = True
    isAbsolute _ = False
-- | Close a GHC session, releasing resources.
closeSession :: NixSession -> IO ()
closeSession _session = do
  -- GHC sessions are cleaned up by the GC, but we could do
  -- explicit cleanup here if needed
  pure ()

-- | Run an action with a temporary session.
withSession :: (NixSession -> IO a) -> IO (Either NixCompileError a)
withSession action = do
  eSession <- initSession
  case eSession of
    Left err -> pure (Left err)
    Right session -> bracket
      (pure session)
      closeSession
      (fmap Right . action)

-- * Evaluation

-- | Result of compilation (before execution).
data CompileResult = CompileResult
  { crCoreExpr :: !CoreExpr
    -- ^ The compiled Core expression
  , crSession :: !NixSession
    -- ^ The session (needed for execution)
  }

-- | Compile a Nix expression to Core (without executing).
--
-- The optional 'Maybe Path' parameter specifies the source file being compiled.
-- When provided, relative imports will resolve relative to this file's directory.
-- When 'Nothing', we're compiling from a REPL or --expr context.
compileNix :: NixSession -> Maybe TypesPath.Path -> NExpr -> IO (Either NixCompileError CompileResult)
compileNix session@NixSession{..} mSourceFile expr = runCompileM $ do
  -- Create initial environment
  -- We need an initial NixEnv Id for 'with' scope tracking
  -- Split the supply: one for envId, rest for compilation
  let (supply1, supply2) = splitUniqSupply nsUniqSupply
      (envUniq, _) = takeUniqFromSupply supply1

      -- Create the env Id with the actual NixEnv type
      envOccName = mkVarOcc "nixEnv"
      envName = mkInternalName envUniq envOccName noSrcSpan
      nixEnvType = refNixEnvType nsRefs  -- Use actual NixEnv type
      envId = mkLocalId envName ManyTy nixEnvType

      env = initCompileEnv (hsc_dflags nsHscEnv) nsRefs envId

  -- Compile the expression
  bodyExpr <- liftIO $ runCompile (compileExpr expr) env supply2

  -- Wrap the body in a let binding: let nixEnv = <init> in <body>
  -- Use mkEnvWithCurrentFile when we have a source file, emptyEnv otherwise
  let initEnvExpr = case mSourceFile of
        Just (TypesPath.Path fp) ->
          -- mkEnvWithCurrentFile takes a Path, so we construct:
          -- mkEnvWithCurrentFile (mkPath "filepath")
          let pathLit = mkStringExprFSWith (refMkStringIds nsRefs) (mkFastString fp)
          in mkCoreApps (Var (refMkEnvWithCurrentFileId nsRefs))
                        [mkCoreApps (Var (refMkPathId nsRefs)) [pathLit]]
        Nothing ->
          Var (refEmptyEnvId nsRefs)
      coreExpr = Let (NonRec envId initEnvExpr) bodyExpr

  pure CompileResult
    { crCoreExpr = coreExpr
    , crSession = session
    }

-- | Evaluate a Nix expression to a value.
--
-- This is for expressions from text (REPL, --expr) where there's no source file.
-- For file-based evaluation, use 'evalNixFile' which passes the file path
-- for proper relative import resolution.
evalNix :: NixSession -> NExpr -> IO (Either NixCompileError NixValue)
evalNix session expr = runCompileM $ do
  -- Initialize runtime with handlers
  liftIO $ initRuntime (mkIOHandlers session)

  -- Compile to Core (no source file)
  CompileResult{..} <- E.ExceptT $ compileNix session Nothing expr

  -- Compile Core to bytecode and execute
  let hsc = nsHscEnv crSession

  -- hscCompileCoreExpr compiles a Core expression to bytecode
  -- and returns a ForeignHValue that we can extract
  (fhv, _, _) <- liftIO $ hscCompileCoreExpr hsc noSrcSpan crCoreExpr

  -- Extract the value using wormhole
  -- This reaches into GHC's runtime to get the actual Haskell value
  hvalue <- liftIO $ wormhole (hscInterp hsc) fhv

  -- Coerce from HValue to NixValue (HValue is just a boxed Any)
  -- Force the result deeply to ensure lazy imports (via unsafePerformIO)
  -- are fully evaluated before returning. This ensures all side effects
  -- from imports complete while the runtime is still initialized.
  let nixValue = unsafeCoerce hvalue :: NixValue
  pure $! nixValue `deepseq` nixValue

-- | Evaluate a Nix file.
--
-- The file path is passed to the compiler so that relative imports
-- resolve correctly relative to the file's directory.
--
-- Uses disk caching to avoid re-compilation of unchanged files:
-- 1. Hash the source file content
-- 2. Check Core cache (if hit, skip parse + compile)
-- 3. On miss: parse, compile, cache the Core, then execute
evalNixFile :: NixSession -> Path -> IO (Either NixCompileError NixValue)
evalNixFile session path = runCompileM $ do
  -- Initialize runtime FIRST (before any cache operations or evaluation).
  -- This must happen exactly once regardless of cache hit/miss.
  liftIO $ initRuntime (mkIOHandlers session)

  -- If path is a directory, append default.nix (matching Nix import semantics)
  finalPath <- liftIO $ pathToDefaultNix path

  let typesPath = coerce finalPath :: TypesPath.Path
      hsc = nsHscEnv session
      cacheConfig = nsCacheConfig session

  -- Hash the source file for cache lookup
  -- Use hashSourceFileWithPath because the compiled Core includes the
  -- file path (for relative import resolution), so different paths
  -- with the same content need different cache entries.
  sourceHash <- liftIO $ hashSourceFileWithPath typesPath

  -- Try Core cache first (skip parse + compile if hit)
  mCachedCore <- liftIO $ readCoreCache cacheConfig sourceHash hsc

  coreExpr <- case mCachedCore of
    Just cached -> do
      -- Cache hit! Use cached Core expression directly
      pure cached

    Nothing -> do
      -- Cache miss: parse and compile
      parseResult <- liftIO $ parseNixFile (coerce finalPath)
      expr <- case parseResult of
        Right e -> pure e
        Left doc -> throwError $ ParseError (show doc)

      -- Compile to Core
      CompileResult{..} <- E.ExceptT $ compileNix session (Just typesPath) expr

      -- Write compiled Core to cache (best-effort, errors ignored)
      liftIO $ writeCoreCache cacheConfig sourceHash crCoreExpr

      pure crCoreExpr

  -- Compile Core to bytecode and execute
  (fhv, _, _) <- liftIO $ hscCompileCoreExpr hsc noSrcSpan coreExpr

  -- Extract the value using wormhole
  hvalue <- liftIO $ wormhole (hscInterp hsc) fhv

  -- Coerce from HValue to NixValue
  -- Force the result deeply to ensure lazy imports (via unsafePerformIO)
  -- are fully evaluated before returning. This ensures all side effects
  -- from imports complete while the runtime is still initialized.
  let nixValue = unsafeCoerce hvalue :: NixValue
  pure $! nixValue `deepseq` nixValue

-- | Evaluate a Nix expression from text.
evalNixText :: NixSession -> Text -> IO (Either NixCompileError NixValue)
evalNixText session text = runCompileM $ do
  -- Parse the text
  expr <- case parseNixText text of
    Right e -> pure e
    Left doc -> throwError $ ParseError (show doc)

  -- Evaluate
  E.ExceptT $ evalNix session expr

-- * IO Handlers

-- | Create IO handlers for a session.
-- Note: IOHandlers uses Nix.Types.Path.Path while evalNixFile uses Nix.Utils.Path.
-- Both are newtypes over FilePath, so we coerce between them.
mkIOHandlers :: NixSession -> IOHandlers
mkIOHandlers session = IOHandlers
  { handleImport = \typesPath -> do
      -- Convert Nix.Types.Path.Path -> Nix.Utils.Path for evalNixFile
      let utilsPath = coerce typesPath :: Path
      -- If path is a directory, append default.nix (matching Nix semantics)
      finalPath <- pathToDefaultNix utilsPath
      result <- evalNixFile session finalPath
      case result of
        Right v -> pure v
        Left err -> throwIO err
  , handleReadFile = \(TypesPath.Path p) -> readFileText p
  , handleReadDir = \(TypesPath.Path p) -> do
      entries <- listDirectory p
      forM entries $ \name -> do
        let fullPath = p </> name
        ft <- getFileType fullPath
        pure (toText name, ft)
  , handlePathExists = \(TypesPath.Path p) -> doesPathExist p
  , handleGetEnv = \name -> fmap toText <$> lookupEnv (toString name)
  }
  where
    getFileType :: FilePath -> IO FileType
    getFileType path = do
      isDir <- doesDirectoryExist path
      if isDir
        then pure Directory
        else do
          isSymlink <- pathIsSymbolicLink path
          if isSymlink
            then pure Symlink
            else pure Regular

-- * Helpers

-- | If path is a directory, append default.nix (matching Nix import semantics).
--
-- Nix's import behavior: when importing a directory, it looks for default.nix
-- within that directory. This is used by both the CLI entry point and the
-- import handler.
pathToDefaultNix :: Path -> IO Path
pathToDefaultNix p = do
  isDir <- doesDirectoryExist (coerce p)
  pure $ if isDir
    then coerce (coerce @Path @FilePath p </> "default.nix")
    else p

-- * Type aliases for cleaner signatures

-- | ExceptT specialized for compile errors.
type CompileM = E.ExceptT NixCompileError IO

-- | Throw a compile error.
throwError :: NixCompileError -> CompileM a
throwError = E.throwE

-- | Run the compile monad.
runCompileM :: CompileM a -> IO (Either NixCompileError a)
runCompileM = E.runExceptT
