{-# LANGUAGE NoStrict #-}

-- | Global runtime state for the compiled Nix evaluator.
--
-- This module manages global state that the compiled Nix runtime needs to
-- access during evaluation. The state includes:
--
-- 1. IO handlers: Functions for filesystem access, imports, etc.
-- 2. Import state: Cache of evaluated imports + pending import tracking
--
-- == Why environment variable for handlers?
--
-- When using GHC's bytecode interpreter, the host code and interpreter code
-- may load separate copies of Haskell libraries. This means:
-- - Haskell IORefs are NOT shared between them - each has its own copy
-- - C global variables are also NOT shared - each library instance has its own
--
-- To work around this, we use an environment variable to store the StablePtr
-- address. Environment variables ARE shared across the entire process, so both
-- host and interpreter can access the same handlers.
--
-- == Thread safety
--
-- The handlers pointer is set once at startup and read many times.
--
-- The import state uses atomic operations to prevent TOCTOU races:
-- - checkAndMarkPending: atomically check cache, check pending, and mark pending
-- - cacheAndUnmarkPending: atomically cache result and unmark pending
-- - unmarkPending: atomically unmark pending (for error cleanup)
--
-- This ensures that concurrent imports of the same file will correctly
-- detect cycles or wait for the first import to complete, rather than
-- both racing to import.
--
-- == Usage pattern
--
-- @
-- main = do
--   let handlers = IOHandlers {...}
--   initRuntime handlers
--   result <- evaluate someExpr
--   resetRuntime  -- Optional: clean up for testing
-- @
module Nix.Compile.Runtime
  ( -- * Import state types
    ImportCheck(..)
    -- * Atomic import state operations
  , checkAndMarkPending
  , cacheAndUnmarkPending
  , unmarkPending
    -- * Initialization
  , initRuntime
  , resetRuntime
    -- * Access
  , getHandlers
  ) where

import Relude hiding (readIORef, writeIORef, newIORef, atomicModifyIORef')
import Data.IORef (readIORef, writeIORef, newIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.IO.Unsafe (unsafePerformIO)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr, castStablePtrToPtr, castPtrToStablePtr)
import Foreign.Ptr (ptrToWordPtr, wordPtrToPtr)
import qualified System.Environment as Env

import Nix.Types.Path (Path)
import Nix.Compile.Value (NixValue)
import Nix.Compile.Value.IO (IOHandlers)

-- | Environment variable name for storing the handlers pointer.
-- Using a unique name to avoid conflicts with other programs.
handlersEnvVar :: String
handlersEnvVar = "HNIX_RUNTIME_HANDLERS_PTR"

-- ============================================================================
-- Import State Management
-- ============================================================================

-- | Combined state for import tracking.
--
-- This combines the cache and pending sets into a single structure so
-- that we can atomically check-and-modify both, avoiding TOCTOU races.
--
-- Without atomic operations, a race could occur:
--   Thread 1: check cache -> miss
--   Thread 2: check cache -> miss
--   Thread 1: check pending -> not pending
--   Thread 2: check pending -> not pending
--   Thread 1: mark pending
--   Thread 2: mark pending -> spurious "already pending" = false cycle!
--
-- With atomic check-and-mark, Thread 2 would see Thread 1's pending mark.
data ImportState = ImportState
  { isCache   :: !(HashMap Path NixValue)  -- ^ Completed imports
  , isPending :: !(HashSet Path)           -- ^ In-progress imports
  }

-- | Result of checking import state atomically.
data ImportCheck
  = CacheHit !NixValue    -- ^ Already evaluated, use this value
  | AlreadyPending        -- ^ Cycle detected (file is being evaluated)
  | NotStarted            -- ^ Safe to proceed (atomically marked as pending)

-- | Global import state reference.
--
-- Unlike the handlers pointer, the import state uses a regular IORef.
-- This is fine because:
-- 1. The state is only used by the interpreter (not the host)
-- 2. It's okay if host and interpreter have separate states
-- 3. Atomic operations ensure thread safety within the interpreter
{-# NOINLINE globalImportState #-}
globalImportState :: IORef ImportState
globalImportState = unsafePerformIO $ newIORef (ImportState HM.empty HS.empty)

-- | Atomically check and mark a path as pending.
--
-- This operation is atomic to prevent TOCTOU races between checking the
-- cache/pending status and marking as pending.
--
-- Returns:
-- - CacheHit v: File already evaluated, use cached value v
-- - AlreadyPending: File currently being evaluated (cycle detected!)
-- - NotStarted: File not in cache or pending; NOW marked as pending
checkAndMarkPending :: Path -> IO ImportCheck
checkAndMarkPending path = atomicModifyIORef' globalImportState $ \st ->
  case HM.lookup path (isCache st) of
    Just v  -> (st, CacheHit v)
    Nothing
      | HS.member path (isPending st) -> (st, AlreadyPending)
      | otherwise -> (st { isPending = HS.insert path (isPending st) }, NotStarted)

-- | Atomically cache a result and unmark pending.
--
-- Called after successful evaluation. The path is removed from pending
-- and added to the cache in a single atomic operation.
cacheAndUnmarkPending :: Path -> NixValue -> IO ()
cacheAndUnmarkPending path value = atomicModifyIORef' globalImportState $ \st ->
  ( st { isCache = HM.insert path value (isCache st)
       , isPending = HS.delete path (isPending st)
       }
  , ()
  )

-- | Atomically unmark a path as pending without caching.
--
-- Called on evaluation error to clean up the pending set. This ensures
-- we don't leave stale pending entries that would cause false cycle
-- detection on retry.
unmarkPending :: Path -> IO ()
unmarkPending path = atomicModifyIORef' globalImportState $ \st ->
  (st { isPending = HS.delete path (isPending st) }, ())

-- | Global reference to the current StablePtr (so we can free it on reset).
-- This is only used by the host code for cleanup.
{-# NOINLINE globalStablePtr #-}
globalStablePtr :: IORef (Maybe (StablePtr IOHandlers))
globalStablePtr = unsafePerformIO $ newIORef Nothing

-- | Initialize the runtime before evaluation.
--
-- This must be called before any evaluation that uses IO operations
-- (import, readFile, readDir, pathExists, getEnv).
--
-- Creates a StablePtr to the handlers and stores its address in an
-- environment variable that can be read by both the host and the
-- bytecode interpreter.
--
-- Clears the import cache from any previous evaluation session.
--
-- Example:
-- @
-- let handlers = IOHandlers
--       { handleImport = myImportHandler
--       , handleReadFile = readFile
--       , handleReadDir = myReadDir
--       , handlePathExists = doesPathExist
--       , handleGetEnv = lookupEnv . toString
--       }
-- initRuntime handlers
-- @
initRuntime :: IOHandlers -> IO ()
initRuntime handlers = do
  -- Free any existing StablePtr from a previous session
  mOldPtr <- readIORef globalStablePtr
  case mOldPtr of
    Just oldPtr -> freeStablePtr oldPtr
    Nothing -> pure ()

  -- Create a new StablePtr to the handlers
  ptr <- newStablePtr handlers

  -- Convert StablePtr to a Word for storage in environment variable
  let ptrAddr = ptrToWordPtr (castStablePtrToPtr ptr)
      ptrStr = show ptrAddr

  -- Store in environment variable (process-wide, survives library reloads)
  Env.setEnv handlersEnvVar ptrStr

  -- Remember the StablePtr so we can free it later
  writeIORef globalStablePtr (Just ptr)

  -- Clear the import state (cache and pending)
  writeIORef globalImportState (ImportState HM.empty HS.empty)

-- | Reset runtime state to uninitialized.
--
-- Useful for:
-- - Testing: ensures clean state between test cases
-- - Long-running processes: releases cached values for GC
-- - Error recovery: resets to known state after failures
--
-- IMPORTANT: This function must NOT be called while evaluation is in progress.
-- Calling resetRuntime during evaluation causes undefined behavior because
-- it frees the StablePtr that getHandlers may be about to dereference.
-- The typical safe usage is:
--
-- @
-- initRuntime handlers
-- result <- evaluate someExpr  -- Evaluation completes fully
-- resetRuntime                 -- Only reset after evaluation finishes
-- @
resetRuntime :: IO ()
resetRuntime = do
  -- Free the StablePtr if one exists
  mOldPtr <- readIORef globalStablePtr
  case mOldPtr of
    Just oldPtr -> freeStablePtr oldPtr
    Nothing -> pure ()

  -- Clear the Haskell reference
  writeIORef globalStablePtr Nothing

  -- Unset the environment variable
  Env.unsetEnv handlersEnvVar

  -- Clear the import state (cache and pending)
  writeIORef globalImportState (ImportState HM.empty HS.empty)

-- | Get the IO handlers for use in primops.
--
-- This function uses unsafePerformIO so it can be called from "pure" code
-- (primops that need to do IO). The IO is safe because:
-- 1. The handlers are set once at startup and never modified
-- 2. Reading from an environment variable is safe
-- 3. Dereferencing a StablePtr is safe as long as the ptr is valid
--
-- Throws an error if called before 'initRuntime'. This is a programming
-- error - the driver must initialize the runtime before evaluation.
--
-- Note: Despite using unsafePerformIO, this function has observable effects
-- (it may throw). The NOINLINE pragma prevents GHC from floating out the
-- read or sharing it inappropriately across different evaluation sessions.
getHandlers :: IOHandlers
getHandlers = unsafePerformIO $ do
  mPtrStr <- Env.lookupEnv handlersEnvVar
  case mPtrStr of
    Nothing -> error "Nix.Compile.Runtime: IOHandlers not initialized. Call initRuntime before evaluation."
    Just ptrStr -> do
      case readMaybe ptrStr of
        Nothing -> error $ "Nix.Compile.Runtime: Invalid handlers pointer in environment: " <> toText ptrStr
        Just ptrAddr -> do
          let ptr = wordPtrToPtr ptrAddr
              stablePtr = castPtrToStablePtr ptr :: StablePtr IOHandlers
          deRefStablePtr stablePtr
{-# NOINLINE getHandlers #-}
