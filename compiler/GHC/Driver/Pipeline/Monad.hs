{-# LANGUAGE DeriveFunctor #-}
-- | The CompPipeline monad and associated ops
--
-- Defined in separate module so that it can safely be imported from Hooks
module GHC.Driver.Pipeline.Monad (
    CompPipeline(..), evalP
  , PhasePlus(..)
  , PipeEnv(..), PipeState(..), PipelineOutput(..)
  , getPipeEnv, getPipeState, getPipeSession
  , setDynFlags, setModLocation, setForeignOs, setIface
  , pipeStateDynFlags, pipeStateModIface, setPlugins
  ) where

import GHC.Prelude

import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Logger

import GHC.Driver.Session
import GHC.Driver.Phases
import GHC.Driver.Env
import GHC.Driver.Plugins

import GHC.SysTools.FileCleanup (TempFileLifetime)

import GHC.Types.SourceFile

import GHC.Unit.Module
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Status

import Control.Monad

newtype CompPipeline a = P { unP :: PipeEnv -> PipeState -> IO (PipeState, a) }
    deriving (Functor)

evalP :: CompPipeline a -> PipeEnv -> PipeState -> IO (PipeState, a)
evalP (P f) env st = f env st

instance Applicative CompPipeline where
    pure a = P $ \_env state -> return (state, a)
    (<*>) = ap

instance Monad CompPipeline where
  P m >>= k = P $ \env state -> do (state',a) <- m env state
                                   unP (k a) env state'

instance MonadIO CompPipeline where
    liftIO m = P $ \_env state -> do a <- m; return (state, a)

data PhasePlus = RealPhase Phase
               | HscOut HscSource ModuleName HscStatus

instance Outputable PhasePlus where
    ppr (RealPhase p) = ppr p
    ppr (HscOut {}) = text "HscOut"

-- -----------------------------------------------------------------------------
-- The pipeline uses a monad to carry around various bits of information

-- PipeEnv: invariant information passed down
data PipeEnv = PipeEnv {
       stop_phase   :: Phase,       -- ^ Stop just before this phase
       src_filename :: String,      -- ^ basename of original input source
       src_basename :: String,      -- ^ basename of original input source
       src_suffix   :: String,      -- ^ its extension
       output_spec  :: PipelineOutput -- ^ says where to put the pipeline output
  }

-- PipeState: information that might change during a pipeline run
data PipeState = PipeState {
       hsc_env   :: HscEnv,
          -- ^ only the DynFlags and the Plugins change in the HscEnv.  The
          -- DynFlags change at various points, for example when we read the
          -- OPTIONS_GHC pragmas in the Cpp phase.
       maybe_loc :: Maybe ModLocation,
          -- ^ the ModLocation.  This is discovered during compilation,
          -- in the Hsc phase where we read the module header.
       foreign_os :: [FilePath],
         -- ^ additional object files resulting from compiling foreign
         -- code. They come from two sources: foreign stubs, and
         -- add{C,Cxx,Objc,Objcxx}File from template haskell
       iface :: Maybe (ModIface, ModDetails)
         -- ^ Interface generated by HscOut phase. Only available after the
         -- phase runs.
  }

pipeStateDynFlags :: PipeState -> DynFlags
pipeStateDynFlags = hsc_dflags . hsc_env

pipeStateModIface :: PipeState -> Maybe (ModIface, ModDetails)
pipeStateModIface = iface

data PipelineOutput
  = Temporary TempFileLifetime
        -- ^ Output should be to a temporary file: we're going to
        -- run more compilation steps on this output later.
  | Persistent
        -- ^ We want a persistent file, i.e. a file in the current directory
        -- derived from the input filename, but with the appropriate extension.
        -- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile
        -- ^ The output must go into the specific outputFile in DynFlags.
        -- We don't store the filename in the constructor as it changes
        -- when doing -dynamic-too.
    deriving Show

getPipeEnv :: CompPipeline PipeEnv
getPipeEnv = P $ \env state -> return (state, env)

getPipeState :: CompPipeline PipeState
getPipeState = P $ \_env state -> return (state, state)

getPipeSession :: CompPipeline HscEnv
getPipeSession = P $ \_env state -> return (state, hsc_env state)

instance HasDynFlags CompPipeline where
    getDynFlags = P $ \_env state -> return (state, hsc_dflags (hsc_env state))

instance HasLogger CompPipeline where
    getLogger = P $ \_env state -> return (state, hsc_logger (hsc_env state))

setDynFlags :: DynFlags -> CompPipeline ()
setDynFlags dflags = P $ \_env state ->
  return (state{hsc_env= (hsc_env state){ hsc_dflags = dflags }}, ())

setPlugins :: [LoadedPlugin] -> [StaticPlugin] -> CompPipeline ()
setPlugins dyn static = P $ \_env state ->
  let hsc_env' = (hsc_env state){ hsc_plugins = dyn, hsc_static_plugins = static }
  in return (state{hsc_env = hsc_env'}, ())

setModLocation :: ModLocation -> CompPipeline ()
setModLocation loc = P $ \_env state ->
  return (state{ maybe_loc = Just loc }, ())

setForeignOs :: [FilePath] -> CompPipeline ()
setForeignOs os = P $ \_env state ->
  return (state{ foreign_os = os }, ())

setIface :: ModIface -> ModDetails -> CompPipeline ()
setIface iface details = P $ \_env state -> return (state{ iface = Just (iface, details) }, ())
