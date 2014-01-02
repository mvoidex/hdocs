module HDocs.Base (
	ModuleDocMap,
	withInitializedPackages, configSession
	) where

import Data.Map (Map)

import Documentation.Haddock (Doc)

import DynFlags
import GHC
import GHC.Paths
import qualified GhcMonad as GHC (liftIO)
import Packages

-- | Documentation in module
type ModuleDocMap = Map String (Doc String)

-- | Run action with initialized packages
withInitializedPackages :: [String] -> (DynFlags -> IO a) -> IO a
withInitializedPackages ghcOpts cont = do
	runGhc (Just libdir) $ do
		fs <- getSessionDynFlags
		defaultCleanupHandler fs $ do
			(fs', _, _) <- parseDynamicFlags fs (map noLoc ghcOpts)
			setSessionDynFlags fs'
			(result, _) <- GHC.liftIO $ initPackages fs
			GHC.liftIO $ cont result

-- | Config GHC session
configSession :: [String] -> IO DynFlags
configSession ghcOpts = do
	runGhc (Just libdir) $ do
		fs <- getSessionDynFlags
		defaultCleanupHandler fs $ do
			(fs', _, _) <- parseDynamicFlags fs (map noLoc ghcOpts)
			setSessionDynFlags fs'
			(result, _) <- GHC.liftIO $ initPackages fs'
			return result
