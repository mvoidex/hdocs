{-# LANGUAGE CPP #-}

module HDocs.Ghc.Compat (
	pkgDatabase, cleanupHandler, UnitId, unitId, moduleUnitId, getPackageDetails
	) where

import qualified GHC
import qualified Module
import qualified Packages as GHC

#if __GLASGOW_HASKELL__ == 710
import Exception (ExceptionMonad)
#endif

pkgDatabase :: GHC.DynFlags -> Maybe [GHC.PackageConfig]
#if __GLASGOW_HASKELL__ == 800
pkgDatabase = fmap (concatMap snd) . GHC.pkgDatabase
#elif __GLASGOW_HASKELL__ == 710
pkgDatabase = GHC.pkgDatabase
#endif

#if __GLASGOW_HASKELL__ == 800
cleanupHandler :: GHC.DynFlags -> m a -> m a
cleanupHandler _ = id
#elif __GLASGOW_HASKELL__ == 710
cleanupHandler :: (ExceptionMonad m) => GHC.DynFlags -> m a -> m a
cleanupHandler = GHC.defaultCleanupHandler
#endif

#if __GLASGOW_HASKELL__ == 800
type UnitId = Module.UnitId
#elif __GLASGOW_HASKELL__ == 710
type UnitId = Module.PackageKey
#endif

unitId :: GHC.PackageConfig -> UnitId
#if __GLASGOW_HASKELL__ == 800
unitId = GHC.unitId
#elif __GLASGOW_HASKELL__ == 710
unitId = GHC.packageKey
#endif

moduleUnitId :: Module.Module -> UnitId
#if __GLASGOW_HASKELL__ == 800
moduleUnitId = Module.moduleUnitId
#elif __GLASGOW_HASKELL__ == 710
moduleUnitId = Module.modulePackageKey
#endif

getPackageDetails :: GHC.DynFlags -> UnitId -> GHC.PackageConfig
getPackageDetails = GHC.getPackageDetails
