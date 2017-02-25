module HDocs.Module (
	-- * Get module docs
	moduleDocsF, moduleDocs, installedDocsF, installedDocs,

	-- * Utility
	exportsDocs,

	module HDocs.Base
	) where

import Control.Applicative
import Control.Monad.Except

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Documentation.Haddock

import DynFlags
import Module
import Outputable (showSDoc, ppr)
import Packages
import Name

import HDocs.Base
import qualified HDocs.Ghc.Compat as Compat
import qualified HDocs.Haddock as H

-- | Load docs for all exported module symbols
moduleDocsF :: DynFlags -> String -> ExceptT String IO ModuleDocMap
moduleDocsF df mname = do
	pkg <- case pkgs of
		[] -> throwError $ "Module " ++ mname ++ " not found"
		[pkg] -> return pkg
		_ -> throwError $ "Module " ++ mname ++ " found in several packages: " ++ intercalate ", " (map (pkgId df) pkgs)
	ifaces <- H.readPackageInterfaces pkg
	iface <- maybe
		(throwError $ "Module " ++ mname ++ " not found in package " ++ pkgId df pkg)
		return
		(find ((== mname) . moduleNameString . moduleName . instMod) ifaces)

	depsfaces <- liftM concat $ mapM H.readPackageInterfaces $
		map (getPackageDetails df) $ ifacePackageDeps iface

	let
		deps = filter (ifaceDep iface) $ ifaces ++ depsfaces

	return $ snd $ exportsDocs (H.installedInterfacesDocs deps) iface
	where
		pkgs = filter exposed $ map snd $ lookupModuleInAllPackages df (mkModuleName mname)

		namePackage :: Name -> Compat.UnitId
		namePackage = Compat.moduleUnitId . nameModule

		ifacePackageDeps :: InstalledInterface -> [Compat.UnitId]
		ifacePackageDeps i = (Compat.moduleUnitId $ instMod i) `delete` (nub . map namePackage . instExports $ i)

		ifaceDep :: InstalledInterface -> InstalledInterface -> Bool
		ifaceDep i idep = instMod i /= instMod idep && instMod idep `elem` map nameModule (instExports i)

		pkgId :: DynFlags -> PackageConfig -> String
		pkgId d = showSDoc d . ppr . Compat.unitId

-- | Load docs for all exported module symbols
moduleDocs :: [String] -> String -> ExceptT String IO ModuleDocMap
moduleDocs opts mname = ExceptT $ withInitializedPackages opts (runExceptT . flip moduleDocsF mname) where

-- | Load docs for all installed modules
installedDocsF :: DynFlags -> ExceptT String IO (Map String ModuleDocMap)
installedDocsF df = do
	fs <- maybe (throwError "Package database empty") (return . concatMap haddockInterfaces) $ Compat.pkgDatabase df
	ifaces <- liftM concat $ mapM ((`mplus` return []) . H.readInstalledInterfaces) fs
	let
		idocs = H.installedInterfacesDocs ifaces
	return $ M.fromList $ map (exportsDocs idocs) ifaces

-- | Load docs for all installed modules
installedDocs :: [String] -> ExceptT String IO (Map String ModuleDocMap)
installedDocs opts = ExceptT $ withInitializedPackages opts (runExceptT . installedDocsF)

-- | Get docs for 'InstalledInterface' with its exports docs
exportsDocs :: Map String ModuleDocMap -> InstalledInterface -> (String, ModuleDocMap)
exportsDocs docs iface = (iname, snd (H.installedInterfaceDocs iface) `M.union` edocs) where
	iname = moduleNameString $ moduleName $ instMod iface
	edocs = M.fromList $ mapMaybe findDoc (instExports iface)
	findDoc n = ((,) (getOccString n)) <$> (H.lookupNameDoc n docs)
