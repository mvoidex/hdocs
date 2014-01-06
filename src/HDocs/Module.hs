module HDocs.Module (
	-- * Get module docs
	moduleDocs, installedDocs,

	-- * Utility
	exportsDocs,

	module HDocs.Base
	) where

import Control.Applicative
import Control.Monad.Error

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Documentation.Haddock

import DynFlags
import Module
import Packages
import Name

import HDocs.Base
import qualified HDocs.Haddock as H

-- | Load docs for all exported module symbols
moduleDocs :: [String] -> String -> ErrorT String IO ModuleDocMap
moduleDocs opts mname = ErrorT $ withInitializedPackages opts (runErrorT . moduleDocs') where
	moduleDocs' :: DynFlags -> ErrorT String IO ModuleDocMap
	moduleDocs' d = do
		pkg <- case pkgs of
			[] -> throwError $ "Module " ++ mname ++ " not found"
			[pkg] -> return pkg
			_ -> throwError $ "Module " ++ mname ++ " found in several packages: " ++ intercalate ", " (map pkgId pkgs)
		ifaces <- H.readPackageInterfaces pkg
		iface <- maybe
			(throwError $ "Module " ++ mname ++ " not found in package " ++ pkgId pkg)
			return
			(find ((== mname) . moduleNameString . moduleName . instMod) ifaces)

		depsfaces <- liftM concat $ mapM H.readPackageInterfaces $
			map (getPackageDetails (pkgState d)) $ ifacePackageDeps iface

		let
			deps = filter (ifaceDep iface) $ ifaces ++ depsfaces

		return $ snd $ exportsDocs (H.installedInterfacesDocs deps) iface
		where
			pkgs = filter exposed $ map fst $ lookupModuleInAllPackages d (mkModuleName mname)

	namePackage :: Name -> PackageId
	namePackage = modulePackageId . nameModule

	ifacePackageDeps :: InstalledInterface -> [PackageId]
	ifacePackageDeps i = (modulePackageId $ instMod i) `delete` (nub . map namePackage . instExports $ i)

	ifaceDep :: InstalledInterface -> InstalledInterface -> Bool
	ifaceDep i idep = instMod i /= instMod idep && instMod idep `elem` map nameModule (instExports i)

	pkgId :: PackageConfig -> String
	pkgId = display . installedPackageId

-- | Load docs for all installed modules
installedDocs :: [String] -> ErrorT String IO (Map String ModuleDocMap)
installedDocs opts = ErrorT $ withInitializedPackages opts (runErrorT . installedDocs') where
	installedDocs' :: DynFlags -> ErrorT String IO (Map String ModuleDocMap)
	installedDocs' d = do
		fs <- maybe (throwError "Package database empty") (return . concatMap haddockInterfaces) $ pkgDatabase d
		ifaces <- liftM concat $ mapM ((`mplus` return []) . H.readInstalledInterfaces) fs
		let
			idocs = H.installedInterfacesDocs ifaces
		return $ M.fromList $ map (exportsDocs idocs) ifaces

-- | Get docs for 'InstalledInterface' with its exports docs
exportsDocs :: Map String ModuleDocMap -> InstalledInterface -> (String, ModuleDocMap)
exportsDocs docs iface = (iname, snd (H.installedInterfaceDocs iface) `M.union` edocs) where
	iname = moduleNameString $ moduleName $ instMod iface
	edocs = M.fromList $ mapMaybe findDoc (instExports iface)
	findDoc n = ((,) (getOccString n)) <$> (H.lookupNameDoc n docs)
