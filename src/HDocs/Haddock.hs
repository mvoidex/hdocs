module HDocs.Haddock (
	readInstalledDocs,
	haddockFiles,
	readDocs,
	installedInterfaceDocs
	) where

import Control.Arrow
import Control.Exception
import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as M

import Documentation.Haddock

import DynFlags
import Module
import Name
import PackageConfig

import HDocs.Base

-- | Read all installed docs
readInstalledDocs :: [String] -> ErrorT String IO (Map String ModuleDocMap)
readInstalledDocs opts = do
	fs <- haddockFiles opts
	liftM M.unions $ forM fs $ \f -> liftErrs (readDocs f) `mplus` (return M.empty)
	where
		liftErrs :: ErrorT String IO a -> ErrorT String IO a
		liftErrs act = ErrorT $ handle onErr (runErrorT act)

		onErr :: SomeException -> IO (Either String a)
		onErr = return . Left . show

-- | Get list of haddock files in package db
haddockFiles :: [String] -> ErrorT String IO [FilePath]
haddockFiles opts = ErrorT $ withInitializedPackages opts $ return . maybe
	(Left "Package database empty")
	(Right . concatMap haddockInterfaces) .
	pkgDatabase

-- | Read docs from .haddock file
readDocs :: FilePath -> ErrorT String IO (Map String ModuleDocMap)
readDocs f = do
	ifile <- ErrorT $ readInterfaceFile freshNameCache f
	return $ M.fromList $ map installedInterfaceDocs $ ifInstalledIfaces ifile

-- | Get docs from 'InstalledInterface'
installedInterfaceDocs :: InstalledInterface -> (String, ModuleDocMap)
installedInterfaceDocs = moduleNameString . moduleName . instMod &&& strDoc . instDocMap where
	strDoc = M.mapKeys getOccString . M.map (fmap getOccString)
