module HDocs.Haddock (
	-- * Documentation functions
	readInstalledDocs,
	readHaddock,
	readSource,

	-- * Extract docs
	installedInterfaceDocs, installedInterfacesDocs,
	interfaceDocs,	

	-- * Utility functions
	haddockFiles,
	readInstalledInterfaces, readPackageInterfaces,
	lookupDoc, lookupNameDoc,

	module HDocs.Base
	) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)

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
	liftM M.unions $ forM fs $ \f -> (readHaddock f) `mplus` (return M.empty)

-- | Read docs from .haddock file
readHaddock :: FilePath -> ErrorT String IO (Map String ModuleDocMap)
readHaddock f = M.fromList . map installedInterfaceDocs <$> readInstalledInterfaces f

-- | Read docs for haskell module
readSource :: [String] -> FilePath -> ErrorT String IO (String, ModuleDocMap)
readSource opts f = do
	ifaces <- liftIO $ createInterfaces ([Flag_Verbosity "0", Flag_NoWarnings] ++ map Flag_OptGhc opts) [f]
	iface <- maybe (throwError $ "Failed to load docs for " ++ f) return $ listToMaybe ifaces
	return $ interfaceDocs iface

-- | Get docs for 'InstalledInterface'
installedInterfaceDocs :: InstalledInterface -> (String, ModuleDocMap)
installedInterfaceDocs = stringize . (instMod &&& instDocMap)

-- | Get docs for 'InstalledInterface's
installedInterfacesDocs :: [InstalledInterface] -> Map String ModuleDocMap
installedInterfacesDocs = M.fromList . map installedInterfaceDocs

-- | Get docs for 'Interface'
interfaceDocs :: Interface -> (String, ModuleDocMap)
interfaceDocs = stringize . (ifaceMod &&& ifaceDocMap)

-- | Get list of haddock files in package db
haddockFiles :: [String] -> ErrorT String IO [FilePath]
haddockFiles opts = ErrorT $ withInitializedPackages opts $ return . maybe
	(Left "Package database empty")
	(Right . concatMap haddockInterfaces) .
	pkgDatabase

-- | Read installed interface
readInstalledInterfaces :: FilePath -> ErrorT String IO [InstalledInterface]
readInstalledInterfaces f = do
	ifile <- liftError $ ErrorT $ readInterfaceFile freshNameCache f
	return $ ifInstalledIfaces ifile

-- | Read installed interfaces for package
readPackageInterfaces :: PackageConfig -> ErrorT String IO [InstalledInterface]
readPackageInterfaces = liftM concat . mapM readInstalledInterfaces . haddockInterfaces

-- | Lookup doc
lookupDoc :: String -> String -> Map String ModuleDocMap -> Maybe (Doc String)
lookupDoc m n = M.lookup m >=> M.lookup n

-- | Lookup doc for Name
lookupNameDoc :: Name -> Map String ModuleDocMap -> Maybe (Doc String)
lookupNameDoc n = lookupDoc (moduleNameString $ moduleName $ nameModule n) (getOccString n)

stringize :: (Module, Map Name (Doc Name)) -> (String, ModuleDocMap)
stringize = moduleNameString . moduleName *** strDoc where
	strDoc = M.mapKeys getOccString . M.map (fmap getOccString)

liftError :: ErrorT String IO a -> ErrorT String IO a
liftError = ErrorT . handle onErr . runErrorT where
	onErr :: SomeException -> IO (Either String a)
	onErr = return . Left . show
