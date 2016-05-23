module HDocs.Haddock (
	-- * Documentation functions
	readInstalledDocs,
	readHaddock,
	readSources, readSources_, readSource, readSourcesGhc, readSourceGhc,

	-- * Extract docs
	installedInterfaceDocs, installedInterfacesDocs,
	interfaceDocs,	

	-- * Utility functions
	haddockFiles,
	readInstalledInterfaces, readPackageInterfaces,
	lookupDoc, lookupNameDoc,

	module HDocs.Base,

	Ghc,
	module Control.Monad.Except,
	withGhc
	) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)

import Distribution.Verbosity (silent)
import Documentation.Haddock
import Documentation.Haddock.Types (_doc)

import Exception (gtry)
import GHC (Ghc)
import DynFlags
import Module
import Name
import PackageConfig

import HDocs.Base

-- | Read all installed docs
readInstalledDocs :: [String] -> ExceptT String IO (Map String ModuleDocMap)
readInstalledDocs opts = do
	fs <- haddockFiles opts
	liftM M.unions $ forM fs $ \f -> (readHaddock f) `mplus` (return M.empty)

-- | Read docs from .haddock file
readHaddock :: FilePath -> ExceptT String IO (Map String ModuleDocMap)
readHaddock f = M.fromList . map installedInterfaceDocs <$> readInstalledInterfaces f

-- | Read docs for haskell modules
readSources :: [String] -> [FilePath] -> ExceptT String IO (Map String ModuleDocMap)
readSources opts = liftM M.fromList . readSources_ opts

-- | Read docs for haskell modules
readSources_ :: [String] -> [FilePath] -> ExceptT String IO [(String, ModuleDocMap)]
readSources_ opts fs = do
	ifaces <- liftError $ liftIO $ createInterfaces ([Flag_Verbosity "0", Flag_NoWarnings, Flag_UseUnicode] ++ map Flag_OptGhc opts) fs
	return $ map interfaceDocs ifaces

-- | Read docs for haskell module
readSource :: [String] -> FilePath -> ExceptT String IO (String, ModuleDocMap)
readSource opts f = liftM listToMaybe (readSources_ opts [f]) >>= maybe (throwError $ "Failed to load docs for " ++ f) return

-- | Read docs for source in Ghc monad
readSourcesGhc :: [String] -> [FilePath] -> ExceptT String Ghc [(String, ModuleDocMap)]
readSourcesGhc opts fs = ExceptT $ liftM (left (show :: SomeException -> String)) $ gtry $ do
	ifaces <- liftM fst $ processModules silent fs ([Flag_Verbosity "0", Flag_NoWarnings, Flag_UseUnicode] ++ map Flag_OptGhc opts) []
	return $ map interfaceDocs ifaces

-- | Read docs for haskell module
readSourceGhc :: [String] -> FilePath -> ExceptT String Ghc (String, ModuleDocMap)
readSourceGhc opts f = liftM listToMaybe (readSourcesGhc opts [f]) >>= maybe (throwError $ "Failed to load docs for " ++ f) return

-- | Get docs for 'InstalledInterface'
installedInterfaceDocs :: InstalledInterface -> (String, ModuleDocMap)
installedInterfaceDocs = stringize . (instMod &&& (fmap _doc . instDocMap))

-- | Get docs for 'InstalledInterface's
installedInterfacesDocs :: [InstalledInterface] -> Map String ModuleDocMap
installedInterfacesDocs = M.fromList . map installedInterfaceDocs

-- | Get docs for 'Interface'
interfaceDocs :: Interface -> (String, ModuleDocMap)
interfaceDocs = stringize . (ifaceMod &&& (fmap _doc . ifaceDocMap))

-- | Get list of haddock files in package db
haddockFiles :: [String] -> ExceptT String IO [FilePath]
haddockFiles opts = ExceptT $ withInitializedPackages opts $ return . maybe
	(Left "Package database empty")
	(Right . concatMap haddockInterfaces . concatMap snd) .
	pkgDatabase

-- | Read installed interface
readInstalledInterfaces :: FilePath -> ExceptT String IO [InstalledInterface]
readInstalledInterfaces f = do
	ifile <- liftError $ ExceptT $ readInterfaceFile freshNameCache f
	return $ ifInstalledIfaces ifile

-- | Read installed interfaces for package
readPackageInterfaces :: PackageConfig -> ExceptT String IO [InstalledInterface]
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

liftError :: ExceptT String IO a -> ExceptT String IO a
liftError = ExceptT . handle onErr . runExceptT where
	onErr :: SomeException -> IO (Either String a)
	onErr = return . Left . show
