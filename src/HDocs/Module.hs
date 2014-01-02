module HDocs.Module (
	-- * Types
	DocsM,
	runDocsM,

	-- * Helpers
	moduleInterface,
	packageInterface,
	formatDoc,

	-- * Get module docs
	moduleDocs,
	fileDocs,
	docs,

	module HDocs.Base
	) where

import Control.Arrow
import Control.Exception
import Control.Monad.State
import Control.Monad.Error

import Data.Either
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as M

import Documentation.Haddock

import System.FilePath (takeExtension)

import DynFlags
import GHC
import GHC.Paths (libdir)
import qualified GhcMonad as GHC (liftIO)
import Module
import Name (getOccString, occNameString)
import Packages

import HDocs.Base

-- | Docs state
type DocsM a = ErrorT String (StateT (Map String ModuleDocMap) IO) a

-- | Run docs monad
runDocsM :: DocsM a -> IO (Either String a)
runDocsM act = catch (evalStateT (runErrorT act) M.empty) onError where
	onError :: SomeException -> IO (Either String a)
	onError = return . Left . show

class DocInterface a where
	getModule :: a -> Module
	getDocMap :: a -> DocMap Name
	getExports :: a -> [Name]
	loadInterfaces :: [String] -> String -> IO [a]

instance DocInterface InstalledInterface where
	getModule = instMod
	getDocMap = instDocMap
	getExports = instExports
	loadInterfaces opts m = withInitializedPackages opts $ \d ->
		liftM (map snd) $ moduleInterface d (mkModuleName m)

instance DocInterface Interface where
	getModule = ifaceMod
	getDocMap = ifaceDocMap
	getExports = const [] -- ifaceExports
	loadInterfaces opts m = do
		createInterfaces (noOutput ++ map Flag_OptGhc opts) [m]
		where
			noOutput = [
				Flag_Verbosity "0",
				Flag_NoWarnings]

-- | Load docs from interface
interfaceDocs :: DocInterface a => a -> [String] -> String -> DocsM ModuleDocMap
interfaceDocs h opts m = do
	loaded <- gets (M.lookup m)
	case loaded of
		Just d' -> return d'
		Nothing -> do
			ifaces <- liftM (`asTypeOf` [h]) $ liftIO $ loadInterfaces opts m
			when (null ifaces) $ throwError $ "Unable to load interface for module: " ++ m
			modify $ M.insert m $ M.unions $ map interfaceNameMap ifaces
			let
				reexports = filter ((/= m) . moduleNameString . moduleName . nameModule) $ concatMap getExports ifaces
			docs' <- liftM M.unions $ forM reexports $ \nm -> do
				doc <- liftM (M.lookup (getOccString nm)) $ interfaceDocs h opts (moduleNameString . moduleName . nameModule $ nm)
				return $ maybe M.empty (M.singleton (getOccString nm)) doc
			modify $ M.update (Just . M.union docs') m
			result <- gets $ M.lookup m
			maybe (throwError $ "Error loading module " ++ m) return result

-- | Load module documentation
moduleDocs :: [String] -> String -> DocsM ModuleDocMap
moduleDocs = interfaceDocs (undefined :: InstalledInterface)

-- | Load file documentation
fileDocs :: [String] -> FilePath -> DocsM ModuleDocMap
fileDocs = interfaceDocs (undefined :: Interface)

-- | Load docs for file or module
docs :: [String] -> String -> DocsM ModuleDocMap
docs opts m
	| takeExtension m `elem` [".hs", ".lhs"] = fileDocs opts m
	| otherwise = moduleDocs opts m

-- | Load installed interface
moduleInterface :: DynFlags -> ModuleName -> IO [(PackageConfig, InstalledInterface)]
moduleInterface d mname = do
	result <- liftIO $ getPackagesByModule d mname
	liftM concat $ mapM packageInterface' $ either (const []) id result
	where
		packageInterface' p = liftM (zip (repeat p)) $ packageInterface d mname p

packageInterface :: DynFlags -> ModuleName -> PackageConfig -> IO [InstalledInterface]
packageInterface _ mname package = do
	files <- getHaddockInterfaceByPackage package
	case partitionEithers files of
		([], []) -> return []
		(_:_, _) -> return []
		(_, files') -> return $ concatMap (filter ((== mname) . moduleName . instMod) . ifInstalledIfaces) files'

-- | Format documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . go where
	go DocEmpty = ""
	go (DocAppend a b) = go a ++ go b
	go (DocString str) = trimSpaces str
	go (DocParagraph p) = go p ++ "\n"
	go (DocIdentifier i) = i
	go (DocIdentifierUnchecked (mname, occname)) = moduleNameString mname ++ "." ++ occNameString occname
	go (DocModule m) = m
	go (DocEmphasis e) = "*" ++ go e ++ "*"
	go (DocMonospaced e) = "`" ++ go e ++ "`"
	go (DocUnorderedList i) = unlines (map (("* " ++) . go) i)
	go (DocOrderedList i) = unlines (zipWith (\i' x -> show i' ++ ". " ++ go x) ([1..] :: [Integer]) i)
	go (DocDefList xs) = unlines (map (\(i,x) -> go i ++ ". " ++ go x) xs)
	go (DocCodeBlock block) = unlines (map ("    " ++) (lines (go block))) ++ "\n"
	go (DocHyperlink (Hyperlink url label)) = maybe url (\l -> l ++ "[" ++ url ++ "]") label
	go (DocPic pic) = pic
	go (DocAName name) = name
	go (DocExamples exs) = unlines (map formatExample exs)

	formatExample :: Example -> String
	formatExample (Example expr result) = "    > " ++ expr ++ unlines (map ("    " ++) result)

	trimSpaces [] = []
	trimSpaces [s] = [s]
	trimSpaces (' ':' ':ss) = trimSpaces (' ':ss)
	trimSpaces (x:y:ss) = x : trimSpaces(y:ss)

-- | Get a map from names to doc string
interfaceNameMap :: DocInterface a => a -> Map String (Doc String)
interfaceNameMap = M.fromList . map (getOccString *** fmap getOccString) . M.toList . getDocMap

-- | Search for a module's package with suggestions if not found
getPackagesByModule :: DynFlags -> ModuleName -> IO (Either [Module] [PackageConfig])
getPackagesByModule d m = return $ fmap (map fst) $ lookupModuleWithSuggestions d m

-- | Trim string
trim :: String -> String
trim = p . p where
	p = reverse . dropWhile isSpace

-- | Show the package name
showPackageName :: PackageIdentifier -> String
showPackageName = packageIdString . mkPackageId

-- | Get the Haddock interfaces of the given package.
getHaddockInterfaceByPackage :: PackageConfig -> IO [Either String InterfaceFile]
getHaddockInterfaceByPackage = mapM (readInterfaceFile freshNameCache) . haddockInterfaces

