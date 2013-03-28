module HDocs.Module (
	DocMap,
	DocsM,
	runDocsM,
	-- * Get module docs
	withInitializedPackages,
	configSession,
	symbolDocs,
	moduleDocs,
	moduleInterface,
	packageInterface,
	-- * Helpers
	formatDoc
	) where

import Control.Arrow
import Control.Monad.State

import Data.Either
import Data.Char (isSpace)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Documentation.Haddock

import DynFlags
import GHC
import GHC.Paths (libdir)
import Module
import Name (getOccString, occNameString, Name)
import PackageConfig (PackageConfig, pkgUrl)
import Packages

-- | Documentations in module
type DocMap = Map String (Doc String)

withInitializedPackages :: (DynFlags -> IO a) -> IO a
withInitializedPackages cont = do
	flags <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
		flags <- getSessionDynFlags
		setSessionDynFlags flags
		return flags
	(flags', ps) <- initPackages flags
	cont flags'

configSession :: [String] -> IO DynFlags
configSession ghcOpts = do
	f <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
		flags <- getSessionDynFlags
		(flags', _, _) <- parseDynamicFlags flags (map noLoc ghcOpts)
		setSessionDynFlags flags'
		return flags'
	(result, ps) <- initPackages f
	return result

-- | Docs state
type DocsM a = StateT (Map ModuleName DocMap) IO a

-- | Run docs monad
runDocsM :: DocsM a -> IO a
runDocsM act = evalStateT act M.empty

-- | Load symbol documentation
symbolDocs :: DynFlags -> String -> String -> DocsM (Maybe String)
symbolDocs d m n = do
	docs <- moduleDocs d m
	return $ fmap formatDoc $ M.lookup n docs

-- | Load module documentation
moduleDocs ::DynFlags -> String -> DocsM DocMap
moduleDocs d m = do
	loaded <- gets (M.lookup mname)
	case loaded of
		Just d -> return d
		Nothing -> do
			ifaces <- liftM (map snd) $ lift $ moduleInterface d mname
			if null ifaces
				then do
					liftIO $ putStrLn $ "Unknown module " ++ m
					return M.empty
				else do
					modify $ M.insert mname $ M.unions $ map interfaceNameMap ifaces
					let
						reexports = filter ((/= mname) . moduleName . nameModule) $ concatMap instExports ifaces
					docs' <- liftM M.unions $ forM reexports $ \nm -> do
						doc <- liftM (M.lookup (getOccString nm)) $ moduleDocs d (moduleNameString $ moduleName $ nameModule nm)
						return $ maybe M.empty (M.singleton (getOccString nm)) doc
					modify $ M.update (Just . M.union docs') mname
					result <- gets $ M.lookup mname
					maybe (return M.empty) return result
	where
		mname = mkModuleName m

moduleInterface :: DynFlags -> ModuleName -> IO [(PackageConfig, InstalledInterface)]
moduleInterface d mname = do
	result <- liftIO $ getPackagesByModule d mname
	ps <- return $ either (const []) id result
	liftM concat $ mapM packageInterface' ps
	where
		packageInterface' p = liftM (zip (repeat p)) $ packageInterface d mname p

packageInterface :: DynFlags -> ModuleName -> PackageConfig -> IO [InstalledInterface]
packageInterface d mname package = do
	files <- getHaddockInterfaceByPackage package
	case partitionEithers files of
		([], []) -> return []
		(errs@(_:_), _) -> return []
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
	go (DocOrderedList i) = unlines (zipWith (\i x -> show i ++ ". " ++ go x) [1..] i)
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
interfaceNameMap :: InstalledInterface -> Map String (Doc String)
interfaceNameMap = M.fromList . map (getOccString *** fmap getOccString) . M.toList . instDocMap

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

