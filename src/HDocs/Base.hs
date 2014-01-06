module HDocs.Base (
	ModuleDocMap,
	withInitializedPackages, configSession,
	formatDoc, formatDocs
	) where

import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as M

import Documentation.Haddock

import DynFlags
import GHC
import GHC.Paths
import qualified GhcMonad as GHC (liftIO)
import Name (occNameString)
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
			(result, _) <- GHC.liftIO $ initPackages fs'
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

	trim :: String -> String
	trim = p . p where
		p = reverse . dropWhile isSpace

-- | Format docs to plain text
formatDocs :: ModuleDocMap -> Map String String
formatDocs = M.map formatDoc
