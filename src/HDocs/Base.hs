{-# LANGUAGE CPP #-}

module HDocs.Base (
	ModuleDocMap,
	withInitializedPackages, configSession,
	formatDoc, formatDocs
	) where

import Data.Char (isSpace)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Foldable (foldMap, toList)
import qualified Data.Map as M

import Documentation.Haddock

import DynFlags
import GHC
import GHC.Paths
import qualified GhcMonad as GHC (liftIO)
import Name (occNameString)
import Packages

import HDocs.Ghc.Compat

-- | Documentation in module
type ModuleDocMap = Map String (Doc String)

-- | Run action with initialized packages
withInitializedPackages :: [String] -> (DynFlags -> IO a) -> IO a
withInitializedPackages ghcOpts cont = runGhc (Just libdir) $ do
	fs <- getSessionDynFlags
	cleanupHandler fs $ do
		(fs', _, _) <- parseDynamicFlags fs (map noLoc ghcOpts)
		_ <- setSessionDynFlags fs'
		(result, _) <- GHC.liftIO $ initPackages fs'
		GHC.liftIO $ cont result

-- | Config GHC session
configSession :: [String] -> IO DynFlags
configSession ghcOpts = runGhc (Just libdir) $ do
	fs <- getSessionDynFlags
	cleanupHandler fs $ do
		(fs', _, _) <- parseDynamicFlags fs (map noLoc ghcOpts)
		_ <- setSessionDynFlags fs'
		(result, _) <- GHC.liftIO $ initPackages fs'
		return result

-- | Format documentation to plain text.
formatDoc :: Doc String -> String
formatDoc = trim . go where
	go :: Doc String -> String
	go (DocAppend a b) = go a ++ go b
	go (DocString str) = trimSpaces str
	go (DocParagraph p) = go p ++ "\n"
	go (DocIdentifier i) = unwrap i
	go (DocIdentifierUnchecked m) = moduleNameString mname ++ "." ++ occNameString occname where
			(mname, occname) = unwrap m
	go (DocModule m) = m
	go (DocWarning w) = go w
	go (DocEmphasis e) = "*" ++ go e ++ "*"
	go (DocMonospaced e) = "`" ++ go e ++ "`"
	go (DocBold b) = "*" ++ go b ++ "*"
	go (DocUnorderedList i) = unlines (map (("* " ++) . go) i)
	go (DocOrderedList i) = unlines (zipWith (\i' x -> show i' ++ ". " ++ go x) ([1..] :: [Integer]) i)
	go (DocDefList xs) = unlines (map (\(i,x) -> go i ++ ". " ++ go x) xs)
	go (DocCodeBlock block) = unlines (map ("    " ++) (lines (go block))) ++ "\n"
	go (DocHyperlink (Hyperlink url label)) = maybe url (\l -> showLabel l ++ "[" ++ url ++ "]") $ label where
#if MIN_VERSION_haddock_library(1,8,0)
			showLabel = go
#else
			showLabel = id
#endif
	go (DocPic pic) = show pic
#if MIN_VERSION_haddock_library(1,4,0)
	go (DocMathInline m) = m
	go (DocMathDisplay m) = m
#endif
	go (DocAName name) = name
	go (DocProperty prop) = prop
	go (DocExamples exs) = unlines (map formatExample exs)
	go (DocHeader h) = foldMap go h
	go _ = ""

	formatExample :: Example -> String
	formatExample (Example expr result) = ">>> " ++ expr ++ "\n" ++ unlines result

	trimSpaces [] = []
	trimSpaces [s] = [s]
	trimSpaces (' ':' ':ss) = trimSpaces (' ':ss)
	trimSpaces (x:y:ss) = x : trimSpaces(y:ss)

	trim :: String -> String
	trim = p . p where
		p = reverse . dropWhile isSpace

#if MIN_VERSION_haddock_library(1,8,0)
	unwrap = fromMaybe (error "toList on `Wrap` returns empty list") . listToMaybe . toList
#else
	unwrap = id
#endif

-- | Format docs to plain text
formatDocs :: ModuleDocMap -> Map String String
formatDocs = M.map formatDoc
