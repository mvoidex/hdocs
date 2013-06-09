module Main (
	main
	) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network (PortNumber)

import HDocs.Module

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO

data HDocsOptions = HDocsOptions {
	optionJson :: Bool,
	optionGHC :: [String] }

instance Monoid HDocsOptions where
	mempty = HDocsOptions False []
	mappend l r = HDocsOptions (optionJson l || optionJson r) (optionGHC l ++ optionGHC r)

opts :: [OptDescr HDocsOptions]
opts = [
	Option ['j'] ["json"] (NoArg $ HDocsOptions True []) "output json",
	Option ['g'] ["--ghc"] (ReqArg (\s -> HDocsOptions False [s]) "GHC_OPT") "option to pass to GHC"]

main :: IO ()
main = do
	hSetEncoding stdout utf8

	(cfgs, cmds, _) <- fmap (getOpt Permute opts) getArgs

	let
		cfg = mconcat cfgs

		toStr :: ByteString -> String
		toStr = T.unpack . T.decodeUtf8 . toStrict

		showError :: Bool -> String -> String
		showError False err = "Error: " ++ err
		showError True err = toStr $ encode [
			T.pack "error" .= err]
		showResult :: Bool -> Map String String -> String
		showResult False rs = unlines $ concatMap (uncurry showResult') (M.toList rs) where
			showResult' nm d = [nm ++ ":", d, ""]
		showResult True rs = toStr $ encode rs

		loadDocs m f = do
			docs <- runDocsM (docs (optionGHC cfg) m)
			putStrLn $ either (showError isJson) (showResult isJson) (fmap (M.map formatDoc) docs >>= f)
			where
				isJson = optionJson cfg

	case cmds of
		["docs", m] -> loadDocs m return
		["docs", m, n] -> loadDocs m $ maybe (Left $ "Symbol '" ++ n ++ "' not found") (return . M.singleton n) . M.lookup n
		_ -> printUsage

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"Usage:",
	"  hdocs docs <module> - get docs for module/file",
	"  hdocs docs <module> <name> - get docs for name in module/file",
	"",
	usageInfo "flags" opts]
