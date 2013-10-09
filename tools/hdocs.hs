module Main (
	main
	) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
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
	optionPretty :: Bool,
	optionGHC :: [String] }

instance Monoid HDocsOptions where
	mempty = HDocsOptions False []
	mappend l r = HDocsOptions (optionPretty l || optionPretty r) (optionGHC l ++ optionGHC r)

opts :: [OptDescr HDocsOptions]
opts = [
	Option [] ["pretty"] (NoArg $ HDocsOptions True []) "pretty JSON output",
	Option ['g'] ["ghc"] (ReqArg (\s -> HDocsOptions False [s]) "GHC_OPT") "option to pass to GHC"]

main :: IO ()
main = do
	hSetEncoding stdout utf8

	(cfgs, cmds, _) <- fmap (getOpt Permute opts) getArgs

	let
		cfg = mconcat cfgs
		isPretty = optionPretty cfg

		toStr :: ToJSON a => a -> String
		toStr = T.unpack . T.decodeUtf8 . toStrict . encode' where
			encode'
				| isPretty = encodePretty
				| otherwise = encode

		jsonError :: String -> Value
		jsonError err = object [
			T.pack "error" .= err]

		loadDocs m f = do
			docs <- runDocsM (docs (optionGHC cfg) m)
			putStrLn $ either (toStr . jsonError) toStr (fmap (M.map formatDoc) docs >>= f)

	case cmds of
		[m] -> loadDocs m return
		[m, n] -> loadDocs m $ maybe (Left $ "Symbol '" ++ n ++ "' not found") (return . M.singleton n) . M.lookup n
		_ -> printUsage

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"Usage:",
	"  hdocs <module> - get docs for module/file",
	"  hdocs <module> <name> - get docs for name in module/file",
	"",
	usageInfo "flags" opts]
