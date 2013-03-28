module Main (
	main
	) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Maybe
import qualified Data.Map as M
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network (PortNumber)

import HDocs.Args
import HDocs.Module

import System.Environment

main :: IO ()
main = do
	(named, cmds) <- fmap namedArgs getArgs
	let
		ghcOpts = fromMaybe [] $ arg_ named "-g"
	case cmds of
		["docs", m] -> do
			flags <- configSession ghcOpts
			docs <- runDocsM $ fmap (M.map formatDoc) $ moduleDocs flags m
			json docs
		["docs", m, n] -> do
			flags <- configSession ghcOpts
			mdocs <- runDocsM $ symbolDocs flags m n
			maybe (return ()) putStrLn mdocs
		_ -> printUsage
	where
		toStr :: ByteString -> String
		toStr = T.unpack . T.decodeUtf8 . toStrict

		json :: ToJSON a => a -> IO ()
		json = putStrLn . toStr . encode

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"Usage:",
	"  hdocs docs <module> - get docs for module",
	"  hdocs docs <module> <name> - get docs for name in module",
	"",
	"  -g opts - options to pass to GHC"]
