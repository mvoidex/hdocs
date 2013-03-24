module Main (
	main
	) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network (PortNumber)

import HDocs.Args
import HDocs.Module
import HDocs.Daemon

import System.Environment

main :: IO ()
main = do
	(named, cmds) <- fmap namedArgs getArgs
	let
		port = maybe 4334 fromInteger $ arg named "-port"
		ghcOpts = fromMaybe [] $ arg_ named "-g"
	case cmds of
		["start"] -> do
			putStrLn "Starting daemon."
			startDaemon port ghcOpts
		["stop"] -> do
			putStrLn "Stopping daemon."
			sendMsg <- client port
			sendMsg $ Message Die ghcOpts print
		["daemon"] -> do
			(onMsg, waitDaemon) <- runDaemon
			server port onMsg
			waitDaemon
		["module", m] -> withClient port $ Message (ModuleCommand m) ghcOpts printResponse
		["name", n, m] -> withClient port $ Message (SymbolCommand n m) ghcOpts printResponse
		["status", "modules"] -> withClient port $ Message StateCommand ghcOpts printResponse

		_ -> printUsage
	where
		printResponse :: Response -> IO ()
		printResponse (ModuleResponse _ docs) = json docs
		printResponse (SymbolResponse _ mdocs) = maybe (return ()) putStrLn mdocs
		printResponse (StateResponse ms) = json $ object [
			(fromString "modules") .= ms]
		printResponse (ErrorResponse err) = json $ object [
			(fromString "error") .= err]

		withClient :: PortNumber -> Message -> IO ()
		withClient port msg = do
			sendMsg <- client port
			sendMsg msg

		toStr :: ByteString -> String
		toStr = T.unpack . T.decodeUtf8 . toStrict

		json :: ToJSON a => a -> IO ()
		json = putStrLn . toStr . encode

printUsage :: IO ()
printUsage = mapM_ putStrLn [
	"Usage:",
	"  hdocs start - start daemon",
	"  hdocs stop - stop daemon",
	"  hdocs module <module> - get docs for module",
	"  hdocs name <name> <module> - get docs for name in module",
	"  hdocs status modules - get list of all loaded modules",
	"",
	"  -port p - port, 4334 by default",
	"  -g opts - options to pass to GHC"]
