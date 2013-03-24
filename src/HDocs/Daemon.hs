module HDocs.Daemon (
	Command(..),
	Response(..),
	Message(..),
	-- * Daemon
	runDaemon,
	server,
	client,
	startDaemon
	) where

import Control.Exception hiding (catch)
import Control.Monad
import Control.Monad.CatchIO (catch, MonadCatchIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cont
import Control.Monad.Trans (lift)
import Control.Monad.State
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Network
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Read (readMaybe)

import HDocs.Module

import GHC (gcatch)
import Module (moduleNameString)

data Command =
	ModuleCommand String |
	SymbolCommand String String |
	StateCommand |
	Die
		deriving (Eq, Ord, Read, Show)

data Response =
	ModuleResponse String (Map String String) |
	SymbolResponse String (Maybe String) |
	StateResponse [String] |
	ErrorResponse String |
	Dead
		deriving (Eq, Ord, Read, Show)

data Message = Message {
	messageCommand :: Command,
	messageGhcOpts :: [String],
	messageResponse :: Response -> IO () }

runDaemon :: IO (Message -> IO (), IO ())
runDaemon = do
	qsem <- newQSem 0
	ch <- newChan
	msgs <- getChanContents ch
	void $ forkIO $ do
		let
			(cmds, ~(die:_)) = span ((/= Die) . messageCommand) msgs
		withInitializedPackages $ \d -> runDocsM $ foldM_ processMessage (d, []) cmds
		messageResponse die Dead
		signalQSem qsem
	return (writeChan ch, waitQSem qsem)
	where
		processMessage (d, oldGhcOpts) (Message cmd ghcOpts onResponse) = flip catch (onError' (d, oldGhcOpts)) $ do
			dflags <- if oldGhcOpts /= ghcOpts
				then liftIO $ do
					putStrLn $ "Reconfig GHC: " ++ show ghcOpts
					configSession ghcOpts
				else return d
			resp <-  catch (processCommand dflags cmd) onError
			lift $ onResponse resp
			return (dflags, ghcOpts)
		processCommand d (ModuleCommand mname) = liftM (ModuleResponse mname . M.map formatDoc) $ moduleDocs d mname
		processCommand d (SymbolCommand name mname) = liftM (SymbolResponse name) $ symbolDocs d name mname
		processCommand d StateCommand = liftM StateResponse $ gets (map moduleNameString . M.keys)

		onError :: MonadCatchIO m => SomeException -> m Response
		onError = return . ErrorResponse . show

		onError' :: MonadCatchIO m => a -> SomeException -> m a
		onError' v _ = return v


server :: PortNumber -> (Message -> IO ()) -> IO ()
server p onMsg = void $ forkIO $ bracket (listenOn (PortNumber p)) sClose $ \s -> forever $ do
	(h, host, port) <- accept s
	void $ forkIO $ do
		cts <- hGetContents h
		mapM_ (onCmd h) $ mapMaybe readMaybe $ lines cts
	where
		onCmd :: Handle -> (Command, [String]) -> IO ()
		onCmd h (cmd, ghcOpts) = onMsg $ Message cmd ghcOpts (hPutStrLn h . show)

client :: PortNumber -> IO (Message -> IO ())
client p = do
	h <- connectTo "" (PortNumber p)
	return $ \(Message cmd ghcOpts onResponse) -> do
		hPutStrLn h (show (cmd, ghcOpts))
		str <- hGetLine h
		maybe (return ()) onResponse $ readMaybe str

startDaemon :: PortNumber -> [String] -> IO ()
startDaemon p ghcOpts = do
	exePath <- getExecutablePath
	void $ createProcess $ (proc exePath $ ["daemon", "-port", show p] ++ opts) {
		close_fds = True }
	where
		opts = concatMap (\opt -> ["-g", opt]) ghcOpts
