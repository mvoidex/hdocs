module Main (
	main
	) where

import Control.Monad
import qualified Data.Map as M

import System.Exit

import HDocs.Module

-- | This is main function
main :: IO ()
main = do
	sdocs <- runDocsM (fileDocs [] "tests/Test.hs")
	tdocs <- either (\e -> putStrLn e >> exitFailure) (return . M.lookup "main") sdocs
	when (fmap formatDoc tdocs /= Just "This is main function") exitFailure
	edocs <- runDocsM (moduleDocs [] "Prelude")
	mdocs <- either (\e -> putStrLn e >> exitFailure) (return . M.lookup "null") edocs
	when (fmap formatDoc mdocs /= Just "Test whether a list is empty.") exitFailure
	exitSuccess
