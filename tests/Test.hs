module Main (
	main
	) where

import qualified Data.Map as M

import System.Exit

import HDocs.Module

main :: IO ()
main = do
	edocs <- runDocsM (moduleDocs [] "Prelude")
	mdocs <- either (\e -> putStrLn e >> exitFailure) (return . M.lookup "null") edocs
	if fmap formatDoc mdocs == Just "Test whether a list is empty." then exitSuccess else exitFailure
