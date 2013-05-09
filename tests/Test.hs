module Main (
	main
	) where

import System.Exit

import HDocs.Module

main :: IO ()
main = do
	flags <- configSession []
	mdocs <- runDocsM $ symbolDocs flags "Prelude" "null"
	if mdocs == Just "Test whether a list is empty." then exitSuccess else exitFailure
