module Main (
	main
	) where

import Control.Monad
import Control.Monad.Error
import qualified Data.Map as M

import System.Exit

import HDocs.Module
import HDocs.Haddock

-- | This is main function
main :: IO ()
main = runErrorT main' >>= either (\e -> putStrLn e >> exitFailure) (\_ -> exitSuccess) where
	main' :: ErrorT String IO ()
	main' = do
		sdocs <- fmt $ liftM snd $ readSource [] "tests/data/Test.hs"
		check "Documentation for file: Test.hs, test"
			(M.lookup "test" sdocs == Just "This is test function with documentation")
		edocs <- fmt $ moduleDocs [] "Prelude"
		check "Documentation for installed module: Prelude.null"
			(M.lookup "null" edocs == Just "Test whether a list is empty.")
		where
			check str p = if p then return () else throwError str
			fmt = liftM formatDocs
