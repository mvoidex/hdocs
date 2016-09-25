module Main (
	main
	) where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map as M

import System.Exit

import HDocs.Module
import HDocs.Haddock

-- | This is main function
main :: IO ()
main = runExceptT main' >>= either (\e -> putStrLn e >> exitFailure) (const exitSuccess) where
	main' :: ExceptT String IO ()
	main' = do
		sdocs <- fmt $ liftM snd $ readSource [] "tests/HelpTest.hs"
		check "Documentation for file: Test.hs, test"
			(M.lookup "test" sdocs == Just "This is test function with documentation")
		-- edocs <- fmt $ moduleDocs [] "Prelude"
		-- check "Documentation for installed module: Prelude.null"
		-- 	(M.lookup "null" edocs `elem`
		-- 		[Just "Test whether a list is empty.",
		-- 		Just "Test whether the structure is empty. The default implementation is\n optimized for structures that are similar to cons-lists, because there\n is no general way to do better."])
		where
			check str p = unless p $ throwError str
			fmt = liftM formatDocs
