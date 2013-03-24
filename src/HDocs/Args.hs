module HDocs.Args (
	namedArgs,
	arg, arg_
	) where

import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Text.Read

namedArgs :: [String] -> (Map String [String], [String])
namedArgs [] = (M.empty, [])
namedArgs [s] = (M.empty, [s])
namedArgs (s:v:ss)
	| "-" `isPrefixOf` s = (M.singleton s [v], []) `append` namedArgs ss
	| otherwise = (M.empty, [s]) `append` namedArgs (v:ss)
	where
		append (lm, ls) (rm, rs) = (M.unionWith (++) lm rm, ls ++ rs)

arg :: Read a => Map String [String] -> String -> Maybe a
arg as name = M.lookup name as >>= listToMaybe >>= readMaybe

arg_ :: Map String [String] -> String -> Maybe [String]
arg_ as name = M.lookup name as
