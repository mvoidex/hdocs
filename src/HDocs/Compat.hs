{-# LANGUAGE CPP #-}

module HDocs.Compat (
	readInterfaceFile,
	mapDoc
	) where

import Control.Monad.IO.Class
import Documentation.Haddock hiding (readInterfaceFile)
import qualified Documentation.Haddock as Haddock

readInterfaceFile :: MonadIO m => NameCacheAccessor m -> FilePath -> m (Either String InterfaceFile)
#if MIN_VERSION_haddock_api(2,23,0)
readInterfaceFile cache f = Haddock.readInterfaceFile cache f False
#else
readInterfaceFile = Haddock.readInterfaceFile
#endif

mapDoc :: (a -> b) -> Doc a -> Doc b
#if MIN_VERSION_haddock_api(2,23,0)
mapDoc = fmap . fmap
#else
mapDoc = fmap
#endif
