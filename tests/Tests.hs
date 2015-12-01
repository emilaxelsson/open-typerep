{-# LANGUAGE CPP #-}

import qualified Simple
#if __GLASGOW_HASKELL__ >= 710
import qualified Custom
#endif



main = do
    Simple.main
#if __GLASGOW_HASKELL__ >= 710
    Custom.main
#endif

-- Importing both modules causes overlapping instances error on GHC < 7.10

