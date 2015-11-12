{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(a,b,c,d) 0
#endif
  -- MIN_VERSION_GLASGOW_HASKELL was introduced in GHC 7.10

#if MIN_VERSION_GLASGOW_HASKELL(7,10,0,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | This module is only to limit the scope of the @OverlappingInstances@ flag

module Data.TypeRep.Sub where

-- TODO Merge this module with `Data.TypeRep.Internal` when support for < 7.10 is dropped



import Language.Syntactic

import Data.TypeRep.Internal



-- | Sub-universe relation
--
-- In general, a universe @t@ is a sub-universe of @u@ if @u@ has the form
--
-- > t1 :+: t2 :+: ... :+: t
class SubUniverse sub sup
  where
    -- | Cast a type representation to a larger universe
    weakenUniverse :: TypeRep sub a -> TypeRep sup a


instance {-# OVERLAPPING #-} SubUniverse t t
  where
    weakenUniverse = id

instance {-# OVERLAPPING #-} (SubUniverse sub sup', sup ~ (t :+: sup')) => SubUniverse sub sup
  where
    weakenUniverse = sugar . mapAST InjR . desugar . weakenUniverse

