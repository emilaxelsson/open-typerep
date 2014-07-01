{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is only to limit the scope of the @OverlappingInstances@ flag

module Data.TypeRep.Sub where



import Data.Syntactic

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

instance SubUniverse t t
  where
    weakenUniverse = id

instance (SubUniverse sub sup', sup ~ (t :+: sup')) => SubUniverse sub sup
  where
    weakenUniverse = sugar . mapAST InjR . desugar . weakenUniverse

