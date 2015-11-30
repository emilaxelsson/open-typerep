{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Typeable' instances for tuple types. The reason for having these in a
-- separate module is that it might be desired to have these instances with
-- other type representations.
--
-- For example, instead of the instance
--
-- > (BoolType :<: t) => Typeable t Bool
--
-- one might want to have
--
-- > Typeable MyTypeRep Bool

module Data.TypeRep.Types.Tuple.Typeable where



import Language.Syntactic

import Data.TypeRep.Representation
import Data.TypeRep.TH
import Data.TypeRep.Types.Tuple



instance (TupleType :<: t, Typeable t a, Typeable t b) =>
    Typeable t (a,b)
  where typeRep' = sugarSym Tup2_t typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c) =>
    Typeable t (a,b,c)
  where typeRep' = sugarSym Tup3_t typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d) =>
    Typeable t (a,b,c,d)
  where typeRep' = sugarSym Tup4_t typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e) =>
    Typeable t (a,b,c,d,e)
  where typeRep' = sugarSym Tup5_t typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f) =>
    Typeable t (a,b,c,d,e,f)
  where typeRep' = sugarSym Tup6_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g) =>
    Typeable t (a,b,c,d,e,f,g)
  where typeRep' = sugarSym Tup7_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h) =>
    Typeable t (a,b,c,d,e,f,g,h)
  where typeRep' = sugarSym Tup8_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i) =>
    Typeable t (a,b,c,d,e,f,g,h,i)
  where typeRep' = sugarSym Tup9_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j)
  where typeRep' = sugarSym Tup10_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j, Typeable t k) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j,k)
  where typeRep' = sugarSym Tup11_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j, Typeable t k, Typeable t l) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j,k,l)
  where typeRep' = sugarSym Tup12_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j, Typeable t k, Typeable t l, Typeable t m) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j,k,l,m)
  where typeRep' = sugarSym Tup13_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j, Typeable t k, Typeable t l, Typeable t m, Typeable t n) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
  where typeRep' = sugarSym Tup14_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

instance (TupleType :<: t, Typeable t a, Typeable t b, Typeable t c, Typeable t d, Typeable t e, Typeable t f, Typeable t g, Typeable t h, Typeable t i, Typeable t j, Typeable t k, Typeable t l, Typeable t m, Typeable t n, Typeable t o) =>
    Typeable t (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
  where typeRep' = sugarSym Tup15_t typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep' typeRep'

deriveWitnessTypeable ''TupleType
derivePWitnessTypeable ''TupleType

