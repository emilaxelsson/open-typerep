{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representations for tuple types
--
-- The reason for using symbol names ending with @_t@ is that 'deriveRender'
-- uses everything that comes before @_@ when rendering the constructor.

module Data.TypeRep.Types.Tuple where



import Data.List (intercalate)
import qualified Data.Typeable as Typeable

import Data.Orphans

import Language.Syntactic

import Data.TypeRep.Representation
import Data.TypeRep.TH



data TupleType a
  where
    Tup2_t  :: TupleType (a :-> b :-> Full (a,b))
    Tup3_t  :: TupleType (a :-> b :-> c :-> Full (a,b,c))
    Tup4_t  :: TupleType (a :-> b :-> c :-> d :-> Full (a,b,c,d))
    Tup5_t  :: TupleType (a :-> b :-> c :-> d :-> e :-> Full (a,b,c,d,e))
    Tup6_t  :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> Full (a,b,c,d,e,f))
    Tup7_t  :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> Full (a,b,c,d,e,f,g))
    Tup8_t  :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> Full (a,b,c,d,e,f,g,h))
    Tup9_t  :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> Full (a,b,c,d,e,f,g,h,i))
    Tup10_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> Full (a,b,c,d,e,f,g,h,i,j))
    Tup11_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> k :-> Full (a,b,c,d,e,f,g,h,i,j,k))
    Tup12_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> k :-> l :-> Full (a,b,c,d,e,f,g,h,i,j,k,l))
    Tup13_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> k :-> l :-> m :-> Full (a,b,c,d,e,f,g,h,i,j,k,l,m))
    Tup14_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> k :-> l :-> m :-> n :-> Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n))
    Tup15_t :: TupleType (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :-> k :-> l :-> m :-> n :-> o :-> Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))



-- The smart constructors are not overloaded using `Syntactic` as this would
-- lead to gigantic type signatures.

tup2Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t (a,b)
tup2Type = sugarSym Tup2_t

tup3Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t (a,b,c)
tup3Type = sugarSym Tup3_t

tup4Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t (a,b,c,d)
tup4Type = sugarSym Tup4_t

tup5Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t (a,b,c,d,e)
tup5Type = sugarSym Tup5_t

tup6Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f-> TypeRep t (a,b,c,d,e,f)
tup6Type = sugarSym Tup6_t

tup7Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t (a,b,c,d,e,f,g)
tup7Type = sugarSym Tup7_t

tup8Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t (a,b,c,d,e,f,g,h)
tup8Type = sugarSym Tup8_t

tup9Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t (a,b,c,d,e,f,g,h,i)
tup9Type = sugarSym Tup9_t

tup10Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t (a,b,c,d,e,f,g,h,i,j)
tup10Type = sugarSym Tup10_t

tup11Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t k -> TypeRep t (a,b,c,d,e,f,g,h,i,j,k)
tup11Type = sugarSym Tup11_t

tup12Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t k -> TypeRep t l -> TypeRep t (a,b,c,d,e,f,g,h,i,j,k,l)
tup12Type = sugarSym Tup12_t

tup13Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t k -> TypeRep t l -> TypeRep t m -> TypeRep t (a,b,c,d,e,f,g,h,i,j,k,l,m)
tup13Type = sugarSym Tup13_t

tup14Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t k -> TypeRep t l -> TypeRep t m -> TypeRep t n
    -> TypeRep t (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
tup14Type = sugarSym Tup14_t

tup15Type :: (TupleType :<: t)
    => TypeRep t a -> TypeRep t b -> TypeRep t c -> TypeRep t d -> TypeRep t e
    -> TypeRep t f -> TypeRep t g -> TypeRep t h -> TypeRep t i -> TypeRep t j
    -> TypeRep t k -> TypeRep t l -> TypeRep t m -> TypeRep t n
    -> TypeRep t o -> TypeRep t (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
tup15Type = sugarSym Tup15_t

tupWidth :: TupleType a -> Int
tupWidth Tup2_t  = 2
tupWidth Tup3_t  = 3
tupWidth Tup4_t  = 4
tupWidth Tup5_t  = 5
tupWidth Tup6_t  = 6
tupWidth Tup7_t  = 7
tupWidth Tup8_t  = 8
tupWidth Tup9_t  = 9
tupWidth Tup10_t = 10
tupWidth Tup11_t = 11
tupWidth Tup12_t = 12
tupWidth Tup13_t = 13
tupWidth Tup14_t = 14
tupWidth Tup15_t = 15

instance Render TupleType
  where
    renderSym tup   = "(" ++ replicate (tupWidth tup - 1) ',' ++ ")"
    renderArgs as _ = "(" ++ intercalate "," as ++ ")"

deriveTypeEq ''TupleType

deriveWitnessAny ''TupleType

deriveWitness ''Typeable.Typeable ''TupleType

deriveWitness ''Eq   ''TupleType
deriveWitness ''Ord  ''TupleType
deriveWitness ''Show ''TupleType

derivePWitnessAny ''TupleType

derivePWitness ''Typeable.Typeable ''TupleType

derivePWitness ''Eq   ''TupleType
derivePWitness ''Ord  ''TupleType
derivePWitness ''Show ''TupleType

instance PWitness Num TupleType t
instance PWitness Integral TupleType t

