{-# LANGUAGE UndecidableInstances #-}

-- | 'Syntactic' instances for tuples and symbol domains decorated with
-- 'TypeRep'

module Language.Syntactic.TypeRep.Sugar.TupleTR where



import Language.Syntactic
import Language.Syntactic.Functional.Tuple

import Data.TypeRep
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.Tuple.Typeable ()
import Language.Syntactic.TypeRep



instance
    ( sym ~ (s :&: TypeRep t)
    , Syntactic a, Domain a ~ sym
    , Syntactic b, Domain b ~ sym
    , Tuple :<: s
    , Typeable t (Internal a)
    , Typeable t (Internal b)
    , TupleType :<: t
    ) =>
      Syntactic (a,b)
  where
    type Domain (a,b)   = Domain a
    type Internal (a,b) = (Internal a, Internal b)
    desugar (a,b) = sugarSymTR Tup2 a b
    sugar ab      = (sugarSymTR Sel1 ab, sugarSymTR Sel2 ab)

instance
    ( sym ~ (s :&: TypeRep t)
    , Syntactic a, Domain a ~ sym
    , Syntactic b, Domain b ~ sym
    , Syntactic c, Domain c ~ sym
    , Tuple :<: s
    , Typeable t (Internal a)
    , Typeable t (Internal b)
    , Typeable t (Internal c)
    , TupleType :<: t
    ) =>
      Syntactic (a,b,c)
  where
    type Domain (a,b,c)   = Domain a
    type Internal (a,b,c) = (Internal a, Internal b, Internal c)
    desugar (a,b,c) = sugarSymTR Tup3 a b c
    sugar abc       = (sugarSymTR Sel1 abc, sugarSymTR Sel2 abc, sugarSymTR Sel3 abc)

instance
    ( sym ~ (s :&: TypeRep t)
    , Syntactic a, Domain a ~ sym
    , Syntactic b, Domain b ~ sym
    , Syntactic c, Domain c ~ sym
    , Syntactic d, Domain d ~ sym
    , Tuple :<: s
    , Typeable t (Internal a)
    , Typeable t (Internal b)
    , Typeable t (Internal c)
    , Typeable t (Internal d)
    , TupleType :<: t
    ) =>
      Syntactic (a,b,c,d)
  where
    type Domain (a,b,c,d)   = Domain a
    type Internal (a,b,c,d) = (Internal a, Internal b, Internal c, Internal d)
    desugar (a,b,c,d) = sugarSymTR Tup4 a b c d
    sugar abcd        = (sugarSymTR Sel1 abcd, sugarSymTR Sel2 abcd, sugarSymTR Sel3 abcd, sugarSymTR Sel4 abcd)

