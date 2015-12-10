-- | Construction and elimination of tuples
--
-- Something similar can be achieved using the 'Syntactic' instances from
-- "Language.Syntactic.TypeRep.Sugar.TupleTR", e.g:
--
-- > sel1' :: forall sym t a b
-- >     .  ( Typeable t a
-- >        , Typeable t b
-- >        , Tuple     :<: sym
-- >        , TupleType :<: t
-- >        )
-- >     => ASTF (sym :&: TypeRep t) (a,b) -> ASTF (sym :&: TypeRep t) a
-- > sel1' ab = a
-- >   where
-- >     (a, _ :: ASTF (sym :&: TypeRep t) b) = sugar ab
--
-- But the point of this module is to do it without the 'Typeable' constraint.

module Language.Syntactic.TypeRep.TupleConversion where



import Language.Syntactic
import Language.Syntactic.Functional.Tuple

import Data.TypeRep.Representation
import Data.TypeRep.Types.Tuple
import Language.Syntactic.TypeRep.Sugar.TupleTR () -- For documentation



sel1 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel1 tup)
sel1 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb             | Just Tup2_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc       | Just Tup3_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td | Just Tup4_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a

sel2 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel2 tup)
sel2 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb             | Just Tup2_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc       | Just Tup3_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td | Just Tup4_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a

sel3 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel3 tup)
sel3 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc       | Just Tup3_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td | Just Tup4_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a

sel4 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel4 tup)
sel4 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td | Just Tup4_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a

tup2
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) (a,b)
tup2 a b =
    Sym (inj Tup2 :&: tup2Type (getDecor a) (getDecor b))
      :$ a :$ b

tup3
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c
    -> ASTF (sym :&: TypeRep t) (a,b,c)
tup3 a b c =
    Sym (inj Tup3 :&: tup3Type (getDecor a) (getDecor b) (getDecor c))
      :$ a :$ b :$ c

tup4
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) (a,b,c,d)
tup4 a b c d =
    Sym (inj Tup4 :&: tup4Type (getDecor a) (getDecor b) (getDecor c) (getDecor d))
      :$ a :$ b :$ c :$ d

