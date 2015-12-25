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
    tup :$ ta :$ tb                                                                               | Just Tup2_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc                                                                         | Just Tup3_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td                                                                   | Just Tup4_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te                                                             | Just Tup5_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel1 :&: TypeRep ta) :$ a

sel2 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel2 tup)
sel2 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb                                                                               | Just Tup2_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc                                                                         | Just Tup3_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td                                                                   | Just Tup4_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te                                                             | Just Tup5_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel2 :&: TypeRep tb) :$ a

sel3 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel3 tup)
sel3 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc                                                                         | Just Tup3_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td                                                                   | Just Tup4_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te                                                             | Just Tup5_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel3 :&: TypeRep tc) :$ a

sel4 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel4 tup)
sel4 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td                                                                   | Just Tup4_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te                                                             | Just Tup5_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel4 :&: TypeRep td) :$ a

sel5 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel5 tup)
sel5 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te                                                             | Just Tup5_t  <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel5 :&: TypeRep te) :$ a

sel6 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel6 tup)
sel6 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf                                                       | Just Tup6_t  <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel6 :&: TypeRep tf) :$ a

sel7 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel7 tup)
sel7 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg                                                 | Just Tup7_t  <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel7 :&: TypeRep tg) :$ a

sel8 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel8 tup)
sel8 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th                                           | Just Tup8_t  <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel8 :&: TypeRep th) :$ a

sel9 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel9 tup)
sel9 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti                                     | Just Tup9_t  <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel9 :&: TypeRep ti) :$ a

sel10 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel10 tup)
sel10 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj                               | Just Tup10_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel10 :&: TypeRep tj) :$ a

sel11 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel11 tup)
sel11 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk                         | Just Tup11_t <- prj tup -> Sym (inj Sel11 :&: TypeRep tk) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel11 :&: TypeRep tk) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel11 :&: TypeRep tk) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel11 :&: TypeRep tk) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel11 :&: TypeRep tk) :$ a

sel12 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel12 tup)
sel12 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl                   | Just Tup12_t <- prj tup -> Sym (inj Sel12 :&: TypeRep tl) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel12 :&: TypeRep tl) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel12 :&: TypeRep tl) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel12 :&: TypeRep tl) :$ a

sel13 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel13 tup)
sel13 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm             | Just Tup13_t <- prj tup -> Sym (inj Sel13 :&: TypeRep tm) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel13 :&: TypeRep tm) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel13 :&: TypeRep tm) :$ a

sel14 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel14 tup)
sel14 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn       | Just Tup14_t <- prj tup -> Sym (inj Sel14 :&: TypeRep tn) :$ a
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel14 :&: TypeRep tn) :$ a

sel15 :: (Tuple :<: sym, TupleType :<: t) =>
    ASTF (sym :&: TypeRep t) tup -> ASTF (sym :&: TypeRep t) (Sel15 tup)
sel15 a = case unTypeRep $ getDecor a of
    tup :$ ta :$ tb :$ tc :$ td :$ te :$ tf :$ tg :$ th :$ ti :$ tj :$ tk :$ tl :$ tm :$ tn :$ to | Just Tup15_t <- prj tup -> Sym (inj Sel15 :&: TypeRep to) :$ a

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

tup5
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e)
tup5 a b c d e =
    Sym (inj Tup5 :&: tup5Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e))
      :$ a :$ b :$ c :$ d :$ e

tup6
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f)
tup6 a b c d e f =
    Sym (inj Tup6 :&: tup6Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f))
      :$ a :$ b :$ c :$ d :$ e :$ f

tup7
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g)
tup7 a b c d e f g =
    Sym (inj Tup7 :&: tup7Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g

tup8
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h)
tup8 a b c d e f g h =
    Sym (inj Tup8 :&: tup8Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h

tup9
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i)
tup9 a b c d e f g h i =
    Sym (inj Tup9 :&: tup9Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i

tup10
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j)
tup10 a b c d e f g h i j =
    Sym (inj Tup10 :&: tup10Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j

tup11
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) k
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j,k)
tup11 a b c d e f g h i j k =
    Sym (inj Tup11 :&: tup11Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j) (getDecor k))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j :$ k

tup12
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) k -> ASTF (sym :&: TypeRep t) l
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j,k,l)
tup12 a b c d e f g h i j k l =
    Sym (inj Tup12 :&: tup12Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j) (getDecor k) (getDecor l))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j :$ k :$ l

tup13
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) k -> ASTF (sym :&: TypeRep t) l
    -> ASTF (sym :&: TypeRep t) m
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j,k,l,m)
tup13 a b c d e f g h i j k l m =
    Sym (inj Tup13 :&: tup13Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j) (getDecor k) (getDecor l) (getDecor m))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j :$ k :$ l :$ m

tup14
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) k -> ASTF (sym :&: TypeRep t) l
    -> ASTF (sym :&: TypeRep t) m -> ASTF (sym :&: TypeRep t) n
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
tup14 a b c d e f g h i j k l m n =
    Sym (inj Tup14 :&: tup14Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j) (getDecor k) (getDecor l) (getDecor m) (getDecor n))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j :$ k :$ l :$ m :$ n

tup15
    :: (Tuple :<: sym, TupleType :<: t)
    => ASTF (sym :&: TypeRep t) a -> ASTF (sym :&: TypeRep t) b
    -> ASTF (sym :&: TypeRep t) c -> ASTF (sym :&: TypeRep t) d
    -> ASTF (sym :&: TypeRep t) e -> ASTF (sym :&: TypeRep t) f
    -> ASTF (sym :&: TypeRep t) g -> ASTF (sym :&: TypeRep t) h
    -> ASTF (sym :&: TypeRep t) i -> ASTF (sym :&: TypeRep t) j
    -> ASTF (sym :&: TypeRep t) k -> ASTF (sym :&: TypeRep t) l
    -> ASTF (sym :&: TypeRep t) m -> ASTF (sym :&: TypeRep t) n
    -> ASTF (sym :&: TypeRep t) o
    -> ASTF (sym :&: TypeRep t) (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
tup15 a b c d e f g h i j k l m n o =
    Sym (inj Tup15 :&: tup15Type (getDecor a) (getDecor b) (getDecor c) (getDecor d) (getDecor e) (getDecor f) (getDecor g) (getDecor h) (getDecor i) (getDecor j) (getDecor k) (getDecor l) (getDecor m) (getDecor n) (getDecor o))
      :$ a :$ b :$ c :$ d :$ e :$ f :$ g :$ h :$ i :$ j :$ k :$ l :$ m :$ n :$ o

