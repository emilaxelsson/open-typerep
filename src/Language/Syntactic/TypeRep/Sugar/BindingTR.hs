{-# LANGUAGE UndecidableInstances #-}

-- | 'Syntactic' instance for functions
--
-- This module is based on domains of the form
-- @((... `:+:` `BindingT` `:+:` ... ) `:&:` `TypeRep` t)@

module Language.Syntactic.TypeRep.Sugar.BindingTR where



import qualified Data.Typeable as Typeable

import Language.Syntactic
import Language.Syntactic.Functional

import Data.TypeRep
import Data.TypeRep.Types.Basic
import Language.Syntactic.TypeRep



instance
    ( sym ~ (s :&: TypeRep t)
    , Syntactic a, Domain a ~ sym
    , Syntactic b, Domain b ~ sym
    , BindingT :<: s
    , Typeable t (Internal a)
    , Typeable t (Internal b)
    , Witness Typeable.Typeable t t
    , FunType :<: t
    ) =>
      Syntactic (a -> b)
  where
    type Domain (a -> b)   = Domain a
    type Internal (a -> b) = Internal a -> Internal b

    desugar f = lamT_template mkVar mkLam (desugar . f . sugar)
      where
        ta :: TypeRep t (Internal a)
        ta = typeRep

        tb :: TypeRep t (Internal b)
        tb = typeRep

        mkVar :: Name -> sym (Full (Internal a))
        mkVar v = inj (mkVarSym ta v) :&: ta

        mkLam :: Name -> sym (Internal b :-> Full (Internal a -> Internal b))
        mkLam v = inj (mkLamSym ta tb v) :&: funType ta tb

    sugar = error "sugar not implemented for (a -> b)"

