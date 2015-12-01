-- | Utilities for working with ASTs of the form @`AST` (sym `:&:` `TypeRep` t)@

module Language.Syntactic.TypeRep where



import qualified Data.Typeable as Typeable

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Sharing

import Data.TypeRep
import Data.TypeRep.Types.Basic



mkVarSym :: Witness Typeable.Typeable t t =>
    TypeRep t a -> Name -> BindingT (Full a)
mkVarSym t v | Dict <- wit pDataTypeable t = VarT v

mkLamSym :: Witness Typeable.Typeable t t
    => TypeRep t a -> TypeRep t b -> Name
    -> BindingT (b :-> Full (a -> b))
mkLamSym ta _ v | Dict <- wit pDataTypeable ta = LamT v

-- | Inject a symbol in an 'AST' with a domain decorated by a type
-- representation
injTR :: (sub :<: sup, Typeable t (DenResult sig)) =>
    sub sig -> AST (sup :&: TypeRep t) sig
injTR s = Sym (inj s :&: typeRep)

-- | Make a smart constructor of a symbol. 'smartSymT' has any type of the form:
--
-- > smartSymTR :: (sub :<: AST (sup :&: TypeRep t), Typeable t x)
-- >     => sub (a :-> b :-> ... :-> Full x)
-- >     -> (ASTF sup a -> ASTF sup b -> ... -> ASTF sup x)
smartSymTR
    :: ( Signature sig
       , supT ~ (sup :&: TypeRep t)
       , f    ~ SmartFun supT sig
       , sig  ~ SmartSig f
       , supT ~ SmartSym f
       , sub :<: sup
       , Typeable t (DenResult sig)
       )
    => sub sig -> f
smartSymTR s = smartSym' (inj s :&: typeRep)

-- | \"Sugared\" symbol application
--
-- 'sugarSymTR' has any type of the form:
--
-- > sugarSymTR ::
-- >     ( sub :<: AST (sup :&: TypeRep t)
-- >     , Syntactic a
-- >     , Syntactic b
-- >     , ...
-- >     , Syntactic x
-- >     , Domain a ~ Domain b ~ ... ~ Domain x
-- >     , Typeable t (Internal x)
-- >     ) => sub (Internal a :-> Internal b :-> ... :-> Full (Internal x))
-- >       -> (a -> b -> ... -> x)
sugarSymTR
    :: ( Signature sig
       , supT ~ (sup :&: TypeRep t)
       , fi   ~ SmartFun supT sig
       , sig  ~ SmartSig fi
       , supT ~ SmartSym fi
       , SyntacticN f fi
       , sub :<: sup
       , Typeable t (DenResult sig)
       )
    => sub sig -> f
sugarSymTR = sugarN . smartSymTR

-- | Default 'CodeMotionInterface' for domains of the form
-- @((... `:+:` `BindingT` `:+:` ... ) `:&:` `TypeRep` t)@
defaultInterfaceTypeRep :: forall binding sym symT t
    .  ( BindingT :<: sym
       , Let      :<: sym
       , symT ~ (sym :&: TypeRep t)
       , FunType :<: t
       , TypeEq t t
       , Witness Typeable.Typeable t t
       )
    => (forall a b . ASTF symT a -> ASTF symT b -> Bool)
         -- ^ Can the expression represented by the first argument be shared in
         -- the second argument?
    -> (forall a . ASTF symT a -> Bool)
         -- ^ Can we hoist over this expression?
    -> CodeMotionInterface symT
defaultInterfaceTypeRep = defaultInterfaceDecor typeEq funType mkVarSym mkLamSym

