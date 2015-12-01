{-# LANGUAGE UndecidableInstances #-}

-- | Open type representations and dynamic types

module Data.TypeRep.Representation where



import Control.Monad.Except
import Data.Char (isAlphaNum)
import qualified Data.Typeable as Typeable

import Data.Constraint (Constraint, Dict (..))
import Data.Proxy (Proxy (..))

import Language.Syntactic



----------------------------------------------------------------------------------------------------
-- * Type representations
----------------------------------------------------------------------------------------------------

-- | 'Full'-indexed type representation
type TR = AST

-- | This class provides reification of type @a@ in a universe @t@. @`Typeable` t a@ means that @a@
-- is in the type universe represented by @t@.
class Typeable t a
  where
    typeRep' :: TR t (Full a)

-- | Representation of type @a@ in a type universe @t@
--
-- This type can also be seen as a witness that @a@ is a member of @t@ (i.e. @`Typeable` t a@); see
-- 'witTypeable'.
newtype TypeRep t a = TypeRep { unTypeRep :: TR t (Full a) }
  -- The newtype is mainly because 'TR' cannot be partially applied

instance Render t => Show (TypeRep t a)
  where
    show = render . desugar

instance Syntactic (TypeRep t a)
  where
    type Domain (TypeRep t a)   = t
    type Internal (TypeRep t a) = a
    desugar = unTypeRep
    sugar   = TypeRep

-- | Reification of type @a@ in a type universe @t@
typeRep :: Typeable t a => TypeRep t a
typeRep = TypeRep typeRep'

-- | Equality on type representations
class Render t => TypeEq t u
  where
    typeEqSym
        :: (t sig1, Args (AST u) sig1)
        -> (t sig2, Args (AST u) sig2)
        -> Either String (Dict (DenResult sig1 ~ DenResult sig2))
  -- The reason to have `Render` as a super class is not to leak unnecessary stuff in the type of
  -- `typeEqM`/`typeEq`.

instance (TypeEq t1 t, TypeEq t2 t) => TypeEq (t1 :+: t2) t
  where
    typeEqSym (InjL t1, as1) (InjL t2, as2) = typeEqSym (t1,as1) (t2,as2)
    typeEqSym (InjR t1, as1) (InjR t2, as2) = typeEqSym (t1,as1) (t2,as2)
    typeEqSym (t1,_) (t2,_) = throwError ""

instance TypeEq Empty t
  where
    typeEqSym = error "typeEqSym: Empty"

-- | Equality on type representations
typeEqM :: (TypeEq t t, MonadError String m) => TypeRep t a -> TypeRep t b -> m (Dict (a ~ b))
typeEqM t1@(TypeRep s1) t2@(TypeRep s2) = case go (s1, Nil) (s2, Nil) of
    Left _     -> throwError $ "type mismatch: " ++ show t1 ++ " /= " ++ show t2
    Right Dict -> return Dict
  where
    go :: TypeEq t t
      => (AST t sig1, Args (AST t) sig1)
      -> (AST t sig2, Args (AST t) sig2)
      -> Either String (Dict ((DenResult sig1 ~ DenResult sig2)))
    go (Sym t1, as1)   (Sym t2, as2)   = typeEqSym (t1,as1) (t2,as2)
    go (s1 :$ a1, as1) (s2 :$ a2, as2) = go (s1, a1 :* as1) (s2, a2 :* as2)
    go _ _ = throwError ""

-- | Equality on type representations
typeEq :: TypeEq t t => TypeRep t a -> TypeRep t b -> Maybe (Dict (a ~ b))
typeEq t1 t2 = either (const Nothing) Just $ typeEqM t1 t2

-- | Type constructor matching. This function makes it possible to match on type representations
-- without dealing with the underlying 'AST' representation.
--
-- For example, to check that a 'TypeRep' represents the type @a -> Int@ for some @a@:
--
-- > is_atoi :: (TypeEq t t, IntType :<: t) => TypeRep t a -> Bool
-- > is_atoi t
-- >     | [E ta, E tb] <- matchCon t
-- >     , Just _       <- typeEq ta intType = True
-- >     | otherwise                         = False
matchCon :: TypeRep t c -> [E (TypeRep t)]
matchCon = simpleMatch (\_ -> foldrArgs (\t -> (E (TypeRep t) :)) []) . unTypeRep

-- | Monadic version of 'matchCon'
--
-- > matchConM = return . matchCon
--
-- 'matchConM' is convenient when matching types in a monad, e.g.:
--
-- > do ...
-- >    [E ta, E tb] <- matchConM t
-- >    Dict         <- typeEq ta tb
-- >    ...
matchConM :: Monad m => TypeRep t c -> m [E (TypeRep t)]
matchConM = return . matchCon

-- | Witness a type constraint for a reified type
class Witness p t u
  where
    witSym :: t sig -> Args (AST u) sig -> Dict (p (DenResult sig))

instance (Witness p t1 t, Witness p t2 t) => Witness p (t1 :+: t2) t
  where
    witSym (InjL s) as = witSym s as
    witSym (InjR s) as = witSym s as

instance Witness p t t => Witness p (AST t) t
  where
    witSym (Sym s)  as = witSym s as
    witSym (s :$ a) as = witSym s (a :* as)

-- | Partially witness a type constraint for a reified type
class (ShowClass p, Render t) => PWitness p t u
  where
    pwitSym :: t sig -> Args (AST u) sig -> Either String (Dict (p (DenResult sig)))
    pwitSym _ _ = throwError ""
  -- The reason to have `Render` as a super class is not to leak unnecessary stuff in the type of
  -- `pwit`.

instance (PWitness p t1 t, PWitness p t2 t) => PWitness p (t1 :+: t2) t
  where
    pwitSym (InjL s) as = pwitSym s as
    pwitSym (InjR s) as = pwitSym s as

-- | Default implementation of 'pwitSym' for types that have a 'Witness' instance
pwitSymDefault :: Witness p t u =>
    t sig -> Args (AST u) sig -> Either String (Dict (p (DenResult sig)))
pwitSymDefault t = return . witSym t

-- | Witness a type constraint for a reified type
wit :: forall p t a . Witness p t t => Proxy p -> TypeRep t a -> Dict (p a)
wit _ (TypeRep a) = witSym a (Nil :: Args (AST t) (Full a))

-- | Partially witness a type constraint for a reified type
pwit :: forall p t m a . (PWitness p t t, MonadError String m) =>
    Proxy p -> TypeRep t a -> m (Dict (p a))
pwit p t@(TypeRep a) = case go a Nil of
    Left _  -> throwError $ unwords ["cannot deduce", showClass p, classArg]
    Right a -> return a
  where
    st       = show t
    classArg = if all isAlphaNum st then st else "(" ++ st ++ ")"

    go :: AST t sig -> Args (AST t) sig -> Either String (Dict (p (DenResult sig)))
    go (Sym s)  as = pwitSym s as
    go (s :$ a) as = go s (a :* as)

-- | Witness a 'Typeable' constraint for a reified type
witTypeable :: Witness (Typeable t) t t => TypeRep t a -> Dict (Typeable t a)
witTypeable = wit Proxy

-- | Partially witness a 'Typeable' constraint for a reified type
pwitTypeable :: PWitness (Typeable t) t t =>
    TypeRep t a -> Either String (Dict (Typeable t a))
pwitTypeable = pwit Proxy



----------------------------------------------------------------------------------------------------
-- * Dynamic types
----------------------------------------------------------------------------------------------------

-- | Safe cast (does not use @unsafeCoerce@)
cast :: forall t a b . (Typeable t a, Typeable t b, TypeEq t t) =>
    Proxy t -> a -> Either String b
cast _ a = do
    Dict <- typeEqM (typeRep :: TypeRep t a) (typeRep :: TypeRep t b)
    return a

-- | Safe generalized cast (does not use @unsafeCoerce@)
gcast :: forall t a b c . (Typeable t a, Typeable t b, TypeEq t t) =>
    Proxy t -> c a -> Either String (c b)
gcast _ a = do
    Dict <- typeEqM (typeRep :: TypeRep t a) (typeRep :: TypeRep t b)
    return a

-- | Dynamic type parameterized on a type universe
data Dynamic t
  where
    Dyn :: TypeRep t a -> a -> Dynamic t

toDyn :: Typeable t a => a -> Dynamic t
toDyn = Dyn typeRep

fromDyn :: forall t a . (Typeable t a, TypeEq t t) => Dynamic t -> Either String a
fromDyn (Dyn t a) = do
    Dict <- typeEqM t (typeRep :: TypeRep t a)
    return a

instance (TypeEq t t, Witness Eq t t) => Eq (Dynamic t)
  where
    Dyn ta a == Dyn tb b
        | Just Dict <- typeEq ta tb
        , Dict      <- wit (Proxy :: Proxy Eq) ta
        = a == b
    _ == _ = False

instance Witness Show t t => Show (Dynamic t)
  where
    show (Dyn t a) | Dict <- wit (Proxy :: Proxy Show) t = show a



----------------------------------------------------------------------------------------------------
-- * Misc.
----------------------------------------------------------------------------------------------------

-- | The universal class
class    Any a
instance Any a

-- | Show the name of type classes
class ShowClass (p :: * -> Constraint)
  where
    -- | Show the name of a type class
    showClass :: Proxy p -> String

instance ShowClass Any               where showClass _ = "Any"
instance ShowClass Typeable.Typeable where showClass _ = "Data.Typeable"
instance ShowClass Eq                where showClass _ = "Eq"
instance ShowClass Ord               where showClass _ = "Ord"
instance ShowClass Show              where showClass _ = "Show"
instance ShowClass Num               where showClass _ = "Num"
instance ShowClass Integral          where showClass _ = "Integral"
instance ShowClass (Typeable t)      where showClass _ = "Typeable t"

-- | Proxy of 'Any' class. Can be passed to 'wit' and 'pwit'.
pAny :: Proxy Any
pAny = Proxy

-- | Proxy of 'Typeable.Typeable' class (from the base library). Can be passed
-- to 'wit' and 'pwit'.
pDataTypeable :: Proxy Typeable.Typeable
pDataTypeable = Proxy

-- | Proxy of 'Eq' class. Can be passed to 'wit' and 'pwit'.
pEq :: Proxy Eq
pEq = Proxy

-- | Proxy of 'Ord' class. Can be passed to 'wit' and 'pwit'.
pOrd :: Proxy Ord
pOrd = Proxy

-- | Proxy of 'Show' class. Can be passed to 'wit' and 'pwit'.
pShow :: Proxy Show
pShow = Proxy

-- | Proxy of 'Num' class. Can be passed to 'wit' and 'pwit'.
pNum :: Proxy Num
pNum = Proxy

-- | Proxy of 'Integral' class. Can be passed to 'wit' and 'pwit'.
pIntegral :: Proxy Integral
pIntegral = Proxy

