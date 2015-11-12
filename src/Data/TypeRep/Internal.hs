{-# LANGUAGE UndecidableInstances #-}

-- | Open type representations and dynamic types

module Data.TypeRep.Internal where



import Control.Monad.Except
import Data.Char (isAlphaNum)

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
  -- `typeEq`.

instance (TypeEq t1 t, TypeEq t2 t) => TypeEq (t1 :+: t2) t
  where
    typeEqSym (InjL t1, as1) (InjL t2, as2) = typeEqSym (t1,as1) (t2,as2)
    typeEqSym (InjR t1, as1) (InjR t2, as2) = typeEqSym (t1,as1) (t2,as2)
    typeEqSym _ _ = throwError ""

instance TypeEq Empty t
  where
    typeEqSym = error "typeEqSym: Empty"

-- | Equality on type representations
typeEq :: (TypeEq t t, MonadError String m) => TypeRep t a -> TypeRep t b -> m (Dict (a ~ b))
typeEq t1@(TypeRep s1) t2@(TypeRep s2) = case go (s1, Nil) (s2, Nil) of
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

-- | Show the name of type classes
class ShowClass (p :: * -> Constraint)
  where
    -- | Show the name of a type class
    showClass :: Proxy p -> String

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



----------------------------------------------------------------------------------------------------
-- * Dynamic types
----------------------------------------------------------------------------------------------------

-- | Safe cast (does not use @unsafeCoerce@)
cast :: forall t a b . (Typeable t a, Typeable t b, TypeEq t t) =>
    Proxy t -> a -> Either String b
cast _ a = do
    Dict <- typeEq (typeRep :: TypeRep t a) (typeRep :: TypeRep t b)
    return a

-- | Safe generalized cast (does not use @unsafeCoerce@)
gcast :: forall t a b c . (Typeable t a, Typeable t b, TypeEq t t) =>
    Proxy t -> c a -> Either String (c b)
gcast _ a = do
    Dict <- typeEq (typeRep :: TypeRep t a) (typeRep :: TypeRep t b)
    return a

-- | Dynamic type parameterized on a type universe
data Dynamic t
  where
    Dyn :: TypeRep t a -> a -> Dynamic t

toDyn :: Typeable t a => a -> Dynamic t
toDyn = Dyn typeRep

fromDyn :: forall t a . (Typeable t a, TypeEq t t) => Dynamic t -> Either String a
fromDyn (Dyn t a) = do
    Dict <- typeEq t (typeRep :: TypeRep t a)
    return a

instance (TypeEq t t, Witness Eq t t) => Eq (Dynamic t)
  where
    Dyn ta a == Dyn tb b
        | Right Dict <- typeEq ta tb
        , Dict       <- wit pEq ta
        = a == b
    _ == _ = False

instance Witness Show t t => Show (Dynamic t)
  where
    show (Dyn t a) | Dict <- wit pShow t = show a



----------------------------------------------------------------------------------------------------
-- * Specific types/classes
----------------------------------------------------------------------------------------------------

-- | The universal class
class    Any a
instance Any a

instance ShowClass Any          where showClass _ = "Any"
instance ShowClass Eq           where showClass _ = "Eq"
instance ShowClass Ord          where showClass _ = "Ord"
instance ShowClass Show         where showClass _ = "Show"
instance ShowClass Num          where showClass _ = "Num"
instance ShowClass Integral     where showClass _ = "Integral"
instance ShowClass (Typeable t) where showClass _ = "Typeable ..."

-- | Witness a 'Typeable' constraint for a reified type
witTypeable :: Witness (Typeable t) t t => TypeRep t a -> Dict (Typeable t a)
witTypeable = wit Proxy

-- | Partially witness a 'Typeable' constraint for a reified type
pwitTypeable :: PWitness (Typeable t) t t => TypeRep t a -> Either String (Dict (Typeable t a))
pwitTypeable = pwit Proxy

pAny :: Proxy Any
pAny = Proxy

pEq :: Proxy Eq
pEq = Proxy

pOrd :: Proxy Ord
pOrd = Proxy

pShow :: Proxy Show
pShow = Proxy

pNum :: Proxy Num
pNum = Proxy

pIntegral :: Proxy Integral
pIntegral = Proxy

data BoolType  a where BoolType  :: BoolType  (Full Bool)
data CharType  a where CharType  :: CharType  (Full Char)
data IntType   a where IntType   :: IntType   (Full Int)
data FloatType a where FloatType :: FloatType (Full Float)
data ListType  a where ListType  :: ListType  (a :-> Full [a])
data FunType   a where FunType   :: FunType   (a :-> b :-> Full (a -> b))

instance Render BoolType  where renderSym BoolType  = "Bool"
instance Render CharType  where renderSym CharType  = "Char"
instance Render IntType   where renderSym IntType   = "Int"
instance Render FloatType where renderSym FloatType = "Float"

instance Render ListType
  where
    renderSym ListType = "[]"
    renderArgs [a] ListType = "[" ++ a ++ "]"

instance Render FunType
  where
    renderSym FunType = "(->)"
    renderArgs [a,b] FunType = a ++ " -> " ++ b

boolType :: (Syntactic a, BoolType :<: Domain a, Internal a ~ Bool) => a
boolType = sugarSym BoolType

charType :: (Syntactic a, CharType :<: Domain a, Internal a ~ Char) => a
charType = sugarSym CharType

intType :: (Syntactic a, IntType :<: Domain a, Internal a ~ Int) => a
intType = sugarSym IntType

floatType :: (Syntactic a, FloatType :<: Domain a, Internal a ~ Float) => a
floatType = sugarSym FloatType

listType
    :: ( Syntactic list
       , Syntactic elem
       , Domain list ~ Domain elem
       , ListType :<: Domain list
       , Internal list ~ [Internal elem]
       , elem ~ c e
       , list ~ c l
           -- These last equalities are used to help type inference by forcing the representations
           -- to use the same type constructor (e.g. 'TR' or 'TypeRep')
       )
    => elem -> list
listType = sugarSym ListType

funType
    :: ( Syntactic fun
       , Syntactic a
       , Syntactic b
       , Domain fun ~ Domain a
       , Domain fun ~ Domain b
       , FunType :<: Domain fun
       , Internal fun ~ (Internal a -> Internal b)
       , a   ~ c x
       , b   ~ c y
       , fun ~ c z
       )
    => a -> b -> fun
funType = sugarSym FunType

instance (BoolType  :<: t)                             => Typeable t Bool     where typeRep' = boolType
instance (CharType  :<: t)                             => Typeable t Char     where typeRep' = charType
instance (IntType   :<: t)                             => Typeable t Int      where typeRep' = intType
instance (FloatType :<: t)                             => Typeable t Float    where typeRep' = floatType
instance (ListType  :<: t, Typeable t a)               => Typeable t [a]      where typeRep' = listType typeRep'
instance (FunType   :<: t, Typeable t a, Typeable t b) => Typeable t (a -> b) where typeRep' = funType typeRep' typeRep'

instance TypeEq BoolType  t where typeEqSym (BoolType, Nil)  (BoolType, Nil)  = return Dict
instance TypeEq CharType  t where typeEqSym (CharType, Nil)  (CharType, Nil)  = return Dict
instance TypeEq IntType   t where typeEqSym (IntType, Nil)   (IntType, Nil)   = return Dict
instance TypeEq FloatType t where typeEqSym (FloatType, Nil) (FloatType, Nil) = return Dict

instance TypeEq t t => TypeEq ListType t
  where
    typeEqSym (ListType, a :* Nil) (ListType, b :* Nil) = do
        Dict <- typeEq (TypeRep a) (TypeRep b)
        return Dict

instance TypeEq t t => TypeEq FunType t
  where
    typeEqSym (FunType, a1 :* b1 :* Nil) (FunType, a2 :* b2 :* Nil) = do
        Dict <- typeEq (TypeRep a1) (TypeRep a2)
        Dict <- typeEq (TypeRep b1) (TypeRep b2)
        return Dict

instance (BoolType  :<: t) => Witness (Typeable t) BoolType  t where witSym BoolType  Nil = Dict
instance (CharType  :<: t) => Witness (Typeable t) CharType  t where witSym CharType  Nil = Dict
instance (IntType   :<: t) => Witness (Typeable t) IntType   t where witSym IntType   Nil = Dict
instance (FloatType :<: t) => Witness (Typeable t) FloatType t where witSym FloatType Nil = Dict

instance (ListType :<: t, Witness (Typeable t) t t) => Witness (Typeable t) ListType t
  where
    witSym ListType (a :* Nil)
        | Dict <- witTypeable (TypeRep a) = Dict

instance (FunType :<: t, Witness (Typeable t) t t) => Witness (Typeable t) FunType t
  where
    witSym FunType (a :* b :* Nil)
        | Dict <- witTypeable (TypeRep a)
        , Dict <- witTypeable (TypeRep b)
        = Dict

instance (BoolType  :<: t)                            => PWitness (Typeable t) BoolType  t where pwitSym = pwitSymDefault
instance (CharType  :<: t)                            => PWitness (Typeable t) CharType  t where pwitSym = pwitSymDefault
instance (IntType   :<: t)                            => PWitness (Typeable t) IntType   t where pwitSym = pwitSymDefault
instance (FloatType :<: t)                            => PWitness (Typeable t) FloatType t where pwitSym = pwitSymDefault
instance (ListType  :<: t, PWitness (Typeable t) t t) => PWitness (Typeable t) ListType  t where pwitSym ListType (a :* Nil) = do Dict <- pwitTypeable (TypeRep a); return Dict
instance (FunType   :<: t, PWitness (Typeable t) t t) => PWitness (Typeable t) FunType   t where pwitSym FunType (a :* b :* Nil) = do Dict <- pwitTypeable (TypeRep a); Dict <- pwitTypeable (TypeRep b); return Dict

instance Witness Any BoolType  t where witSym _ _ = Dict
instance Witness Any CharType  t where witSym _ _ = Dict
instance Witness Any IntType   t where witSym _ _ = Dict
instance Witness Any FloatType t where witSym _ _ = Dict
instance Witness Any ListType  t where witSym _ _ = Dict
instance Witness Any FunType   t where witSym _ _ = Dict

instance PWitness Any BoolType  t where pwitSym _ _ = return Dict
instance PWitness Any CharType  t where pwitSym _ _ = return Dict
instance PWitness Any IntType   t where pwitSym _ _ = return Dict
instance PWitness Any FloatType t where pwitSym _ _ = return Dict
instance PWitness Any ListType  t where pwitSym _ _ = return Dict
instance PWitness Any FunType   t where pwitSym _ _ = return Dict

instance                   Witness Eq BoolType  t where witSym BoolType  Nil = Dict
instance                   Witness Eq CharType  t where witSym CharType  Nil = Dict
instance                   Witness Eq IntType   t where witSym IntType   Nil = Dict
instance                   Witness Eq FloatType t where witSym FloatType Nil = Dict
instance Witness Eq t t => Witness Eq ListType  t where witSym ListType (a :* Nil) | Dict <- wit pEq (TypeRep a) = Dict

instance                    PWitness Eq BoolType  t where pwitSym = pwitSymDefault
instance                    PWitness Eq CharType  t where pwitSym = pwitSymDefault
instance                    PWitness Eq IntType   t where pwitSym = pwitSymDefault
instance                    PWitness Eq FloatType t where pwitSym = pwitSymDefault
instance PWitness Eq t t => PWitness Eq ListType  t where pwitSym ListType (a :* Nil) = do Dict <- pwit pEq (TypeRep a); return Dict
instance PWitness Eq FunType t

instance                    Witness Ord BoolType  t where witSym BoolType  Nil = Dict
instance                    Witness Ord CharType  t where witSym CharType  Nil = Dict
instance                    Witness Ord IntType   t where witSym IntType   Nil = Dict
instance                    Witness Ord FloatType t where witSym FloatType Nil = Dict
instance Witness Ord t t => Witness Ord ListType  t where witSym ListType (a :* Nil) | Dict <- wit pOrd (TypeRep a) = Dict

instance                     PWitness Ord BoolType  t where pwitSym = pwitSymDefault
instance                     PWitness Ord CharType  t where pwitSym = pwitSymDefault
instance                     PWitness Ord IntType   t where pwitSym = pwitSymDefault
instance                     PWitness Ord FloatType t where pwitSym = pwitSymDefault
instance PWitness Ord t t => PWitness Ord ListType  t where pwitSym ListType (a :* Nil) = do Dict <- pwit pOrd (TypeRep a); return Dict
instance PWitness Ord FunType t

instance                     Witness Show BoolType  t where witSym BoolType  Nil = Dict
instance                     Witness Show CharType  t where witSym CharType  Nil = Dict
instance                     Witness Show IntType   t where witSym IntType   Nil = Dict
instance                     Witness Show FloatType t where witSym FloatType Nil = Dict
instance Witness Show t t => Witness Show ListType  t where witSym ListType (a :* Nil) | Dict <- wit pShow (TypeRep a) = Dict

instance                      PWitness Show BoolType  t where pwitSym = pwitSymDefault
instance                      PWitness Show CharType  t where pwitSym = pwitSymDefault
instance                      PWitness Show IntType   t where pwitSym = pwitSymDefault
instance                      PWitness Show FloatType t where pwitSym = pwitSymDefault
instance PWitness Show t t => PWitness Show ListType  t where pwitSym ListType (a :* Nil) = do Dict <- pwit pShow (TypeRep a); return Dict
instance PWitness Show FunType t

instance Witness Num IntType   t where witSym IntType   Nil = Dict
instance Witness Num FloatType t where witSym FloatType Nil = Dict

instance PWitness Num BoolType  t
instance PWitness Num CharType  t
instance PWitness Num IntType   t where pwitSym = pwitSymDefault
instance PWitness Num FloatType t where pwitSym = pwitSymDefault
instance PWitness Num ListType  t
instance PWitness Num FunType   t

instance Witness Integral IntType t where witSym IntType Nil = Dict

instance PWitness Integral BoolType  t
instance PWitness Integral CharType  t
instance PWitness Integral IntType   t where pwitSym = pwitSymDefault
instance PWitness Integral FloatType t
instance PWitness Integral ListType  t
instance PWitness Integral FunType   t

dynToInteger :: PWitness Integral t t => Dynamic t -> Either String Integer
dynToInteger (Dyn tr a) = do
    Dict <- pwit pIntegral tr
    return (toInteger a)

