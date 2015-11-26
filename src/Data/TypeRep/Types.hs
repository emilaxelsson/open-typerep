{-# LANGUAGE UndecidableInstances #-}

module Data.TypeRep.Types where



import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))

import Language.Syntactic

import Data.TypeRep.Representation



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

data BoolType   a where BoolType   :: BoolType   (Full Bool)
data CharType   a where CharType   :: CharType   (Full Char)
data IntType    a where IntType    :: IntType    (Full Int)
data WordType   a where WordType   :: WordType   (Full Word)
data FloatType  a where FloatType  :: FloatType  (Full Float)
data DoubleType a where DoubleType :: DoubleType (Full Double)
data ListType   a where ListType   :: ListType (a :-> Full [a])
data FunType    a where FunType    :: FunType  (a :-> b :-> Full (a -> b))

instance Render BoolType   where renderSym BoolType   = "Bool"
instance Render CharType   where renderSym CharType   = "Char"
instance Render IntType    where renderSym IntType    = "Int"
instance Render WordType   where renderSym WordType   = "Word"
instance Render FloatType  where renderSym FloatType  = "Float"
instance Render DoubleType where renderSym DoubleType = "Double"

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

wordType :: (Syntactic a, WordType :<: Domain a, Internal a ~ Word) => a
wordType = sugarSym WordType

floatType :: (Syntactic a, FloatType :<: Domain a, Internal a ~ Float) => a
floatType = sugarSym FloatType

doubleType :: (Syntactic a, DoubleType :<: Domain a, Internal a ~ Double) => a
doubleType = sugarSym DoubleType

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

instance (BoolType   :<: t)                             => Typeable t Bool     where typeRep' = boolType
instance (CharType   :<: t)                             => Typeable t Char     where typeRep' = charType
instance (IntType    :<: t)                             => Typeable t Int      where typeRep' = intType
instance (WordType   :<: t)                             => Typeable t Word     where typeRep' = wordType
instance (FloatType  :<: t)                             => Typeable t Float    where typeRep' = floatType
instance (DoubleType :<: t)                             => Typeable t Double   where typeRep' = doubleType
instance (ListType   :<: t, Typeable t a)               => Typeable t [a]      where typeRep' = listType typeRep'
instance (FunType    :<: t, Typeable t a, Typeable t b) => Typeable t (a -> b) where typeRep' = funType typeRep' typeRep'

instance TypeEq BoolType   t where typeEqSym (BoolType, Nil)   (BoolType, Nil)   = return Dict
instance TypeEq CharType   t where typeEqSym (CharType, Nil)   (CharType, Nil)   = return Dict
instance TypeEq IntType    t where typeEqSym (IntType, Nil)    (IntType, Nil)    = return Dict
instance TypeEq WordType   t where typeEqSym (WordType, Nil)   (WordType, Nil)   = return Dict
instance TypeEq FloatType  t where typeEqSym (FloatType, Nil)  (FloatType, Nil)  = return Dict
instance TypeEq DoubleType t where typeEqSym (DoubleType, Nil) (DoubleType, Nil) = return Dict

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

instance (BoolType  :<: t)  => Witness (Typeable t) BoolType   t where witSym BoolType   Nil = Dict
instance (CharType  :<: t)  => Witness (Typeable t) CharType   t where witSym CharType   Nil = Dict
instance (IntType   :<: t)  => Witness (Typeable t) IntType    t where witSym IntType    Nil = Dict
instance (WordType  :<: t)  => Witness (Typeable t) WordType   t where witSym WordType   Nil = Dict
instance (FloatType :<: t)  => Witness (Typeable t) FloatType  t where witSym FloatType  Nil = Dict
instance (DoubleType :<: t) => Witness (Typeable t) DoubleType t where witSym DoubleType Nil = Dict

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

instance (BoolType   :<: t)                            => PWitness (Typeable t) BoolType   t where pwitSym = pwitSymDefault
instance (CharType   :<: t)                            => PWitness (Typeable t) CharType   t where pwitSym = pwitSymDefault
instance (IntType    :<: t)                            => PWitness (Typeable t) IntType    t where pwitSym = pwitSymDefault
instance (WordType   :<: t)                            => PWitness (Typeable t) WordType   t where pwitSym = pwitSymDefault
instance (FloatType  :<: t)                            => PWitness (Typeable t) FloatType  t where pwitSym = pwitSymDefault
instance (DoubleType :<: t)                            => PWitness (Typeable t) DoubleType t where pwitSym = pwitSymDefault
instance (ListType   :<: t, PWitness (Typeable t) t t) => PWitness (Typeable t) ListType   t where pwitSym ListType (a :* Nil) = do Dict <- pwitTypeable (TypeRep a); return Dict
instance (FunType    :<: t, PWitness (Typeable t) t t) => PWitness (Typeable t) FunType    t where pwitSym FunType (a :* b :* Nil) = do Dict <- pwitTypeable (TypeRep a); Dict <- pwitTypeable (TypeRep b); return Dict

instance Witness Any BoolType   t where witSym _ _ = Dict
instance Witness Any CharType   t where witSym _ _ = Dict
instance Witness Any IntType    t where witSym _ _ = Dict
instance Witness Any WordType   t where witSym _ _ = Dict
instance Witness Any FloatType  t where witSym _ _ = Dict
instance Witness Any DoubleType t where witSym _ _ = Dict
instance Witness Any ListType   t where witSym _ _ = Dict
instance Witness Any FunType    t where witSym _ _ = Dict

instance PWitness Any BoolType   t where pwitSym _ _ = return Dict
instance PWitness Any CharType   t where pwitSym _ _ = return Dict
instance PWitness Any IntType    t where pwitSym _ _ = return Dict
instance PWitness Any WordType   t where pwitSym _ _ = return Dict
instance PWitness Any FloatType  t where pwitSym _ _ = return Dict
instance PWitness Any DoubleType t where pwitSym _ _ = return Dict
instance PWitness Any ListType   t where pwitSym _ _ = return Dict
instance PWitness Any FunType    t where pwitSym _ _ = return Dict

instance                   Witness Eq BoolType   t where witSym BoolType   Nil = Dict
instance                   Witness Eq CharType   t where witSym CharType   Nil = Dict
instance                   Witness Eq IntType    t where witSym IntType    Nil = Dict
instance                   Witness Eq WordType   t where witSym WordType   Nil = Dict
instance                   Witness Eq FloatType  t where witSym FloatType  Nil = Dict
instance                   Witness Eq DoubleType t where witSym DoubleType Nil = Dict
instance Witness Eq t t => Witness Eq ListType   t where witSym ListType (a :* Nil) | Dict <- wit pEq (TypeRep a) = Dict

instance                    PWitness Eq BoolType   t where pwitSym = pwitSymDefault
instance                    PWitness Eq CharType   t where pwitSym = pwitSymDefault
instance                    PWitness Eq IntType    t where pwitSym = pwitSymDefault
instance                    PWitness Eq WordType   t where pwitSym = pwitSymDefault
instance                    PWitness Eq FloatType  t where pwitSym = pwitSymDefault
instance                    PWitness Eq DoubleType t where pwitSym = pwitSymDefault
instance PWitness Eq t t => PWitness Eq ListType   t where pwitSym ListType (a :* Nil) = do Dict <- pwit pEq (TypeRep a); return Dict
instance PWitness Eq FunType t

instance                    Witness Ord BoolType   t where witSym BoolType   Nil = Dict
instance                    Witness Ord CharType   t where witSym CharType   Nil = Dict
instance                    Witness Ord IntType    t where witSym IntType    Nil = Dict
instance                    Witness Ord WordType   t where witSym WordType   Nil = Dict
instance                    Witness Ord FloatType  t where witSym FloatType  Nil = Dict
instance                    Witness Ord DoubleType t where witSym DoubleType Nil = Dict
instance Witness Ord t t => Witness Ord ListType   t where witSym ListType (a :* Nil) | Dict <- wit pOrd (TypeRep a) = Dict

instance                     PWitness Ord BoolType   t where pwitSym = pwitSymDefault
instance                     PWitness Ord CharType   t where pwitSym = pwitSymDefault
instance                     PWitness Ord IntType    t where pwitSym = pwitSymDefault
instance                     PWitness Ord WordType   t where pwitSym = pwitSymDefault
instance                     PWitness Ord FloatType  t where pwitSym = pwitSymDefault
instance                     PWitness Ord DoubleType t where pwitSym = pwitSymDefault
instance PWitness Ord t t => PWitness Ord ListType   t where pwitSym ListType (a :* Nil) = do Dict <- pwit pOrd (TypeRep a); return Dict
instance PWitness Ord FunType t

instance                     Witness Show BoolType   t where witSym BoolType   Nil = Dict
instance                     Witness Show CharType   t where witSym CharType   Nil = Dict
instance                     Witness Show IntType    t where witSym IntType    Nil = Dict
instance                     Witness Show WordType   t where witSym WordType   Nil = Dict
instance                     Witness Show FloatType  t where witSym FloatType  Nil = Dict
instance                     Witness Show DoubleType t where witSym DoubleType Nil = Dict
instance Witness Show t t => Witness Show ListType   t where witSym ListType (a :* Nil) | Dict <- wit pShow (TypeRep a) = Dict

instance                      PWitness Show BoolType   t where pwitSym = pwitSymDefault
instance                      PWitness Show CharType   t where pwitSym = pwitSymDefault
instance                      PWitness Show IntType    t where pwitSym = pwitSymDefault
instance                      PWitness Show WordType   t where pwitSym = pwitSymDefault
instance                      PWitness Show FloatType  t where pwitSym = pwitSymDefault
instance                      PWitness Show DoubleType t where pwitSym = pwitSymDefault
instance PWitness Show t t => PWitness Show ListType   t where pwitSym ListType (a :* Nil) = do Dict <- pwit pShow (TypeRep a); return Dict
instance PWitness Show FunType t

instance Witness Num IntType    t where witSym IntType    Nil = Dict
instance Witness Num WordType   t where witSym WordType   Nil = Dict
instance Witness Num FloatType  t where witSym FloatType  Nil = Dict
instance Witness Num DoubleType t where witSym DoubleType Nil = Dict

instance PWitness Num BoolType   t
instance PWitness Num CharType   t
instance PWitness Num IntType    t where pwitSym = pwitSymDefault
instance PWitness Num WordType   t where pwitSym = pwitSymDefault
instance PWitness Num FloatType  t where pwitSym = pwitSymDefault
instance PWitness Num DoubleType t where pwitSym = pwitSymDefault
instance PWitness Num ListType   t
instance PWitness Num FunType    t

instance Witness Integral IntType  t where witSym IntType  Nil = Dict
instance Witness Integral WordType t where witSym WordType Nil = Dict

instance PWitness Integral BoolType   t
instance PWitness Integral CharType   t
instance PWitness Integral IntType    t where pwitSym = pwitSymDefault
instance PWitness Integral WordType   t where pwitSym = pwitSymDefault
instance PWitness Integral FloatType  t
instance PWitness Integral DoubleType t
instance PWitness Integral ListType   t
instance PWitness Integral FunType    t

dynToInteger :: PWitness Integral t t => Dynamic t -> Either String Integer
dynToInteger (Dyn tr a) = do
    Dict <- pwit pIntegral tr
    return (toInteger a)

