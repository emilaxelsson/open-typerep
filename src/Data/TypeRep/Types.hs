{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representations for specific types
--
-- The reason for using symbol names ending with @_t@ is that 'deriveRender'
-- uses everything that comes before @_@ when rendering the constructor.

module Data.TypeRep.Types where



import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))

import Language.Syntactic

import Data.TypeRep.Representation
import Data.TypeRep.TH



instance ShowClass Any          where showClass _ = "Any"
instance ShowClass Eq           where showClass _ = "Eq"
instance ShowClass Ord          where showClass _ = "Ord"
instance ShowClass Show         where showClass _ = "Show"
instance ShowClass Num          where showClass _ = "Num"
instance ShowClass Integral     where showClass _ = "Integral"
instance ShowClass (Typeable t) where showClass _ = "Typeable ..."



--------------------------------------------------------------------------------
-- * Class proxies
--------------------------------------------------------------------------------

-- These can be passed to 'wit' and 'pwit'

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



--------------------------------------------------------------------------------
-- * Specific type representations
--------------------------------------------------------------------------------

data BoolType   a where Bool_t   :: BoolType   (Full Bool)
data CharType   a where Char_t   :: CharType   (Full Char)
data IntType    a where Int_t    :: IntType    (Full Int)
data WordType   a where Word_t   :: WordType   (Full Word)
data FloatType  a where Float_t  :: FloatType  (Full Float)
data DoubleType a where Double_t :: DoubleType (Full Double)
data ListType   a where List_t   :: ListType (a :-> Full [a])
data FunType    a where Fun_t    :: FunType  (a :-> b :-> Full (a -> b))

boolType :: (Syntactic a, BoolType :<: Domain a, Internal a ~ Bool) => a
boolType = sugarSym Bool_t

charType :: (Syntactic a, CharType :<: Domain a, Internal a ~ Char) => a
charType = sugarSym Char_t

intType :: (Syntactic a, IntType :<: Domain a, Internal a ~ Int) => a
intType = sugarSym Int_t

wordType :: (Syntactic a, WordType :<: Domain a, Internal a ~ Word) => a
wordType = sugarSym Word_t

floatType :: (Syntactic a, FloatType :<: Domain a, Internal a ~ Float) => a
floatType = sugarSym Float_t

doubleType :: (Syntactic a, DoubleType :<: Domain a, Internal a ~ Double) => a
doubleType = sugarSym Double_t

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
listType = sugarSym List_t

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
funType = sugarSym Fun_t

deriveRender_forType ''BoolType
deriveRender_forType ''CharType
deriveRender_forType ''IntType
deriveRender_forType ''WordType
deriveRender_forType ''FloatType
deriveRender_forType ''DoubleType

instance Render ListType
  where
    renderSym List_t = "[]"
    renderArgs [a] List_t = "[" ++ a ++ "]"

instance Render FunType
  where
    renderSym Fun_t = "(->)"
    renderArgs = renderArgsSmart

deriveTypeEq ''BoolType
deriveTypeEq ''CharType
deriveTypeEq ''IntType
deriveTypeEq ''WordType
deriveTypeEq ''FloatType
deriveTypeEq ''DoubleType
deriveTypeEq ''ListType
deriveTypeEq ''FunType

deriveWitnessAny ''BoolType
deriveWitnessAny ''CharType
deriveWitnessAny ''IntType
deriveWitnessAny ''WordType
deriveWitnessAny ''FloatType
deriveWitnessAny ''DoubleType
deriveWitnessAny ''ListType
deriveWitnessAny ''FunType

derivePWitnessAny ''BoolType
derivePWitnessAny ''CharType
derivePWitnessAny ''IntType
derivePWitnessAny ''WordType
derivePWitnessAny ''FloatType
derivePWitnessAny ''DoubleType
derivePWitnessAny ''ListType
derivePWitnessAny ''FunType

deriveWitness ''Eq ''BoolType
deriveWitness ''Eq ''CharType
deriveWitness ''Eq ''IntType
deriveWitness ''Eq ''WordType
deriveWitness ''Eq ''FloatType
deriveWitness ''Eq ''DoubleType
deriveWitness ''Eq ''ListType

derivePWitness ''Eq ''BoolType
derivePWitness ''Eq ''CharType
derivePWitness ''Eq ''IntType
derivePWitness ''Eq ''WordType
derivePWitness ''Eq ''FloatType
derivePWitness ''Eq ''DoubleType
derivePWitness ''Eq ''ListType

deriveWitness ''Ord ''BoolType
deriveWitness ''Ord ''CharType
deriveWitness ''Ord ''IntType
deriveWitness ''Ord ''WordType
deriveWitness ''Ord ''FloatType
deriveWitness ''Ord ''DoubleType
deriveWitness ''Ord ''ListType

derivePWitness ''Ord ''BoolType
derivePWitness ''Ord ''CharType
derivePWitness ''Ord ''IntType
derivePWitness ''Ord ''WordType
derivePWitness ''Ord ''FloatType
derivePWitness ''Ord ''DoubleType
derivePWitness ''Ord ''ListType

deriveWitness ''Show ''BoolType
deriveWitness ''Show ''CharType
deriveWitness ''Show ''IntType
deriveWitness ''Show ''WordType
deriveWitness ''Show ''FloatType
deriveWitness ''Show ''DoubleType
deriveWitness ''Show ''ListType

derivePWitness ''Show ''BoolType
derivePWitness ''Show ''CharType
derivePWitness ''Show ''IntType
derivePWitness ''Show ''WordType
derivePWitness ''Show ''FloatType
derivePWitness ''Show ''DoubleType
derivePWitness ''Show ''ListType

deriveWitness ''Num ''IntType
deriveWitness ''Num ''WordType
deriveWitness ''Num ''FloatType
deriveWitness ''Num ''DoubleType

derivePWitness ''Num ''IntType
derivePWitness ''Num ''WordType
derivePWitness ''Num ''FloatType
derivePWitness ''Num ''DoubleType

deriveWitness ''Integral ''IntType
deriveWitness ''Integral ''WordType

derivePWitness ''Integral ''IntType
derivePWitness ''Integral ''WordType



-- 'PWitness' instances for non-members

instance PWitness Eq FunType t

instance PWitness Ord FunType t

instance PWitness Show FunType t

instance PWitness Num BoolType   t
instance PWitness Num CharType   t
instance PWitness Num ListType   t
instance PWitness Num FunType    t

instance PWitness Integral BoolType   t
instance PWitness Integral CharType   t
instance PWitness Integral FloatType  t
instance PWitness Integral DoubleType t
instance PWitness Integral ListType   t
instance PWitness Integral FunType    t



--------------------------------------------------------------------------------
-- * Misc.
--------------------------------------------------------------------------------

dynToInteger :: PWitness Integral t t => Dynamic t -> Either String Integer
dynToInteger (Dyn tr a) = do
    Dict <- pwit pIntegral tr
    return (toInteger a)

