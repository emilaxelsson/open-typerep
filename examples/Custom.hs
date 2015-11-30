{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This file demonstrates how to create one's own type universe

import Control.Monad



import Language.Syntactic

import Data.TypeRep.Representation
import Data.TypeRep.TH



-- Universe of types built using 'Bool', 'Int' and '[]'
data Type sig
  where
    Bool_t :: Type (Full Bool)
    Int_t  :: Type (Full Int)
    List_t :: Type (a :-> Full [a])

instance (Type :<: t) => Typeable t Bool where typeRep' = sugarSym Bool_t
instance (Type :<: t) => Typeable t Int  where typeRep' = sugarSym Int_t

instance (Type :<: t, Typeable t a) => Typeable t [a] where typeRep' = sugarSym List_t typeRep'

deriveRender_forType ''Type
deriveTypeEq ''Type
deriveWitness ''Eq ''Type
derivePWitness ''Eq ''Type
deriveWitness ''Show ''Type
derivePWitness ''Show ''Type
deriveWitnessTypeable ''Type

hlist :: [Dynamic Type]
hlist = [toDyn True, toDyn (1 :: Int), toDyn [1,2,3,4 :: Int]]

main = do
    unless test $ fail "Test failed"
    putStrLn "Test passed"
  where
    test = show hlist == "[True,1,[1,2,3,4]]"

