{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representations for signed and unsigned integer types
--
-- The reason for using symbol names ending with @_t@ is that 'deriveRender'
-- uses everything that comes before @_@ when rendering the constructor.

module Data.TypeRep.Types.IntWord where



import Data.Int
import qualified Data.Typeable as Typeable
import Data.Word

import Language.Syntactic

import Data.TypeRep.TH



data IntWordType a
  where
    Int8_t   :: IntWordType (Full Int8)
    Int16_t  :: IntWordType (Full Int16)
    Int32_t  :: IntWordType (Full Int32)
    Int64_t  :: IntWordType (Full Int64)
    Word8_t  :: IntWordType (Full Word8)
    Word16_t :: IntWordType (Full Word16)
    Word32_t :: IntWordType (Full Word32)
    Word64_t :: IntWordType (Full Word64)

int8Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int8) => a
int8Type = sugarSym Int8_t

int16Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int16) => a
int16Type = sugarSym Int16_t

int32Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int32) => a
int32Type = sugarSym Int32_t

int64Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Int64) => a
int64Type = sugarSym Int64_t

word8Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word8) => a
word8Type = sugarSym Word8_t

word16Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word16) => a
word16Type = sugarSym Word16_t

word32Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word32) => a
word32Type = sugarSym Word32_t

word64Type :: (Syntactic a, IntWordType :<: Domain a, Internal a ~ Word64) => a
word64Type = sugarSym Word64_t

deriveRender_forType ''IntWordType
deriveTypeEq         ''IntWordType
deriveWitnessAny     ''IntWordType
derivePWitnessAny    ''IntWordType

deriveWitness ''Typeable.Typeable ''IntWordType
deriveWitness ''Eq       ''IntWordType
deriveWitness ''Ord      ''IntWordType
deriveWitness ''Show     ''IntWordType
deriveWitness ''Num      ''IntWordType
deriveWitness ''Integral ''IntWordType

derivePWitness ''Typeable.Typeable ''IntWordType
derivePWitness ''Eq       ''IntWordType
derivePWitness ''Ord      ''IntWordType
derivePWitness ''Show     ''IntWordType
derivePWitness ''Num      ''IntWordType
derivePWitness ''Integral ''IntWordType

