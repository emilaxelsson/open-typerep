{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Typeable' instances for signed and unsigned integer types. The reason for
-- having these in a separate module is that it might be desired to have these
-- instances with other type representations.
--
-- For example, instead of the instance
--
-- > (IntWordType :<: t) => Typeable t Int8
--
-- one might want to have
--
-- > Typeable MyTypeRep Int8

module Data.TypeRep.Types.IntWord.Typeable where



import Data.Int
import Data.Word

import Language.Syntactic

import Data.TypeRep.Representation
import Data.TypeRep.TH
import Data.TypeRep.Types.IntWord



instance (IntWordType :<: t) => Typeable t Int8   where typeRep' = int8Type
instance (IntWordType :<: t) => Typeable t Int16  where typeRep' = int16Type
instance (IntWordType :<: t) => Typeable t Int32  where typeRep' = int32Type
instance (IntWordType :<: t) => Typeable t Int64  where typeRep' = int64Type
instance (IntWordType :<: t) => Typeable t Word8  where typeRep' = word8Type
instance (IntWordType :<: t) => Typeable t Word16 where typeRep' = word16Type
instance (IntWordType :<: t) => Typeable t Word32 where typeRep' = word32Type
instance (IntWordType :<: t) => Typeable t Word64 where typeRep' = word64Type

deriveWitnessTypeable  ''IntWordType
derivePWitnessTypeable ''IntWordType

