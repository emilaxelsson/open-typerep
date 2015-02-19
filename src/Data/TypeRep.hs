-- | Open type representations and dynamic types

module Data.TypeRep
    ( -- * Helper types
      module Data.Constraint
    , module Data.Proxy
    , module Data.Syntactic
      -- * Type representations
    , Typeable
    , TypeRep
    , typeRep
    , TypeEq
    , typeEq
    , matchCon
    , matchConM
    , Witness
    , PWitness
    , wit
    , pwit
      -- * Dynamic types
    , cast
    , gcast
    , Dynamic (..)
    , toDyn
    , fromDyn
    , dynToInteger
      -- * Type class witnessing
    , Any
    , witTypeable
    , pwitTypeable
    , pAny
    , pEq
    , pOrd
    , pShow
    , pNum
    , pIntegral
    , BoolType
    , CharType
    , IntType
    , FloatType
    , ListType
    , FunType
    , boolType
    , charType
    , intType
    , floatType
    , listType
    , funType
      -- * Sub-universes
    , module Data.TypeRep.Sub
    ) where



import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))

import Data.Syntactic ((:+:), Project (..), (:<:) (..), E (..), Render (..))

import Data.TypeRep.Internal
import Data.TypeRep.Sub

