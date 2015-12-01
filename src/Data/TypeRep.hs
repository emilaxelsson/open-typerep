-- | Open type representations and dynamic types

module Data.TypeRep
    ( -- * Helper types
      module Data.Constraint
    , module Data.Proxy
    , module Language.Syntactic
      -- * Type representations
    , Typeable
    , TypeRep
    , typeRep
    , TypeEq
    , typeEqM
    , typeEq
    , matchCon
    , matchConM
    , Witness
    , PWitness
    , wit
    , pwit
    , witTypeable
    , pwitTypeable
      -- * Dynamic types
    , cast
    , gcast
    , Dynamic (..)
    , toDyn
    , fromDyn
      -- * Misc.
    , Any
    , pAny
    , pDataTypeable
    , pEq
    , pOrd
    , pShow
    , pNum
    , pIntegral
      -- * Sub-universes
    , module Data.TypeRep.Sub
    ) where



import Data.Constraint (Dict (..))
import Data.Proxy (Proxy (..))

import Language.Syntactic ((:+:), Project (..), (:<:) (..), E (..))

import Data.TypeRep.Representation
import Data.TypeRep.Sub

