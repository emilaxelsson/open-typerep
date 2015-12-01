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
      -- * Type class witnessing
    , Any
    , witTypeable
    , pwitTypeable
      -- * Misc.
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

