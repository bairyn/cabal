{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.InferUnspecified
    ( InferUnspecified(..)
    ) where

import Distribution.Compat.Prelude

data InferUnspecified
  = NoInferUnspecified
  | InferUnspecified
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic, Typeable)

instance Binary InferUnspecified
instance Structured InferUnspecified
