{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Provide a type for categorizing artifact requirements.
module Distribution.Solver.Types.ArtifactSelection
    ( ArtifactSelection(..)
    , ArtifactKind(..)
    , allArtifacts
    , dynOutsOnly
    , staticOutsOnly
    , noOuts
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import qualified Data.Set as S

-- | A type for specifying which artifacts are available to be required.
newtype ArtifactSelection = ArtifactSelection (S.Set ArtifactKind)
  deriving (Eq, Show, Generic, Semigroup, Monoid)

-- | Specific kinds of artifacts.
data ArtifactKind
  = DynOuts     -- ^ Exclude static outputs.
  | StaticOuts  -- ^ Exclude dynamic outputs.
  deriving (Eq, Show, Generic, Ord)

-- | ArtifactSelection alias: e.g. dynamic and static interface files.
allArtifacts :: ArtifactSelection
allArtifacts = ArtifactSelection $ S.fromList [DynOuts, StaticOuts]

-- | ArtifactSelection alias: exclude static outputs.
dynOutsOnly :: ArtifactSelection
dynOutsOnly = ArtifactSelection $ S.fromList [DynOuts]

-- | ArtifactSelection alias: exclude static outputs.
staticOutsOnly :: ArtifactSelection
staticOutsOnly = ArtifactSelection $ S.fromList [StaticOuts]

-- | ArtifactSelection alias: exclude all artifacts.
noOuts :: ArtifactSelection
noOuts = ArtifactSelection $ S.fromList []

{-
-- Aliases for 

type AllArtifacts 

-- | A type for specifying which artifacts are available to be required.
data ArtifactSelection
    AllArtifacts    -- ^ e.g. dynamic and static interface files.
  | DynOutsOnly     -- ^ Exclude static outputs.
  | StaticOutsOnly  -- ^ Exclude dynamic artifacts.
  | NoOuts          -- ^ Exclude all artifacts.
  deriving (Eq, Show, Generic)

-- | Union of available arifacts.
orArtifacts :: ArtifactSelection -> ArtifactSelection -> ArtifactSelection
orArtifacts NoOuts             sel              = sel
orArtifacts sel@StaticOutsOnly NoOuts           = sel
orArtifacts sel@StaticOutsOnly StaticOutsOnly   = sel
orArtifacts StaticOutsOnly     DynOutsOnly      = AllArtifacts
orArtifacts StaticOutsOnly     sel@AllArtifacts = sel
orArtifacts sel@DynOutsOnly    NoOuts           = sel
orArtifacts DynOutsOnly        StaticOutsOnly   = AllArtifacts
orArtifacts sel@DynOutsOnly    DynOutsOnly      = sel
orArtifacts DynOutsOnly        sel@AllArtifacts = sel
orArtifacts sel@AllArtifacts   _                = sel

-- | Complement of the selection.
complArtifacts :: ArtifactSelections -> ArtifactSelections
complArtifacts NoOuts         = AllArtifacts
complArtifacts StaticOutsOnly = DynOutsOnly
complArtifacts DynOutsOnly    = StaticOutsOnly
complArtifacts AllArtifacts   = NoOuts

-- | Union of required artifacts.
andArtifacts :: ArtifactSelection -> ArtifactSelection -> ArtifactSelection
-}
