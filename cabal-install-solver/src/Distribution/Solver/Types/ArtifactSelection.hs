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
    , unArtifactSelection
    , artsSubsetOf
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Data.Function (on)

import qualified Data.Set as S

import Distribution.Pretty ( Pretty(pretty) )

-- | A type for specifying which artifacts are available to be required.
newtype ArtifactSelection = ArtifactSelection (S.Set ArtifactKind)
  deriving (Eq, Show, Generic, Semigroup, Monoid)

instance Pretty ArtifactSelection where
  pretty (ArtifactSelection artSet)
    | artSet == allArtifacts   = PP.text "all artifacts"
    | artSet == dynOutsOnly    = PP.text "dynamic artifacts only"
    | artSet == staticOutsOnly = PP.text "static artifacts only"
    | artSet == noOuts         = PP.text "no output artifacts"
    | otherwise                = PP.text "unknown artifacts"

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

-- | Obtain the set of artifact kinds included in this artifact selection.
unArtifactSelection :: ArtifactSelection -> S.Set ArtifactKind
unArtifactSelection (ArtifactSelection set) = set

-- | Is a selection a subset of another?
artsSubsetOf :: ArtifactSelection -> ArtifactSelection -> Artifactselection
artsSubsetOf = S.isSubsetOf `on` unArtifactSelection

-- | Return artifacts in the first set not present in the second set.
artsDifference :: ArtifactSelection -> ArtifactSelection
artsDifference (ArtifactSelection a) (ArtifactSelection b) =
  ArtifactSelection $ a `S.difference` b

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
