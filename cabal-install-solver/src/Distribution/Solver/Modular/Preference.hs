{-# LANGUAGE ScopedTypeVariables #-}
-- | Reordering or pruning the tree in order to prefer or make certain choices.
module Distribution.Solver.Modular.Preference
    ( avoidReinstalls
    , deferSetupExeChoices
    , deferWeakFlagChoices
    , enforceManualFlags
    , enforcePackageConstraints
    , enforceSingleInstanceRestriction
    , enforceArtifactRequirements
    , firstGoal
    , preferBaseGoalChoice
    , preferLinked
    , preferPackagePreferences
    , preferReallyEasyGoalChoices
    , requireInstalled
    , onlyConstrained
    , sortGoals
    , pruneAfterFirstSuccess
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.List as L
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.Reader (Reader, runReader, mapReaderT, ask, local, asks)

import Distribution.PackageDescription (lookupFlagAssignment, unFlagAssignment) -- from Cabal

import Distribution.Solver.Types.Flag
import Distribution.Solver.Types.InstalledPreference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.Variable

import Distribution.Solver.Types.ArtifactSelection
import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Var
import Distribution.Solver.Modular.Version
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W

-- | Update the weights of children under 'PChoice' nodes. 'addWeights' takes a
-- list of weight-calculating functions in order to avoid sorting the package
-- choices multiple times. Each function takes the package name, sorted list of
-- children's versions, and package option. 'addWeights' prepends the new
-- weights to the existing weights, which gives precedence to preferences that
-- are applied later.
addWeights :: [PN -> [Ver] -> POption -> Weight] -> EndoTreeTrav d c
addWeights fs = go
  where
    go :: TreeF d c (Tree d c) -> TreeF d c (Tree d c)
    go (PChoiceF qpn@(Q _ pn) rdm x cs) =
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
          weights k = [f pn sortedVersions k | f <- fs]

          elemsToWhnf :: [a] -> ()
          elemsToWhnf = foldr seq ()
      in  PChoiceF qpn rdm x
          -- Evaluate the children's versions before evaluating any of the
          -- subtrees, so that 'sortedVersions' doesn't hold onto all of the
          -- subtrees (referenced by cs) and cause a space leak.
          (elemsToWhnf sortedVersions `seq`
             W.mapWeightsWithKey (\k w -> weights k ++ w) cs)
    go x                            = x

addWeight :: (PN -> [Ver] -> POption -> Weight) -> EndoTreeTrav d c
addWeight f = addWeights [f]

version :: POption -> Ver
version (POption (I v _) _) = v

-- | Prefer to link packages whenever possible.
preferLinked :: EndoTreeTrav d c
preferLinked = addWeight (const (const linked))
  where
    linked (POption _ Nothing)  = 1
    linked (POption _ (Just _)) = 0

-- Works by setting weights on choice nodes. Also applies stanza preferences.
preferPackagePreferences :: (PN -> PackagePreferences) -> EndoTreeTrav d c
preferPackagePreferences pcs =
    preferPackageStanzaPreferences pcs .
    -- Each package is assigned a list of weights (currently three of them),
    -- and options are ordered by comparison of these lists.
    --
    -- The head of the list (and thus the top priority for ordering)
    -- is whether the package version is "preferred"
    -- (https://hackage.haskell.org/packages/preferred-versions).
    --
    -- The next two elements depend on 'PackagePreferences'.
    -- For 'PreferInstalled' they are whether the version is installed (0 or 1)
    -- and how close is the version to the latest one (between 0.0 and 1.0).
    -- For 'PreferLatest' the weights are the same, but swapped, so that
    -- ordering considers how new is the package first.
    -- For 'PreferOldest' one weight measures how close is the version to the
    -- the oldest one possible (between 0.0 and 1.0) and another checks whether
    -- the version is installed (0 or 1).
    addWeights [
          \pn _  opt -> preferred pn opt
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> installed opt
                          PreferLatest    -> latest vs opt
                          PreferOldest    -> oldest vs opt
        , \pn vs opt -> case preference pn of
                          PreferInstalled -> latest vs opt
                          PreferLatest    -> installed opt
                          PreferOldest    -> installed opt
        ]
  where
    -- Prefer packages with higher version numbers over packages with
    -- lower version numbers.
    latest :: [Ver] -> POption -> Weight
    latest sortedVersions opt =
      let l = length sortedVersions
          index = fromMaybe l $ L.findIndex (<= version opt) sortedVersions
      in  fromIntegral index / fromIntegral l

    -- Prefer packages with lower version numbers over packages with
    -- higher version numbers.
    oldest :: [Ver] -> POption -> Weight
    oldest sortedVersions opt = 1 - latest sortedVersions opt

    preference :: PN -> InstalledPreference
    preference pn =
      let PackagePreferences _ ipref _ = pcs pn
      in  ipref

    -- | Prefer versions satisfying more preferred version ranges.
    preferred :: PN -> POption -> Weight
    preferred pn opt =
      let PackagePreferences vrs _ _ = pcs pn
      in fromIntegral . negate . L.length $
         L.filter (flip checkVR (version opt)) vrs

    -- Prefer installed packages over non-installed packages.
    installed :: POption -> Weight
    installed (POption (I _ (Inst _)) _) = 0
    installed _                          = 1

-- | Traversal that tries to establish package stanza enable\/disable
-- preferences. Works by reordering the branches of stanza choices.
-- Note that this works on packages lower in the path as well as at the top level.
-- This is because stanza preferences apply to local packages only
-- and for local packages, a single version is fixed, which means
-- (for now) that all stanza preferences must be uniform at all levels.
-- Further, even when we can have multiple versions of the same package,
-- the build plan will be more efficient if we can attempt to keep
-- stanza preferences aligned at all levels.
preferPackageStanzaPreferences :: (PN -> PackagePreferences) -> EndoTreeTrav d c
preferPackageStanzaPreferences pcs = go
  where
    go (SChoiceF qsn@(SN (Q _pp pn) s) rdm gr _tr ts)
      | enableStanzaPref pn s =
          -- move True case first to try enabling the stanza
          let ts' = W.mapWeightsWithKey (\k w -> weight k : w) ts
              weight k = if k then 0 else 1
          -- defer the choice by setting it to weak
          in  SChoiceF qsn rdm gr (WeakOrTrivial True) ts'
    go x = x

    enableStanzaPref :: PN -> OptionalStanza -> Bool
    enableStanzaPref pn s =
      let PackagePreferences _ _ spref = pcs pn
      in  s `elem` spref

-- | Helper function that tries to enforce a single package constraint on a
-- given instance for a P-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintP :: forall d c. QPN
                          -> ConflictSet
                          -> I
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintP qpn c i (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go i prop
    else r
  where
    go :: I -> PackageProperty -> Tree d c
    go (I v _) (PackagePropertyVersion vr)
        | checkVR vr v  = r
        | otherwise     = Fail c (GlobalConstraintVersion vr src)
    go _       PackagePropertyInstalled
        | instI i       = r
        | otherwise     = Fail c (GlobalConstraintInstalled src)
    go _       PackagePropertySource
        | not (instI i) = r
        | otherwise     = Fail c (GlobalConstraintSource src)
    go _       _        = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintF :: forall d c. QPN
                          -> Flag
                          -> ConflictSet
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintF qpn f c b' (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go prop
    else r
  where
    go :: PackageProperty -> Tree d c
    go (PackagePropertyFlags fa) =
        case lookupFlagAssignment f fa of
          Nothing            -> r
          Just b | b == b'   -> r
                 | otherwise -> Fail c (GlobalConstraintFlag src)
    go _                             = r

-- | Helper function that tries to enforce a single package constraint on a
-- given flag setting for an F-node. Translates the constraint into a
-- tree-transformer that either leaves the subtree untouched, or replaces it
-- with an appropriate failure node.
processPackageConstraintS :: forall d c. QPN
                          -> OptionalStanza
                          -> ConflictSet
                          -> Bool
                          -> LabeledPackageConstraint
                          -> Tree d c
                          -> Tree d c
processPackageConstraintS qpn s c b' (LabeledPackageConstraint (PackageConstraint scope prop) src) r =
    if constraintScopeMatches scope qpn
    then go prop
    else r
  where
    go :: PackageProperty -> Tree d c
    go (PackagePropertyStanzas ss) =
        if not b' && s `elem` ss then Fail c (GlobalConstraintFlag src)
                                 else r
    go _                               = r

-- | Traversal that tries to establish various kinds of user constraints. Works
-- by selectively disabling choices that have been ruled out by global user
-- constraints.
enforcePackageConstraints :: M.Map PN [LabeledPackageConstraint]
                          -> EndoTreeTrav d c
enforcePackageConstraints pcs = go
  where
    go (PChoiceF qpn@(Q _ pn) rdm gr                    ts) =
      let c = varToConflictSet (P qpn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ (POption i _) -> foldl (\ h pc -> h . processPackageConstraintP qpn c i pc)
                                       id
                                       (M.findWithDefault [] pn pcs)
      in PChoiceF qpn rdm gr        (W.mapWithKey g ts)
    go (FChoiceF qfn@(FN qpn@(Q _ pn) f) rdm gr tr m d ts) =
      let c = varToConflictSet (F qfn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintF qpn f c b pc)
                           id
                           (M.findWithDefault [] pn pcs)
      in FChoiceF qfn rdm gr tr m d (W.mapWithKey g ts)
    go (SChoiceF qsn@(SN qpn@(Q _ pn) f) rdm gr tr   ts) =
      let c = varToConflictSet (S qsn)
          -- compose the transformation functions for each of the relevant constraint
          g = \ b -> foldl (\ h pc -> h . processPackageConstraintS qpn f c b pc)
                           id
                           (M.findWithDefault [] pn pcs)
      in SChoiceF qsn rdm gr tr     (W.mapWithKey g ts)
    go x = x

-- | Transformation that tries to enforce the rule that manual flags can only be
-- set by the user.
--
-- If there are no constraints on a manual flag, this function prunes all but
-- the default value. If there are constraints, then the flag is allowed to have
-- the values specified by the constraints. Note that the type used for flag
-- values doesn't need to be Bool.
--
-- This function makes an exception for the case where there are multiple goals
-- for a single package (with different qualifiers), and flag constraints for
-- manual flag x only apply to some of those goals. In that case, we allow the
-- unconstrained goals to use the default value for x OR any of the values in
-- the constraints on x (even though the constraints don't apply), in order to
-- allow the unconstrained goals to be linked to the constrained goals. See
-- https://github.com/haskell/cabal/issues/4299. Removing the single instance
-- restriction (SIR) would also fix #4299, so we may want to remove this
-- exception and only let the user toggle manual flags if we remove the SIR.
--
-- This function does not enforce any of the constraints, since that is done by
-- 'enforcePackageConstraints'.
enforceManualFlags :: M.Map PN [LabeledPackageConstraint] -> EndoTreeTrav d c
enforceManualFlags pcs = go
  where
    go (FChoiceF qfn@(FN (Q _ pn) fn) rdm gr tr Manual d ts) =
        FChoiceF qfn rdm gr tr Manual d $
          let -- A list of all values specified by constraints on 'fn'.
              -- We ignore the constraint scope in order to handle issue #4299.
              flagConstraintValues :: [Bool]
              flagConstraintValues =
                  [ flagVal
                  | let lpcs = M.findWithDefault [] pn pcs
                  , (LabeledPackageConstraint (PackageConstraint _ (PackagePropertyFlags fa)) _) <- lpcs
                  , (fn', flagVal) <- unFlagAssignment fa
                  , fn' == fn ]

              -- Prune flag values that are not the default and do not match any
              -- of the constraints.
              restrictToggling :: Eq a => a -> [a] -> a -> Tree d c -> Tree d c
              restrictToggling flagDefault constraintVals flagVal r =
                  if flagVal `elem` constraintVals || flagVal == flagDefault
                  then r
                  else Fail (varToConflictSet (F qfn)) ManualFlag

      in W.mapWithKey (restrictToggling d flagConstraintValues) ts
    go x                                                            = x

-- | Require installed packages.
requireInstalled :: (PN -> Bool) -> EndoTreeTrav d c
requireInstalled p = go
  where
    go (PChoiceF v@(Q _ pn) rdm gr cs)
      | p pn      = PChoiceF v rdm gr (W.mapWithKey installed cs)
      | otherwise = PChoiceF v rdm gr                         cs
      where
        installed (POption (I _ (Inst _)) _) x = x
        installed _ _ = Fail (varToConflictSet (P v)) CannotInstall
    go x          = x

-- | Avoid reinstalls.
--
-- This is a tricky strategy. If a package version is installed already and the
-- same version is available from a repo, the repo version will never be chosen.
-- This would result in a reinstall (either destructively, or potentially,
-- shadowing). The old instance won't be visible or even present anymore, but
-- other packages might have depended on it.
--
-- TODO: It would be better to actually check the reverse dependencies of installed
-- packages. If they're not depended on, then reinstalling should be fine. Even if
-- they are, perhaps this should just result in trying to reinstall those other
-- packages as well. However, doing this all neatly in one pass would require to
-- change the builder, or at least to change the goal set after building.
avoidReinstalls :: (PN -> Bool) -> EndoTreeTrav d c
avoidReinstalls p = go
  where
    go (PChoiceF qpn@(Q _ pn) rdm gr cs)
      | p pn      = PChoiceF qpn rdm gr disableReinstalls
      | otherwise = PChoiceF qpn rdm gr cs
      where
        disableReinstalls =
          let installed = [ v | (_, POption (I v (Inst _)) _, _) <- W.toList cs ]
          in  W.mapWithKey (notReinstall installed) cs

        notReinstall vs (POption (I v InRepo) _) _ | v `elem` vs =
          Fail (varToConflictSet (P qpn)) CannotReinstall
        notReinstall _ _ x =
          x
    go x          = x

-- | Require all packages to be mentioned in a constraint or as a goal.
onlyConstrained :: (PN -> Bool) -> EndoTreeTrav d QGoalReason
onlyConstrained p = go
  where
    go (PChoiceF v@(Q _ pn) _ gr _) | not (p pn)
      = FailF
        (varToConflictSet (P v) `CS.union` goalReasonToConflictSetWithConflict v gr)
        NotExplicit
    go x
      = x

-- | Sort all goals using the provided function.
sortGoals :: (Variable QPN -> Variable QPN -> Ordering) -> EndoTreeTrav d c
sortGoals variableOrder = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.sortByKeys goalOrder xs)
    go x                    = x

    goalOrder :: Goal QPN -> Goal QPN -> Ordering
    goalOrder = variableOrder `on` (varToVariable . goalToVar)

    varToVariable :: Var QPN -> Variable QPN
    varToVariable (P qpn)                    = PackageVar qpn
    varToVariable (F (FN qpn fn))     = FlagVar qpn fn
    varToVariable (S (SN qpn stanza)) = StanzaVar qpn stanza

-- | Reduce the branching degree of the search tree by removing all choices
-- after the first successful choice at each level. The returned tree is the
-- minimal subtree containing the path to the first backjump.
pruneAfterFirstSuccess :: EndoTreeTrav d c
pruneAfterFirstSuccess = go
  where
    go (PChoiceF qpn rdm gr       ts) = PChoiceF qpn rdm gr       (W.takeUntil active ts)
    go (FChoiceF qfn rdm gr w m d ts) = FChoiceF qfn rdm gr w m d (W.takeUntil active ts)
    go (SChoiceF qsn rdm gr w     ts) = SChoiceF qsn rdm gr w     (W.takeUntil active ts)
    go x                              = x

-- | Always choose the first goal in the list next, abandoning all
-- other choices.
--
-- This is unnecessary for the default search strategy, because
-- it descends only into the first goal choice anyway,
-- but may still make sense to just reduce the tree size a bit.
firstGoal :: EndoTreeTrav d c
firstGoal = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.firstOnly xs)
    go x                    = x
    -- Note that we keep empty choice nodes, because they mean success.

-- | Transformation that tries to make a decision on base as early as
-- possible by pruning all other goals when base is available. In nearly
-- all cases, there's a single choice for the base package. Also, fixing
-- base early should lead to better error messages.
preferBaseGoalChoice :: EndoTreeTrav d c
preferBaseGoalChoice = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.filterIfAnyByKeys isBase xs)
    go x                    = x

    isBase :: Goal QPN -> Bool
    isBase (Goal (P (Q _pp pn)) _) = unPN pn == "base"
    isBase _                       = False

-- | Deal with setup and build-tool-depends dependencies after regular dependencies,
-- so we will link setup/exe dependencies against package dependencies when possible
deferSetupExeChoices :: EndoTreeTrav d c
deferSetupExeChoices = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.preferByKeys noSetupOrExe xs)
    go x                    = x

    noSetupOrExe :: Goal QPN -> Bool
    noSetupOrExe (Goal (P (Q (PackagePath _ns (QualSetup _)) _)) _) = False
    noSetupOrExe (Goal (P (Q (PackagePath _ns (QualExe _ _)) _)) _) = False
    noSetupOrExe _                                                  = True

-- | Transformation that tries to avoid making weak flag choices early.
-- Weak flags are trivial flags (not influencing dependencies) or such
-- flags that are explicitly declared to be weak in the index.
deferWeakFlagChoices :: EndoTreeTrav d c
deferWeakFlagChoices = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.prefer noWeakFlag (P.prefer noWeakStanza xs))
    go x                    = x

    noWeakStanza :: Tree d c -> Bool
    noWeakStanza (SChoice _ _ _ (WeakOrTrivial True)   _) = False
    noWeakStanza _                                        = True

    noWeakFlag :: Tree d c -> Bool
    noWeakFlag (FChoice _ _ _ (WeakOrTrivial True) _ _ _) = False
    noWeakFlag _                                          = True

-- | Transformation that prefers goals with lower branching degrees.
--
-- When a goal choice node has at least one goal with zero or one children, this
-- function prunes all other goals. This transformation can help the solver find
-- a solution in fewer steps by allowing it to backtrack sooner when it is
-- exploring a subtree with no solutions. However, each step is more expensive.
preferReallyEasyGoalChoices :: EndoTreeTrav d c
preferReallyEasyGoalChoices = go
  where
    go (GoalChoiceF rdm xs) = GoalChoiceF rdm (P.filterIfAny zeroOrOneChoices xs)
    go x                    = x

-- | Monad used internally in enforceSingleInstanceRestriction
--
-- For each package instance we record the goal for which we picked a concrete
-- instance. The SIR means that for any package instance there can only be one.
type EnforceSIR = Reader (Map (PI PN) QPN)

-- | Enforce ghc's single instance restriction
--
-- From the solver's perspective, this means that for any package instance
-- (that is, package name + package version) there can be at most one qualified
-- goal resolving to that instance (there may be other goals _linking_ to that
-- instance however).
enforceSingleInstanceRestriction :: Tree d c -> Tree d c
enforceSingleInstanceRestriction = (`runReader` M.empty) . go
  where
    go :: Tree d c -> EnforceSIR (Tree d c)

    -- We just verify package choices.
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> traverse go ts
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> traverse go ts
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    -- The check proper
    goP :: QPN -> POption -> EnforceSIR (Tree d c) -> EnforceSIR (Tree d c)
    goP qpn@(Q _ pn) (POption i linkedTo) r = do
      let inst = PI pn i
      env <- ask
      case (linkedTo, M.lookup inst env) of
        (Just _, _) ->
          -- For linked nodes we don't check anything
          r
        (Nothing, Nothing) ->
          -- Not linked, not already used
          local (M.insert inst qpn) r
        (Nothing, Just qpn') -> do
          -- Not linked, already used. This is an error
          return $ Fail (CS.union (varToConflictSet (P qpn)) (varToConflictSet (P qpn'))) MultipleInstances

-- TODO: probably map of map instead of map would help trim things more.  Can
-- categorize map of what to trim in addition to what to select, with another
-- layer.

-- | Monad used internally in 'enforceArtifactRequirements'.
--
-- Track which 'I's / 'POptions' and values for other variables would be chosen
-- up to our location if this path in the tree were taken.  That lets us lookup
-- dependencies and consult the index to retrieve the 'PInfo' of the
-- dependencies to see what build artifacts they provide.
--
-- Also track choices (flag and stanza choices) that we know would introduce a
-- dependency between two solved package options with conflicting build artifact
-- requirements and availability.
type EnforceAR = Reader (Assignment, ARUndeterminedDeps)

-- | TODO doc
--
-- Introducing package, required artifacts.
--
-- TODO: left-biased
data ARUndeterminedDeps
  = ARDeps ARReveal (Map ARChoice ARUndeterminedDeps)
  deriving (Eq)

instance Semigroup ARUndeterminedDeps where
  ARDeps ar ad <> ARDeps br bd = ARDeps (ar <> br) (M.unionWith (<>) ad bd)

instance Monoid ARUndeterminedDeps where
  mempty = ARDeps mempty M.empty

-- | TODO doc
--
-- ARPackage artifactselection is artifacts required, which if not provided
-- means error.
--
-- TODO: then ARFlag being the opposite, that is, what would instead of what
-- would not cause the error, would be inconsistent.  But if ARflag etc. is
-- consistent, then ARChoice is not a choice made but a breaking choice.
-- Probably the latter, to be consistent.
-- Actually former but not ArtifactSelection represents what *not* to choose
-- oslt.
--
-- TODO: left-biased
data ARChoice = ARPackage QPN ArtifactSelection | ARFlag QFN Bool | ARStanza QSN
  deriving (Eq, Ord)

-- | TODO doc
--
-- Introducing package, required artifacts.
data ARReveal
  = ARReveal (Maybe ARFailure)
  deriving (Eq)

instance Semigroup ARReveal where
  ARReveal Nothing <> b = b
  a@(ARReveal (Just _)) <> _ = a

instance Monoid ARReveal where
  mempty = ARReveal Nothing

-- | TODO doc
type ARFailure = (ConflictSet, FailReason)

enforceArtifactRequirements :: Index -> Tree d c -> Tree d c
enforceArtifactRequirements idx = (`runReader` initialTracking) . go
  where
    initialTracking :: (Assignment, ARUndeterminedDeps)
    initialTracking = (A M.empty M.empty M.empty, mempty)

    -- For all child nodes, note that we made a choice that brought about any
    -- conflict.
    failingChoice :: CS.ConflictSet -> ARUndeterminedDeps -> ARUndeterminedDeps
    failingChoice annotation (ARDeps failure choices) = ARDeps failure' choices'
      where
        (ARReveal arr) = failure
        failure' = ARReveal $ (\(cs, fr) -> (annotation `CS.union` cs, fr)) <$> arr
        choices' = failingChoice annotation <$> choices

    -- TODO: docs; depending QPN and required arts.
    -- TODO: this lets us build a tree that lets us inspect the outcome of
    -- certain choices, to see whether they introduce _.  Note that the tree
    -- represents what would happen _if_ we made certain choices, with respect
    -- to our check.  That is, no need to check a qpn that _.
    -- TODO: doc: expand the conditional tree.
    depToAR :: Assignment -> QPN -> ArtifactSelection -> FlaggedDep QPN -> ARUndeterminedDeps
    depToAR assn@(A _pa fa _sa) qpn requiredArts (Flagged qfn _fInfo trueDeps falseDeps)
      = let
          flagAR      b     = ARFlag qfn b
          failing           = failingChoice (CS.singleton $ F qfn)
          conflicting True  = failing $ foldMap (depToAR assn qpn requiredArts) trueDeps
          conflicting False = failing $ foldMap (depToAR assn qpn requiredArts) falseDeps
        in
          case M.lookup qfn fa of
            (Just b)  -> conflicting b
            (Nothing) -> ARDeps mempty (M.fromList [(flagAR b, conflicting b) | b <- [True, False]])
    depToAR assn@(A _pa _fa sa) qpn requiredArts (Stanza qsn trueDeps)
      = let
          stanzaAR    = ARStanza qsn
          failing     = failingChoice (CS.singleton $ S qsn)
          conflicting = failing $ foldMap (depToAR assn qpn requiredArts) trueDeps
        in
          case M.lookup qsn sa of
            (Just False) -> mempty
            (Just True)  -> conflicting
            (Nothing)    -> ARDeps mempty (M.singleton stanzaAR conflicting)
    depToAR _ qpn requiredArts (Simple (LDep _dr (Dep (PkgComponent dep _) _ci)) _comp)
      = let
          rdepAR providedArts      = ARPackage dep providedArts
          conflicting providedArts = ARDeps (ARReveal $ Just (cs, fr providedArts)) M.empty
          cs                       = cps [qpn, dep]
          fr providedArts          = MissingArtifacts $ requiredArts `artsDifference` providedArts
          cps qpns                 = foldr CS.union CS.empty . map (CS.singleton . P) $ qpns

          toAs             = ArtifactSelection . S.fromList
          choices          = M.fromList $
            [ (rdepAR providedArts, conflicting providedArts)
            | providedArts <- asSelections
            , not $ requiredArts `artsSubsetOf` providedArts
            ]
        in ARDeps mempty choices
    depToAR _ qpn requiredArts (Simple (LDep dr _) comp) = mempty

    powerlist :: [a] -> [[a]]
    powerlist []     = [[]]
    powerlist (x:xs) = [zs | ys <- powerlist xs, zs <- [ys, x:ys]]

    asSelections :: [ArtifactSelection]
    asSelections = toAs <$> powerlist asKinds
      where
        toAs = ArtifactSelection . S.fromList
        asKinds = let ArtifactSelection asSet = allArtifacts in S.toList asSet

    compl :: ARChoice -> [ARChoice]
    compl (ARFlag    qfn b)            = [ARFlag qfn b' | b' <- [True, False], b' /= b]
    compl (ARStanza  qsn)              = []
    compl (ARPackage qpn providedArts) = [ARPackage qpn as | as <- asSelections, as /= providedArts]

    -- TODO: doc
    reduceARDeps :: ARChoice -> ARUndeterminedDeps -> ARUndeterminedDeps
    reduceARDeps choice _arDeps@(ARDeps arr choices) =
      case M.lookup choice choices of
        (Nothing)                               ->
          ARDeps arr (reduceARDeps choice <$> choices)
        (Just arDeps'@(ARDeps arr' choices')) ->
          case arr of
            (ARReveal (Just _)) ->
              ARDeps (arr <> arr') (reduceARDeps choice <$> (trim $ M.delete choice choices <> choices'))
            _                   ->
              arDeps'
      where
        trim = foldr (\x acc -> M.delete x . acc) id $ compl choice

    -- Selecting a package, flag, or stanza can introduce a missing artifact
    -- condition.  Check each possible location.
    go :: Tree d c -> EnforceAR (Tree d c)
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> sequenceA (W.mapWithKey (goF qfn) (fmap go ts))
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> sequenceA (W.mapWithKey (goS qsn) (fmap go ts))
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    {-
    -- If we are trying an option for a package, check reverse dependencies and
    -- dependencies in case we introduced a missing artifact condition, and
    -- also track what future choices (e.g. flag or stanza choices) would also
    -- introduce such a condition (e.g. in case we know that a conditional
    -- dependency would cause this violation but we haven't tried the flag that
    -- enables it yet).
    -}

    -- TODO: doc.
    arGuard :: CS.ConflictSet -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    arGuard annotation r = do
      -- Make sure we don't detect a conflict.
      (_assn, arDeps) <- ask
      case arDeps of
        (ARDeps (ARReveal (Just failure) ) _choices) -> do
          -- (Redundantly ensure that our choice is part of the conflict set.)
          let
            (cs, fr) = failure
            cs' = cs `CS.union` annotation
          return $ Fail cs' fr
        _ -> do
          r

    goP :: QPN -> POption -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goP qpn@(Q _ pn) (POption i _) r = do
      -- We are trying an instance.  Our job here is to detect when a choice
      -- would introduce a case of missing artifacts (one package depends on
      -- another that doesn't provide the build artifacts the first requires).
      -- We'll also track what assignments we would make if we took this path
      -- in the tree, and we'll also track which choices would introduce this
      -- condition.

      -- Get the dependencies of this 'I' and the build artifacts it provides
      -- and requires.
      let PInfo deps _exes _finfo _fr (providedArts, requiredArts) = idx ! pn ! i

      let
        qdeps = qualifyDeps (defaultQualifyOptions idx) qpn deps

        assign (A pa fa sa, arDeps) = (A (M.insert qpn i $ pa) fa sa, arDeps)
        merge  (assn,       arDeps) = (assn, arDeps <> foldMap (depToAR assn qpn requiredArts) qdeps)
        reduce (assn,       arDeps) = (assn, reduceARDeps (ARPackage qpn providedArts) $ arDeps)
      local (reduce . merge . assign) $ do
        arGuard (varToConflictSet (P qpn)) $ do
          r

    goF :: QFN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goF qfn@(FN qpn@(Q _ _pn) _f) b r = do
      let
        assign (A pa fa sa, arDeps) = (A pa (M.insert qfn b $ fa) sa, arDeps)
        reduce (assn,       arDeps) = (assn, reduceARDeps (ARFlag qfn b) $ arDeps)
      local (reduce . assign) $ do
        arGuard (varToConflictSet (F qfn)) $ do
          r

    goS :: QSN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goS qsn@(SN (Q _pp _pn) _s) b r = do
      let
        assign (A pa fa sa, arDeps) = (A pa fa (M.insert qsn b $ sa), arDeps)
        reduce (assn,       arDeps)
          | True <- b = (assn, reduceARDeps (ARStanza qsn) $ arDeps)
          | otherwise = (assn, arDeps)
      local (reduce . assign) $ do
        arGuard (varToConflictSet (S qsn)) $ do
          r

















{-
-- | Monad used internally in 'enforceArtifactRequirements'.
--
-- Track which 'I's / 'POptions' and values for other variables would be chosen
-- up to our location if this path in the tree were taken.  That lets us lookup
-- dependencies and consult the index to retrieve the 'PInfo' of the
-- dependencies to see what build artifacts they provide.
--
-- Also track choices (flag and stanza choices) that we know would introduce a
-- dependency between two solved package options with conflicting build artifact
-- requirements and availability.
type EnforceAR = Reader (Assignment, ARUndeterminedDeps)

-- | TODO doc
--
-- Introducing package, required artifacts.
--
-- TODO: left-biased
data ARUndeterminedDeps
  -- = ARDeps (Map ARChoice ARReveal)
  = ARDeps ARReveal (Map ARChoice ARUndeterminedDeps)
  deriving (Eq, Ord)

instance Semigroup ARUndeterminedDeps where
  ARDeps ar ad <> ARDeps br bd = ARDeps (ar <> br) (M.unionWith (<>) ad bd)

instance Monoid ARUndeterminedDeps where
  mempty = ARDeps mempty M.empty

-- | TODO doc
--
-- ARPackage artifactselection is artifacts required, which if not provided
-- means error.
--
-- TODO: then ARFlag being the opposite, that is, what would instead of what
-- would not cause the error, would be inconsistent.  But if ARflag etc. is
-- consistent, then ARChoice is not a choice made but a breaking choice.
-- Probably the latter, to be consistent.
-- Actually former but not ArtifactSelection represents what *not* to choose
-- oslt.
--
-- TODO: left-biased
data ARChoice = ARPackage QPN ArtifactSelection | ARFlag QFN Bool | ARStanza QSN
  deriving (Eq, Ord)

-- | TODO doc
--
-- Introducing package, required artifacts.
data ARReveal
  = ARReveal (Maybe ARFailure)  -- (S.Set (QPN, ArtifactSelection, ARUndeterminedDeps))
  deriving (Eq, Ord)

instance Semigroup ARUndeterminedDeps where
  ARReveal Nothing <> b = b
  a@(ARReveal (Just _)) <> _ = a

instance Monoid ARUndeterminedDeps where
  mempty = ARDeps mempty

-- | TODO doc
type ARFailure = (ConflictSet, FailReason)

-- | TODO: document.
-- TODO: 3 main steps remain:
-- 1) fix the arts powerset thing.
-- 2) reduce tree
-- 3) finish goF and goS
-- Then go and wrap up your second iteration!
enforceArtifactRequirements :: Index -> Tree d c -> Tree d c
enforceArtifactRequirements idx = (`runReader` initialTracking) . go
  where
    initialTracking :: (Assignment, ARUndeterminedDeps)
    initialTracking = (A M.empty M.empty M.empty, mempty)

    {-
    -- | Convert a dependency tree to our choice tree using our parent
    -- information.
    depsToAR :: (QPN, ArtifactSelection) -> FlaggedDeps -> ARUndeterminedDeps
    depsToAR (qpn, artsRequired) deps
      = foldr (\dep acc -> unionWith) M.empty . map depToAr $ deps
      where
        unARUndeterminedDeps (ARDeps choices) = choices
        depToAr :: FlaggedDep -> ARUndeterminedDeps
        depToAr (Simple _) = _
    depsToAR (qpn, artsRequired) (S)
    -}

    -- For all child nodes, note that we made a choice that brought about any
    -- conflict.
    failingChoice :: CS.ConflictSet -> ARUndeterminedDeps -> ARUndetermineddeps
    failingChoice annotation (ARDeps failure choices) = ARDeps failure' choices'
      where
        failure' = (\(cs, fr) -> (annotation `CS.union` cs, fr)) <$> failure
        choices' = failingChoice annotation <$> choices

    -- TODO: docs; depending QPN and required arts.
    -- TODO: this lets us build a tree that lets us inspect the outcome of
    -- certain choices, to see whether they introduce _.  Note that the tree
    -- represents what would happen _if_ we made certain choices, with respect
    -- to our check.  That is, no need to check a qpn that _.
    -- TODO: doc: expand the conditional tree.
    depToAR :: Assignment -> QPN -> ArtifactSelection -> FlaggedDep -> ARUndeterminedDeps
    depToAR assn@(A _pa fa _sa) qpn requiredArts (Flagged qfn _fInfo trueDeps falseDeps)
      = let
          flagAR      b     = ARFlag qfn b
          failing           = failingChoice (cs.singleton $ F qfn)
          conflicting True  = failing $ foldMap (depToAR assn qpn requiredArts) trueDeps
          conflicting False = failing $ foldMap (depToAR assn qpn requiredArts) falseDeps
        in
          case M.lookup qfn fa of
            (Just b)  -> conflicting b
            (Nothing) -> ARDeps mempty (M.fromList [(flagAR b, conflicting b) | b <- [True, False]])
    depToAR assn@(A _pa _fa sa) qpn requiredArts (Stanza qsn trueDeps)
      = let
          stanzaAR    = ARStanza qfn b
          failing     = failingChoice (cs.singleton $ S qsn)
          conflicting = failing $ foldMap (depToAR assn qpn requiredArts) trueDeps
        in
          case M.lookup qfsn fa of
            (Just False) -> mempty
            (Just True)  -> conflicting
            (Nothing)    -> ARDeps mempty (M.singleton stanzaAR conflicting)
    depToAR _ qpn requiredArts (Simple (LDep _dr (Dep (PkgComponent dep _) _ci)) _comp)
      = let
          rdepAR providedArts      = ARPackage dep providedArts
          conflicting providedArts = ARDeps (ARReveal $ Just (cs, fr providedArts)) M.empty
          cs                       = cps [qpn, dep]
          fr providedArts          = MissingArtifacts $ requiredArts `artsDifference` providedArts
          cps qpns                 = foldr CS.union CS.empty . map (CS.singleton . P) $ qpns

          powerlist []     = [[]]
          powerlist (x:xs) = [zs | ys <- powerlist xs, zs <- [ys, x:ys]]
          asKinds          = let ArtifactSelection asSet = allArtifacts in S.toList asSet
          asComplement as  = [as' | ass <- powerlist asKinds, as' <- ass, as' != as]
          choices          = M.fromList $
            [ (rdepAR providedArts, conflicting providedArts)
            | providedArts <- asComplement requiredArts
            ]
        in ARDeps mempty choices
    depToAR _ qpn requiredArts (Simple (LDep dr _) comp) = mempty

    -- TODO: doc
    reduceARDeps :: ARChoice -> ARUndeterminedDeps -> ARUndeterminedDeps
    reduceARDeps choice _arDeps@(ARDeps arr choices) =
      case M.lookup choice choices of
        (Nothing)                               ->
          ARDeps arr (reducedARDeps choice <$> choices)
        (Just arDeps'@(ARDeps arr' arChoices')) ->
          case arr of
            (ARReveal (Just _)) ->
              ARDeps (arr <> arr') (reduceARDeps choice <$> M.delete key arChoices <> arChoices')
            _                   ->
              arDeps'

    -- Selecting a package, flag, or stanza can introduce a missing artifact
    -- condition.  Check each possible location.
    go :: Tree d c -> EnforceAR (Tree d c)
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> sequenceA (W.mapWithKey (goF qfn) (fmap go ts))
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> sequenceA (W.mapWithKey (goS qsn) (fmap go ts))
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    {-
    -- If we are trying an option for a package, check reverse dependencies and
    -- dependencies in case we introduced a missing artifact condition, and
    -- also track what future choices (e.g. flag or stanza choices) would also
    -- introduce such a condition (e.g. in case we know that a conditional
    -- dependency would cause this violation but we haven't tried the flag that
    -- enables it yet).
    -}

    -- TODO: doc.
    arGuard :: CS.ConflictSet -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    arGuard annotation r = do
      -- Make sure we don't detect a conflict.
      (_assn, arDeps) <- ask
      case arDeps of
        (ARDeps (ARReveal (Just failure) ) _choices) -> do
          -- (Redundantly ensure that our choice is part of the conflict set.)
          let
            (cs, fr) = failure
            cs' = cs `CS.union` annotation
          return $ Fail cs' fr
        _ -> do
          r

    goP :: QPN -> POption -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goP qpn@(Q _ pn) (POption i _) r = do
      -- We are trying an instance.  Our job here is to detect when a choice
      -- would introduce a case of missing artifacts (one package depends on
      -- another that doesn't provide the build artifacts the first requires).
      -- We'll also track what assignments we would make if we took this path
      -- in the tree, and we'll also track which choices would introduce this
      -- condition.

      -- Get the dependencies of this 'I' and the build artifacts it provides
      -- and requires.
      let PInfo deps _exes _finfo _fr (providedArts, requiredArts) = idx ! pn ! i

      {-
      -- TODO: this would probably look better as a chain of ‘.’s with 1 local.
      -- Assign TODO.
      local (\(A pa fa sa, arDeps) -> (A (M.insert qpn i $ pa) fa sa, arDeps)) r
        -- Merge in TODO.
        local (\(assn, arDeps) -> (assn, arDeps <> foldMap (depToAR qpn requiredArts) deps)) $ do
          -- TODO: reduce tree.
          -- TODO: providedARts
          local (\(assn, arDeps) -> (assn, reduceARDeps assn (ARPackage QPN providedArts) $ arDeps))
            -- Make sure we don't detect a conflict.
            (_assn, arDeps) <- ask
            case ask of
              (ARDeps (ARReveal (Just failure) ) _choices) -> do
                -- (Redundantly ensure that our choice is part of the conflict set.)
                let
                  (cs, fr) = failure
                  cs' = cs `CS.union` (varToConflictSet (P qpn))
                return $ Fail cs' fr
              _ -> do
                r
      -}

      let
        assign (A pa fa sa, arDeps) = (A (M.insert qpn i $ pa) fa sa, arDeps)
        merge  (assn,       arDeps) = (assn, arDeps <> foldMap (depToAR assn qpn requiredArts) deps)
        reduce (assn,       arDeps) = (assn, reduceARDeps assn (ARPackage qpn providedArts) $ arDeps)
      local (reduce . merge . assign) $ do
        arGuard (varToConflictSet (P qpn)) $ do
          r

    goF :: QFN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goF qfn@(FN qpn@(Q _ _pn) _f) b r = do
      let
        assign (A pa fa sa, arDeps) = (A pa (M.insert qfn b $ fa) sa, arDeps)
        reduce (assn,       arDeps) = (assn, reduceARDeps assn (ARFlag qfn b) $ arDeps)
      local (reduce . assign) $ do
        arGuard (varToConflictSet (P qpn)) $ do
          r

    goS :: QSN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goS qsn@(SN (Q _pp _pn) _s) b r = do
      let
        assign (A pa fa sa, arDeps) = (A pa fa (M.insert qsn b $ sa), arDeps)
        reduce (assn,       arDeps) =
          | True <- b = (assn, reduceARDeps assn (ARStanza qsn) $ arDeps)
          | otherwise = (assn, arDeps)
      local (reduce . assign) $ do
        arGuard (varToConflictSet (P qpn)) $ do
          r
-}







{-
-- | Track failing flag choices for 'EnforceAR'.
type ARConflictingFlags = Map (QFN, Bool) (ConflictSet, FailReason)

-- | Track failing stanza choices for 'EnforceAR'.
type ARConflictingStanzas = Map (QSN, Bool) (ConflictSet, FailReason)

-- | TODO: document.
type ExceptFailure = Except (ConflictSet, FailReason)

-- | TODO: document.
enforceArtifactRequirements :: Index -> Tree d c -> Tree d c
enforceArtifactRequirements idx = (`runReader` initialTracking) . go
  where
    initialTracking :: (Assignment, ARConflictingFlags, ARConflictingStanzas)
    initialTracking = (A M.empty M.empty M.empty, M.empty, M.empty)

    {-
    getRdm :: Tree d c -> Maybe RevDepMap
    getRdm (PChoice _qpn rdm _gr _cs) = return $ rdm
    getRdm (FChoice _qfn rdm _y _t _m _d _ts) = return $ rdm
    getRdm (SChoice _qsn rdm _y _t _ts) = return $ rdm
    getRdm (GoalChoice rdm _ts) = return $ rdm
    getRdm (Done rdm _d) = return $ rdm
    getRdm (Fail _cs _fr) = Nothing
    -}

    getRdm :: Tree d c -> ExceptFailure RevDepMap
    getRdm (PChoice _qpn rdm _gr _cs) = return $ rdm
    getRdm (FChoice _qfn rdm _y _t _m _d _ts) = return $ rdm
    getRdm (SChoice _qsn rdm _y _t _ts) = return $ rdm
    getRdm (GoalChoice rdm _ts) = return $ rdm
    getRdm (Done rdm _d) = return $ rdm
    getRdm (Fail cs fr) = except . Left $ (cs, fr)

    embedFailure :: ExceptFailure (Tree d c) -> Identity (Tree d c)
    embedFailure = Identity . either (uncurry Fail) id . runExcept

    embedPure :: Identity a -> ExceptFailure a
    embedPure = except . Right . runIdentity

    -- TODO: maybe can remov arPure.
    --arPure :: EnforceAR a -> ReaderT (Assignment, ARConflictingFlags, ARConflictingStanzas) ExceptFailure a
    --arPure = mapReaderT embedPure

    -- Selecting a package, flag, or stanza can introduce a missing artifact
    -- condition.  Check each possible location.
    go :: Tree d c -> EnforceAR (Tree d c)
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> sequenceA (W.mapWithKey (goF qfn) (fmap go ts))
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> sequenceA (W.mapWithKey (goS qsn) (fmap go ts))
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    -- If we are trying an option for a package, check reverse dependencies and
    -- dependencies in case we introduced a missing artifact condition, and
    -- also track what future choices (e.g. flag or stanza choices) would also
    -- introduce such a condition (e.g. in case we know that a conditional
    -- dependency would cause this violation but we haven't tried the flag that
    -- enables it yet).
    goP :: QPN -> POption -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goP qpn@(Q _ pn) (POption i _) r = do
      -- Get our dependencies.
      let PInfo deps _exes _finfo _fr (providedArts, requiredArts) = idx ! pn ! i
      -- Assign this package choice in this path in the tree.
      local (\(A pa fa sa, cfl, cst) -> (A (M.insert qpn i $ pa) fa sa, cfl, cst)) $ do
        -- For each package choice we consider, we will look at the
        -- dependencies of this package choice, and categorize each dependency
        -- in one of 3 ways:
        -- 1) The dependency does not introduce a missing artifact condition,
        --    so we can ignore it.
        -- 2) The dependency is known given assignments we would make if we
        --    chose this path in the tree, and it would introduce a missing
        --    artifact condition, so replace this node in the tree with this
        --    discovery, so we don't produce a build plan we know would fail.
        -- 3) The dependency is unknown since it depends on flag or stanza
        --    choices we have not yet determined, so store it for validation
        --    later, when it will be determined.

        -- TODO: at each node, track set of pairs of ‘parent’ package (revdeps) and artifact requirements, etc.

        -- TODO: basically, for #3, it's a set of flag and stanza choices that
        -- leads to conflicts.  So, ignoring all trees that lead to valid
        -- results, track ... uh, figure this out.  Might look like converting
        -- that FlaggedDeps tree into your Reader env or something.
        undefined

        -- TODO: shortcut failures if use rdm, but actually can just queue
        -- unresolved dependencies.
        {-
        -- Shortcut failures, allowing us to more conveniently proceed so long
        -- as we haven't encountered a reason to fail of some sort.
        mapReaderT embedFailure $ do
          -- TODO: actually maybe you don't even need to look at rdm; just look
          -- at forward dependencies?  But if you end up needing rdm too, then
          -- you can use the below:
          {-
          --mapReaderT embedPure $ r
          --aoeu
          -- First collect all reverse dependencies for which this choice
          -- introduces a missing artifact condition.
          choiceNode <- mapReaderT embedPure r
          rdm <- getRdm choiceNode
          let
            qpnRevDeps :: [QPN]
            qpnRevDeps = [qpn' | Just revDeps <- M.lookup qpn rdm, (_comp, qpn') <- revDeps]
          -}
          undefined
        {-
        --mapReaderT embedFailure $ do
          --undefined
          -- First collect all reverse dependencies for which this choice
          -- introduces a missing artifact condition (note: 'extendOpen' appears
          -- to only extend the reverse dependency map for 'Simple' dependencies).
          --…

          -- Then collect all dependencies .  If we know of a flag or stanza
          -- choice that causes a missing artifact condition, but we haven't
          -- tried that flag or stanza choice yet, store 

          -- 2) Will check dependencies for flags we haven't solved yet, to store
          -- options we know will fail, if they are the next ones chosen.
          -- 1) Check reverse dependencies then dependencies, and concatenate the
          -- result.  Unchanged tree on success, 
          -- Check reverse dependencies: 
          --TODO
        -}
        -}

    -- 1) Check for the case where we solved both POption ends of a dependency
    -- with missing artifacts and now we're picking a flag that enables that
    -- dependency, and
    -- 2) Track what flag assignments we would have made if this path in the
    -- tree were chosen, so that other checks know what flags and conditional
    -- dependencies have already been decided.
    goF :: QFN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goF qfn@(FN qpn@(Q _ pn) f) b r = do
      (_, cfl, _) <- ask
      case M.lookup (qfn, b) cfl of
        (Just (cs, fr) ) -> do
          let cs' = cs `CS.union` (varToConflictSet (F qfn))  -- TODO: actually probably calculate this union earlier (in goP), not here, and then just use ‘cs’ on the next line.
          return $ Fail cs' fr
        (Nothing) -> do
          local (\(A pa fa sa, cfl, cst) -> (A pa (M.insert qfn b $ fa) sa, cfl, cst)) r

    -- 1) Check for the case where we solved both POption ends of a dependency
    -- with missing artifacts and now we're picking a stanza that enables that
    -- dependency, and
    -- 2) Track what flag assignments we would have made if this path in the
    -- tree were chosen, so that other checks know what flags and conditional
    -- dependencies have already been decided.
    goS :: QSN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goS qsn@(SN (Q _pp pn) s) b r = do
      (_, _, cst) <- ask
      case M.lookup (qsn, b) cst of
        (Just (cs, fr) ) -> do
          let cs' = cs `CS.union` (varToConflictSet (S qsn))  -- TODO: actually probably calculate this union earlier (in goP), not here, and then just use ‘cs’ on the next line.
          return $ Fail cs' fr
        (Nothing) -> do
          local (\(A pa fa sa, cfl, cst) -> (A pa fa (M.insert qsn b $ sa), cfl, cst)) r
-}































{-
-- | Monad used internally in 'enforceArtifactRequirements'.
--
-- Track which 'I's / 'POptions' and values for other variables would be chosen
-- up to our location if this path in the tree were taken.  That lets us lookup
-- dependencies and consult the index to retrieve the 'PInfo' of the
-- dependencies to see what build artifacts they provide.
--
-- Also track choices (flag and stanza choices) that we know would introduce a
-- dependency between two solved package options with conflicting build artifact
-- requirements and availability.
type EnforceAR = Reader (Assignment, ARConflictingFlags, ARConflictingStanzas)

-- | Track failing flag choices for 'EnforceAR'.
type ARConflictingFlags = Map (QFN, Bool) (ConflictSet, FailReason)

-- | Track failing stanza choices for 'EnforceAR'.
type ARConflictingStanzas = Map (QSN, Bool) (ConflictSet, FailReason)

-- | TODO: document.
type ExceptFailure = Except (ConflictSet, FailReason)

-- | TODO: document.
enforceArtifactRequirements :: Index -> Tree d c -> Tree d c
enforceArtifactRequirements idx = (`runReader` initialTracking) . go
  where
    initialTracking :: (Assignment, ARConflictingFlags, ARConflictingStanzas)
    initialTracking = (A M.empty M.empty M.empty, M.empty, M.empty)

    {-
    getRdm :: Tree d c -> Maybe RevDepMap
    getRdm (PChoice _qpn rdm _gr _cs) = return $ rdm
    getRdm (FChoice _qfn rdm _y _t _m _d _ts) = return $ rdm
    getRdm (SChoice _qsn rdm _y _t _ts) = return $ rdm
    getRdm (GoalChoice rdm _ts) = return $ rdm
    getRdm (Done rdm _d) = return $ rdm
    getRdm (Fail _cs _fr) = Nothing
    -}

    getRdm :: Tree d c -> ExceptFailure RevDepMap
    getRdm (PChoice _qpn rdm _gr _cs) = return $ rdm
    getRdm (FChoice _qfn rdm _y _t _m _d _ts) = return $ rdm
    getRdm (SChoice _qsn rdm _y _t _ts) = return $ rdm
    getRdm (GoalChoice rdm _ts) = return $ rdm
    getRdm (Done rdm _d) = return $ rdm
    getRdm (Fail cs fr) = except . Left $ (cs, fr)

    embedFailure :: ExceptFailure (Tree d c) -> Identity (Tree d c)
    embedFailure = Identity . either (uncurry Fail) id . runExcept

    embedPure :: Identity a -> ExceptFailure a
    embedPure = except . Right . runIdentity

    -- TODO: maybe can remov arPure.
    --arPure :: EnforceAR a -> ReaderT (Assignment, ARConflictingFlags, ARConflictingStanzas) ExceptFailure a
    --arPure = mapReaderT embedPure

    -- Selecting a package, flag, or stanza can introduce a missing artifact
    -- condition.  Check each possible location.
    go :: Tree d c -> EnforceAR (Tree d c)
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> sequenceA (W.mapWithKey (goF qfn) (fmap go ts))
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> sequenceA (W.mapWithKey (goS qsn) (fmap go ts))
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    -- If we are trying an option for a package, check reverse dependencies and
    -- dependencies in case we introduced a missing artifact condition, and
    -- also track what future choices (e.g. flag or stanza choices) would also
    -- introduce such a condition (e.g. in case we know that a conditional
    -- dependency would cause this violation but we haven't tried the flag that
    -- enables it yet).
    goP :: QPN -> POption -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goP qpn@(Q _ pn) (POption i _) r = do
      -- Get our dependencies.
      let PInfo deps _exes _finfo _fr (providedArts, requiredArts) = idx ! pn ! i
      -- Assign this package choice in this path in the tree.
      local (\(A pa fa sa, cfl, cst) -> (A (M.insert qpn i $ pa) fa sa, cfl, cst)) $ do
        -- For each package choice we consider, we will look at the
        -- dependencies of this package choice, and categorize each dependency
        -- in one of 3 ways:
        -- 1) The dependency does not introduce a missing artifact condition,
        --    so we can ignore it.
        -- 2) The dependency is known given assignments we would make if we
        --    chose this path in the tree, and it would introduce a missing
        --    artifact condition, so replace this node in the tree with this
        --    discovery, so we don't produce a build plan we know would fail.
        -- 3) The dependency is unknown since it depends on flag or stanza
        --    choices we have not yet determined, so store it for validation
        --    later, when it will be determined.

        -- TODO: at each node, track set of pairs of ‘parent’ package (revdeps) and artifact requirements, etc.

        -- TODO: basically, for #3, it's a set of flag and stanza choices that
        -- leads to conflicts.  So, ignoring all trees that lead to valid
        -- results, track ... uh, figure this out.  Might look like converting
        -- that FlaggedDeps tree into your Reader env or something.
        undefined

        -- TODO: shortcut failures if use rdm, but actually can just queue
        -- unresolved dependencies.
        {-
        -- Shortcut failures, allowing us to more conveniently proceed so long
        -- as we haven't encountered a reason to fail of some sort.
        mapReaderT embedFailure $ do
          -- TODO: actually maybe you don't even need to look at rdm; just look
          -- at forward dependencies?  But if you end up needing rdm too, then
          -- you can use the below:
          {-
          --mapReaderT embedPure $ r
          --aoeu
          -- First collect all reverse dependencies for which this choice
          -- introduces a missing artifact condition.
          choiceNode <- mapReaderT embedPure r
          rdm <- getRdm choiceNode
          let
            qpnRevDeps :: [QPN]
            qpnRevDeps = [qpn' | Just revDeps <- M.lookup qpn rdm, (_comp, qpn') <- revDeps]
          -}
          undefined
        {-
        --mapReaderT embedFailure $ do
          --undefined
          -- First collect all reverse dependencies for which this choice
          -- introduces a missing artifact condition (note: 'extendOpen' appears
          -- to only extend the reverse dependency map for 'Simple' dependencies).
          --…

          -- Then collect all dependencies .  If we know of a flag or stanza
          -- choice that causes a missing artifact condition, but we haven't
          -- tried that flag or stanza choice yet, store 

          -- 2) Will check dependencies for flags we haven't solved yet, to store
          -- options we know will fail, if they are the next ones chosen.
          -- 1) Check reverse dependencies then dependencies, and concatenate the
          -- result.  Unchanged tree on success, 
          -- Check reverse dependencies: 
          --TODO
        -}
        -}

    -- 1) Check for the case where we solved both POption ends of a dependency
    -- with missing artifacts and now we're picking a flag that enables that
    -- dependency, and
    -- 2) Track what flag assignments we would have made if this path in the
    -- tree were chosen, so that other checks know what flags and conditional
    -- dependencies have already been decided.
    goF :: QFN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goF qfn@(FN qpn@(Q _ pn) f) b r = do
      (_, cfl, _) <- ask
      case M.lookup (qfn, b) cfl of
        (Just (cs, fr) ) -> do
          let cs' = cs `CS.union` (varToConflictSet (F qfn))  -- TODO: actually probably calculate this union earlier (in goP), not here, and then just use ‘cs’ on the next line.
          return $ Fail cs' fr
        (Nothing) -> do
          local (\(A pa fa sa, cfl, cst) -> (A pa (M.insert qfn b $ fa) sa, cfl, cst)) r

    -- 1) Check for the case where we solved both POption ends of a dependency
    -- with missing artifacts and now we're picking a stanza that enables that
    -- dependency, and
    -- 2) Track what flag assignments we would have made if this path in the
    -- tree were chosen, so that other checks know what flags and conditional
    -- dependencies have already been decided.
    goS :: QSN -> Bool -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goS qsn@(SN (Q _pp pn) s) b r = do
      (_, _, cst) <- ask
      case M.lookup (qsn, b) cst of
        (Just (cs, fr) ) -> do
          let cs' = cs `CS.union` (varToConflictSet (S qsn))  -- TODO: actually probably calculate this union earlier (in goP), not here, and then just use ‘cs’ on the next line.
          return $ Fail cs' fr
        (Nothing) -> do
          local (\(A pa fa sa, cfl, cst) -> (A pa fa (M.insert qsn b $ sa), cfl, cst)) r
-}

{-
-- | Monad used internally in 'enforceArtifactRequirements'.
--
-- Track what 'I's / 'POption's would be chosen up to our location if this path
-- in the tree were taken.  That lets us lookup dependencies and consult the
-- index to retrieve the 'PInfo' of the dependencies to see what build
-- artifacts they provide.
type EnforceAR = Reader PAssignment
-- | TODO: document.
type ExceptFailure = Except (ConflictSet, FailReason)

-- | TODO: document.
-- TODO: flattenFlaggedDeps: please handle flags and stanzas before merging -
-- currently this just considers all possible dependencies (where do
-- dependencies excluded from flag / stanza options get trimmed?).  (Might
-- just test this preliminary version first.)
-- TODO: look for e.g. ‘avoid.*reinst’ to see how an option can be added (it's
-- already a param, but flags / options need to be added).
-- TODO: for the entire patchset, add a commit that handles the one diamond
-- regression, as our fixes make it fail as it should (that is, either adjust
-- the test or fix the bug it reveals) (probably better than changing the test
-- to skip now that it fails as it should).
enforceArtifactRequirements :: Index -> Tree d c -> Tree d c
enforceArtifactRequirements idx = (`runReader` M.empty) . go
  where
    getRdm :: Tree d c -> ExceptFailure RevDepMap
    getRdm (PChoice _qpn rdm _gr _cs) = return $ rdm
    getRdm (FChoice _qfn rdm _y _t _m _d _ts) = return $ rdm
    getRdm (SChoice _qsn rdm _y _t _ts) = return $ rdm
    getRdm (GoalChoice rdm _ts) = return $ rdm
    getRdm (Done rdm _d) = return $ rdm
    getRdm (Fail cs fr) = except . Left $ (cs, fr)

    embedFailure :: ExceptFailure (Tree d c) -> Identity (Tree d c)
    embedFailure = Identity . either (uncurry Fail) id . runExcept

    go :: Tree d c -> EnforceAR (Tree d c)

    -- We just verify package choices.
    go (PChoice qpn rdm gr cs) =
      PChoice qpn rdm gr <$> sequenceA (W.mapWithKey (goP qpn) (fmap go cs))
    go (FChoice qfn rdm y t m d ts) =
      FChoice qfn rdm y t m d <$> traverse go ts
    go (SChoice qsn rdm y t ts) =
      SChoice qsn rdm y t <$> traverse go ts
    go (GoalChoice rdm ts) =
      GoalChoice rdm <$> traverse go ts
    go x@(Fail _ _) = return x
    go x@(Done _ _) = return x

    -- The check proper
    goP :: QPN -> POption -> EnforceAR (Tree d c) -> EnforceAR (Tree d c)
    goP qpn@(Q _ pn) (POption i linkedTo) r = mapReaderT embedFailure $ do
      -- We are trying an instance.  Our job here is to detect when a choice
      -- would introduce a case of missing artifacts (one package depends on
      -- another that doesn't provide the build artifacts the first requires).
      -- Get the dependencies of this 'I' and the build artifacts it provides.
      let PInfo deps _exes _finfo _fr arts = idx ! pn ! i

      -- arts: assume that the build artifacts an option builds are also the
      -- build artifacts an option requires.
      let requiredArts = arts
      let providedArts = arts

      -- If we introduced a missing artifact condition by selecting this
      -- instance, it occurred in at least 1 of 2 places:
      -- 1) Our dependencies
      -- 2) Our reverse dependencies (what depends on us).

      -- First check reverse dependencies; look at the map of reverse
      -- dependencies calculated if we had made this choice.
      rdm <- lift $ getRdm <$> r

      let
        qpnDeps :: [QPN]
        qpnDeps = [dep | (LDep _ (Dep (PkgComponent dep _) _ci), _comp) <- flattenFlaggedDeps deps]
        qpnRevDeps :: [QPN]
        qpnRevDeps = [qpn' | Just revDeps <- M.lookup qpn rdm, (_comp, qpn') <- revDeps]

      -- Our depencies provide a selection of build artifacts.  Make sure it is
      -- a superset of the selection of build artifacts we require ('arts').
      -- Fail if for any instance we known we would try, it would fail to
      -- provide the build artifacts we require.

      -- Find all dependencies we've chosen in the path we are trying, to see
      -- if they would make this build plan fail.
      let ins = M.insert qpn i
      tryingChoices <- asks ins
      let 
        brokenDeps :: [(QPN, ArtifactSelection)]
        brokenDeps =
          [ (dep, depArts)
          | dep <- qpnDeps
          , Just depI <- M.lookup dep tryingChoices
          , let PInfo _ _ _ _ depArts = idx ! pn ! depI
          , requiredArts `artsSubsetOf` depArts
          , let missing = requiredArts `artsDifference` depArts
          ]
        -- Make sure we're not picking a dependency for other packages that
        -- require what we don't provide.
        brokenRevDeps :: [(QPN, ArtifactSelection)]
        brokenRevDeps =
          [ (revDep, missing)
          | revDep <- qpnRevDeps
          , Just revDepI <- M.lookup revDep tryingChoices
          , let PInfo _ _ _ _ revDepArts = idx ! pn ! revDepI
          , revDepArts `artsSubsetOf` providedArts
          , let missing = revDepArts `artsDifference` providedArts
          ]

        allBroken :: [(QPN, ArtifactSelection)]
        allBroken = brokenDeps ++ brokenRevDeps

      -- Make sure we have no broken deps or reverse deps.
      case allBroken of
        [] -> except . Right <$> local ins r  -- This package choice passed our test.
        _  ->
          let
            q2c = varToConflictSet . P
            cs = foldr (CS.union `on` q2c) qpn $ fst <$> allBroken
            fr = MissingArtifacts . mconcat $ snd <$> allBroken
          in lift . except . Left $ (cs, fr)
-}













{-
      -- Check through rdm that anything that depends on us does not require
      -- what we don't provide - through POption/Is map.
      --
      -- Also cheeck that we don't depend on anything already solved at this
      -- point to a package instance (IPI) that does not provide the build
      -- artifacts we require.
      -- TODO












-- | Monad used internally in enforceArtifactRequirements
--
-- Track would our path in the solver would _.
-- Used so that when we look at dependencies (both ways), we know which
-- instance is used to depend on us.

--
-- For each package instance we record the goal for which we picked a concrete
-- instance. The SIR means that for any package instance there can only be one.
--type EnforceSIR = Reader (Map (PI PN) QPN)
type EnforceSIR = Reader PAssignment
-- | Enforce artifact requirements.
--
-- <Purpose / higher-level description / inegration>
--
-- <What it does / how>: traverse the tree, and at each I (where a possible IPI
-- is chosen or tried), check whether we introducing missing required artifacts
-- in either of 2 possible locations: in dependencies or reverse dependencies.
--
-- In the case of reverse dependencies (what depends on the I we encounter),
-- check for missing artifacts in this connection.  In the case of
-- dependencies, keep track of the partially solved package assignment we would
-- get if we chose this path up to this, and look up the IPI chosen and its
-- provided artifacts and compare it with our own required artifacts, for each
-- of our dependencies for this I/POption (we can look up the dependencies
-- similar to how it's done with ‘extendOpen’ in the build phase).
enforceArtifactRequirements :: Blah Tree -> EndoTreeTrav d c























-- If any reveerse dependency requires artifacts that we don't provide, fail
--wiith missing artifacts.  (We assume ‘provides’ equavilent to ‘requires’.)
--
-- That is, we rule out installed packages as options if they don't provide
-- required artifacts, so that we then can only choose between any installed
-- packages that do or any available source packages where we can choose our
-- own (any) output.
--
-- EndoTrav
-- TODO: once this is done, revert out or remove the old buildPhase location
-- for this.
--aoeu :: 
-- Once an instance is chosen, then I think the RevDevMap of that tree has the
-- revdevmap you are interested in.  But double check extendOpen to see how it
-- is done.
-- TODO: probably map Is.
-- So here we handle Is, because this is where dependencies are added (in
-- either direction), originally by extendOpen in the build phase, where build artifacts could be missing.
-- So check 2 dependencies in separate places: anything that depends on this I itself, and also check I's dependencies in case
-- any of them have versions we already are trying to instantiate (through
-- extendOpen) but we now have additional requiremments that this previous
-- option we tried lack, so we'll have to look for other options back there if there is a valid solution.
-- Probably try to rephrase that to make it clearer; maybe somethhing like
-- ‘1) check anything (rdms) that depends on this new ‘I’ we are trying now
-- (although there may be future rdms on us, this new ‘I’, that we don't know of,
-- which brings us to the next location, #2); 2) check this new ‘I’'s dependencies, in a manner like the build phase's ‘extendOpen’,
-- in case it adds to any older ‘I’s we tried before.’
-- (example scenario: we require a partial artifact selection, and an available
-- IPI provides all, so its dependencies require all, although maybe another
-- source or IPI doesn't provide all so doesn't require all too, in case the
-- alternatiev option fails)
enforceArtifactRequirements :: EndoTreeTrav d c
enforceArtifactRequirements xs = go xs
  where
    go :: TreeF d c (Tree d c) -> TreeF d c (Tree d c)
    go (PChoiceF qpn@(Q _ pn) rdm x cs) =
      -- Check through rdm that anything that depends on us does not require
      -- what we don't provide - through POption/Is map.
      --
      -- Also cheeck that we don't depend on anything already solved at this
      -- point to a package instance (IPI) that does not provide the build
      -- artifacts we require.
      -- TODO
    go x = x







{-
    go :: TreeF d c (Tree d c) -> TreeF d c (Tree d c)
    go (PChoiceF qpn@(Q _ pn) rdm x cs) =
      let sortedVersions = L.sortBy (flip compare) $ L.map version (W.keys cs)
          weights k = [f pn sortedVersions k | f <- fs]

          elemsToWhnf :: [a] -> ()
          elemsToWhnf = foldr seq ()
      in  PChoiceF qpn rdm x
          -- Evaluate the children's versions before evaluating any of the
          -- subtrees, so that 'sortedVersions' doesn't hold onto all of the
          -- subtrees (referenced by cs) and cause a space leak.
          (elemsToWhnf sortedVersions `seq`
             W.mapWeightsWithKey (\k w -> weights k ++ w) cs)
    go x                            = x
-}
-}
