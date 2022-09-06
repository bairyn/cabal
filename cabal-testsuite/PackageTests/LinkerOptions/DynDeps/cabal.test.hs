import Test.Cabal.Prelude

-- This test shows how 2022-07-28 Archlinux Haskell with a standard ghc and
-- cabal-install fails to build e.g. even attoparsec.  Essentially, the system
-- packages strip away static libraries and files, and build with only dynamic
-- files.
--
-- (ghc-static provides its own custom packagedb location, in e.g.
-- /usr/lib/static-package.conf.d rather than /usr/lib/package.conf.d, which
-- cabal and ghc doesn't know about unless you add it with --package-db.  But
-- the haskell-* libraries build with flags like
-- "--enable-shared --enable-executabledynamic --disable-library-vanilla".)
--
-- Then a vanilla cabal build will see these packages are installed, but when
-- it's trying to build with a "ghc" that has "-static", itthinks the packages
-- installs provide the files, but whereas it would compile if only with
-- "-dynamic", it fails for "-static" with errors like the following:
--
-- > [1 of 1] Compiling Main             ( Main.hs, ../setup.dist/work/depender/dist/build/depender/depender-tmp/Main.o )
-- >
-- > Main.hs:3:1: error:
-- >     Could not find module `Dynamic'
-- >     There are files missing in the `dynamic-1.0' package,
-- >     try running 'ghc-pkg check'.
-- >     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- >   |
-- >   | import qualified Dynamic (number)
-- >   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--
-- (A workaround to the system "haskell-*" packages lacking static libraries
-- (ghc-static provides some, though) without a fixed solver is to use stack
-- for everything.)

-- Simulate the above scenario but in our framework (without a cabal with the
-- project-local build flags fix, the test should still pass but not pass the
-- correct dynamic vs static flags through, so on these old cabals this test
-- would pass where it should fail because dynamic is being built with static
-- options enabled too; however, with a new enough cabal but an old GHC, the
-- build artifacts won't be threaded through the IPIs, so it should still fail
-- with an older GHC).
main = do
    -- TODO: skip for < GHC 9.6; perhaps 9.6 will depend on a Cabal-syntax that
    -- provides the new IPI fields.
    -- TODO: skip for cabal < 3.9.
    cabalTest $ do
        env <- getTestEnv
        let
            dynamicArgs =
                [
                    "--enable-shared",
                    "--enable-executable-dynamic",
                    "--disable-library-vanilla",
                    "--disable-static",
                    "--disable-executable-static"
                ]
            staticArgs =
                [
                    "--enable-static"
                ]
        withDirectory "dynamic" $ do
            cabal "v2-configure" $ [] ++ dynamicArgs
            cabal "v2-build" []
            cabal "v2-install" $ ["--lib"] ++ dynamicArgs
            cabal "v2-sdist" []
        withDirectory "depender" $ do
            -- depender knows of the source package and the installed package.
            -- The installed package should only have dynamic files (.dyn_hi,
            -- .so), but not any static files (.a, .hi).  New ghc-pkg IPI file
            -- fields track these, so with a new GHC, a new cabal-install
            -- should reject the installed package while building the tree
            -- (reason: missing build artifacts) and instead choose the sdist
            -- (source package) so that it can figure out its own configuration
            -- flags.
            --
            -- (For instance, if you comment out the sdist references so that we
            -- only see the installed package, you should see an error message
            -- like this:)
            -- > Error: cabal: Could not resolve dependencies:
            -- > [__0] trying: depender-1.0 (user goal)
            -- > [__1] next goal: dynamic (dependency of depender)
            -- > [__1] rejecting: dynamic-1.0/installed-19c7c1e50b8f1e56115c4f668dfdadd7114fc2c7dad267c2df43028892cc0ff5 (missing build artifacts: static artifacts)
            -- > [__1] fail (backjumping, conflict set: depender, dynamic)
            -- > After searching the rest of the dependency tree exhaustively, these were the goals I've had most trouble fulfilling: depender (3), dynamic (2)
            cabal "v2-configure" $ [] ++ staticArgs
            cabal "v2-build" $ [] ++ ["--package-db=/home/bairyn/git/cabal/cabal-testsuite/PackageTests/LinkerOptions/DynDeps/cabal.dist/home/.cabal/store/ghc-9.4.2/package.db"]
