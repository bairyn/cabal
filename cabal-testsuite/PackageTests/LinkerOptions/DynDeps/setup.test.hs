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
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "dynamic" $ do
            setup_install $
                [
                    "--enable-shared",
                    "--enable-executable-dynamic",
                    "--disable-library-vanilla",
                    "--disable-static",
                    "--disable-executable-static"
                ]
        withDirectory "depender" $ do
            setup_install ["--enable-static"]
            setup "clean" []
            ran <- runInstalledExe' "depender" []
            assertOutputContains "Dynamic's number is 3." ran
