{ lib
, haskell
}:
lib.pipe
  (haskell.packages.ghc96.callCabal2nix "biblegateway" (lib.cleanSource ./.) { })
  [ haskell.lib.compose.dontHaddock ]

