{ lib
, haskell
}:
lib.pipe
  (haskell.packages.ghc910.callCabal2nix "biblegateway" (lib.cleanSource ./.) { })
  [ haskell.lib.compose.dontHaddock ]

