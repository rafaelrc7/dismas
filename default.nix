{ lib
, haskell
, haskellPackages
}:
let hscompose = haskell.lib.compose;
in lib.pipe
  (haskellPackages.callCabal2nix "biblegateway" (lib.cleanSource ./.) { })
  [ hscompose.dontHaddock ]

