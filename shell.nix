{ pkgs ? import <nixpkgs> { }, devTools ? true }:
let
  haskell = pkgs.haskellPackages.extend
    (final: prev: { biblegateway = pkgs.callPackage ./default.nix { }; });
in
haskell.shellFor {
  packages = p: [ p.biblegateway ];
  nativeBuildInputs = with pkgs;
    [ zlib ghc cabal-install ] ++ lib.optional devTools [
      hlint
      ormolu
      (ghc.withPackages (p: [ p.haskell-language-server p.haskell-dap p.haskell-debug-adapter p.ghci-dap ]))
      gcc
    ];
}

