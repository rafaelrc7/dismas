{ pkgs ? import <nixpkgs> { }, devTools ? true }:
let
  haskell = pkgs.haskell.packages.ghc96.extend
    (final: prev: { biblegateway = pkgs.callPackage ./default.nix { }; });
in
haskell.shellFor {
  packages = p: [ p.biblegateway ];
  nativeBuildInputs =
    [ pkgs.zlib haskell.ghc haskell.cabal-install ] ++ pkgs.lib.optional devTools [
      pkgs.hlint
      pkgs.ormolu
      pkgs.gcc
      (haskell.ghc.withPackages (p: [ p.haskell-language-server p.haskell-dap p.haskell-debug-adapter p.ghci-dap ]))
    ];
}

