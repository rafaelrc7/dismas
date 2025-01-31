{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default-linux";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { config, pkgs, ... }:
        let haskellPackages = pkgs.haskell.packages.ghc96;
        in
        {
          devShells.default = import ./shell.nix { inherit pkgs haskellPackages; };

          packages = rec {
            default = biblegateway;
            biblegateway = pkgs.callPackage ./default.nix { inherit haskellPackages; };
          };

          apps.default = {
            type = "app";
            program = "${config.packages.biblegateway}/bin/biblegateway";
          };

          overlayAttrs = {
            inherit (config.packages) biblegateway;
          };

          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              stylish-haskell.enable = true;
            };
          };
        };
    };
}
