{ lib
, haskell
, less
, makeWrapper
}:
let
  biblegateway = lib.pipe
    (haskell.packages.ghc96.callCabal2nix "biblegateway" (lib.cleanSource ./.) { })
    (with haskell.lib.compose; [ dontHaddock ]);
in
  biblegateway.overrideAttrs (attrs: {
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ makeWrapper ];
    buildInputs = attrs.buildInputs ++ [ less ];
    postFixup = ''
      wrapProgram $out/bin/bg-rd --set PAGER "${less}/bin/less"
    '';
  })

