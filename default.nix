{
  lib,
  haskell,
  haskellPackages,
  less,
  makeWrapper,
}:
let
  dismas = lib.pipe (haskellPackages.callCabal2nix "dismas" (lib.cleanSource ./.) { }) (
    with haskell.lib.compose; [ dontHaddock ]
  );
in
dismas.overrideAttrs (attrs: {
  nativeBuildInputs = attrs.nativeBuildInputs ++ [ makeWrapper ];
  buildInputs = attrs.buildInputs ++ [ less ];
  postFixup = ''
    wrapProgram $out/bin/dismas --set PAGER "${less}/bin/less"
  '';
})
