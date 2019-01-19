{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  dependent-map-aeson = (
    with rec {
      dependent-map-aesonSource = pkgs.lib.cleanSource ../.;
      dependent-map-aesonBasic  = self.callCabal2nix "dependent-map-aeson" dependent-map-aesonSource { };
    };
    overrideCabal dependent-map-aesonBasic (old: {
    })
  );
}
