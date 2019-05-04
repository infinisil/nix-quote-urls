let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/5d09420337fb3792f6313ac45ecf921241129b2b";
    sha256 = "0rzrkv5zag4accn0wihfx6hhzh7zpphphpx6qhykl0d5wvb5m7xa";
  };
in
{ pkgs ? import nixpkgs {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc864.extend (self: super: {
    nix-quote-urls = self.callCabal2nix "nix-quote-urls" (lib.sourceByRegex ./. [
      "^\\src.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
    ]) {};

    hnix = hlib.overrideCabal super.hnix (old: {
      broken = false;
    });
  });
in hpkgs.nix-quote-urls // {
  inherit pkgs hpkgs;
}
