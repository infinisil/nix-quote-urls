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
    nix-quote-urls = hlib.overrideCabal (self.callCabal2nix "nix-quote-urls" (lib.sourceByRegex ./. [
      "^\\src.*$"
      "^\\app.*$"
      "^\\tests.*$"
      "^.*\\.cabal$"
      "^LICENSE$"
    ]) {}) {
      enableLibraryProfiling = false;
    };

    hnix = hlib.overrideCabal super.hnix (old: {
      enableLibraryProfiling = false;
      # https://github.com/haskell-nix/hnix/pull/498
      patches = [(pkgs.fetchpatch {
        name = "nixUri-span";
        url = "https://github.com/infinisil/hnix/commit/ad67c6bfaf9145d12e82055eb5ce37b1af691319.patch";
        sha256 = "19l4wcp5npll4gcz86fmfdhz5ax0w70286r6l33gq3yyz26iqcwg";
      })];
      broken = false;
    });
  });
in hpkgs.nix-quote-urls // {
  inherit pkgs hpkgs;
}
