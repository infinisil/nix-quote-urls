with import ./. {};

hpkgs.shellFor rec {
  packages = p: [ hpkgs.nix-quote-urls ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
