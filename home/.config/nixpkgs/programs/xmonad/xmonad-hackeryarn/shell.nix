{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    alsa-lib
    cabal-install
    ghc
    haskell-language-server
    haskellPackages.brittany
    pkgconfig
    x11
    xorg.libXScrnSaver
    xorg.libXrandr
  ];
}
