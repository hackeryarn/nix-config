{ pkgs ? import <nixpkgs> { } }:

let
  haskellPkgs = pkgs.haskellPackages.ghcWithPackages (hsPkgs:
    with hsPkgs; [
      xmonad
      xmonad-contrib
      xmonad-extras
      containers
      dbus
      cabal-install
      brittany
      haskell-language-server
    ]);
in pkgs.mkShell { buildInputs = [ haskellPkgs ]; }
