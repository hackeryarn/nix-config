{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.developPackage {
  name = "xmonad-hackeryarn";
  withHoogle = true;
  root =
    pkgs.nix-gitignore.gitignoreSourcePure [ "dist-newstyle" ".*#" ".git" ] ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      brittany
    ]);
}
