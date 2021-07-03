let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.developPackage {
  root =
    pkgs.nix-gitignore.gitignoreSourcePure [ "dist-newstyle" ".*#" ".git" ] ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      brittany
    ]);
}
