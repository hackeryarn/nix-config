{ config, pkgs, lib, ... }:

let
  defaultPkgs = with pkgs; [
    any-nix-shell
    arandr
    cachix
    docker-compose
    dive
    exa
    fd
    git
    ncdu
    nix-doc
    nix-index
    rnix-lsp
    ripgrep
    tldr
    xclip
    stow
    sqlite
    multilockscreen
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    brittany
    cabal2nix
    cabal-install
    ghc
    haskell-language-server
    hoogle
    nix-tree
  ];

  polybarPkgs = with pkgs; [
    font-awesome-ttf
  ];

  xmonadPkgs = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet
    nitrogen
    xcape
    xorg.xkbcomp
    xorg.xmodmap
    xorg.xrandr
  ];

  scripts = pkgs.callPackage ./scripts/default.nix { inherit config pkgs; };

in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  nixpkgs.config = {
    allowUnfree = true;
  };

  imports =  (import ./programs) ++ (import ./services);

  xdg.enable = true;

  home = {
    username = "artem";
    homeDirectory = "/home/artem";
    stateVersion = "21.05";

    packages = defaultPkgs ++ haskellPkgs ++ polybarPkgs ++ xmonadPkgs ++ scripts;

    sessionVariables = {
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };
  systemd.user.startServices = "sd-switch";

  news.display = "silent";

  programs = {
    emacs.enable = true;
    jq.enable = true;
    bat.enable = true;
    bash.enable = true;

    autojump = {
      enable = true;
      enableFishIntegration = true;
    };

    broot = {
      enable = true;
      enableFishIntegration = true;
    };

    htop = {
      enable = true;
      settings = {
        sort_direction = "descending";
        sort_key = "PERCENT_CPU";
      };
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
