{ config, pkgs, lib, ... }:

let
  defaultPkgs = with pkgs; [
    any-nix-shell
    arandr
    brave
    cachix
    dive
    docker-compose
    exa
    fd
    fractal
    multilockscreen
    ncdu
    nixfmt
    nix-doc
    nix-index
    nodejs
    ripgrep
    rnix-lsp
    signal-desktop
    slack
    sqlite
    stow
    tldr
    xclip
    zoom
  ];

  gnomePkgs = with pkgs.gnome3; [
    eog # image viewer
    evince # pdf reader
    gnome-calendar # calendar
    nautilus # file manager
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    ghc
    cabal2nix
    cabal-install
    hoogle
    nix-tree
  ];

  polybarPkgs = with pkgs; [ font-awesome-ttf ];

  xmonadPkgs = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet
    nitrogen
    numix-cursor-theme
    xcape
    xorg.xkbcomp
    xorg.xmodmap
    xorg.xrandr
  ];

  extraPkgs = import ./packages;

  scripts = pkgs.callPackage ./scripts/default.nix { inherit config pkgs; };

in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = p: {
      nur = import (import pinned/nur.nix) { inherit pkgs; };
    };
  };

  imports = (import ./programs) ++ (import ./services) ++ [ (import ./themes) ];

  xdg.enable = true;

  home = {
    username = "hackeryarn";
    homeDirectory = "/home/hackeryarn";
    stateVersion = "21.05";

    packages = defaultPkgs ++ haskellPkgs ++ polybarPkgs ++ xmonadPkgs
      ++ gnomePkgs ++ scripts ++ extraPkgs;

    sessionVariables = {
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };
  systemd.user.startServices = "sd-switch";

  news.display = "silent";

  programs = {
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
      enableBashIntegration = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };
  };

  # Screenshots
  services = {
    flameshot.enable = true;
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
  };
}
