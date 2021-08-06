{ config, pkgs, lib, ... }:

let
  defaultPkgs = with pkgs; [
    any-nix-shell
    arandr
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    brave
    cachix
    clang
    coreutils
    dive
    docker-compose
    editorconfig-core-c
    exa
    fd
    fractal
    google-chrome
    gparted # partition manager
    graphviz
    kanshi
    librecad
    multilockscreen
    ncdu
    nixfmt
    nix-doc
    nix-index
    nodejs
    pandoc
    phantomjs2
    racket
    ripgrep
    rnix-lsp
    signal-desktop
    shellcheck
    slack
    sqlite
    stow
    tldr
    xclip
    zoom-us
  ];

  gnomePkgs = with pkgs.gnome3; [
    eog # image viewer
    evince # pdf reader
    gnome-calendar # calendar
  ];

  matePkgs = with pkgs.mate;
    [
      caja # file manager
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
    numix-cursor-theme
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
      ++ gnomePkgs ++ scripts ++ extraPkgs ++ matePkgs;

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

  xdg.configFile."kanshi/config".text = ''
    profile {
      output eDP-1 enable
    }
    profile {
      output eDP-1 disable
      output HDMI-A-2 enable
    }
  '';
}
