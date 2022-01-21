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
    element-desktop
    google-chrome
    gparted # partition manager
    graphviz
    librecad
    multilockscreen
    ncdu
    nixfmt
    nix-doc
    nix-index
    nodejs
    pandoc
    phantomjs2
    pinentry-qt
    racket
    ripgrep
    rnix-lsp
    signal-desktop
    shellcheck
    slack
    sqlite
    stow
    texlive.combined.scheme-medium
    tldr
    xclip
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
    # nix-tree
  ];

  polybarPkgs = with pkgs; [ font-awesome-ttf ];

  xmonadPkgs = with pkgs; [
    networkmanager
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
    autojump = { enable = true; };

    broot = { enable = true; };

    htop = {
      enable = true;
      settings = {
        sort_direction = "descending";
        sort_key = "PERCENT_CPU";
      };
    };

    fzf = { enable = true; };

    browserpass = {
      enable = true;
      browsers = [ "firefox" "chrome" "chromium" ];
    };
  };

  services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };

    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
      enableSshSupport = true;
      defaultCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };
  };
}
