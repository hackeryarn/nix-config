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
    calibre
    clang
    clj-kondo
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
    kanshi
    librecad
    multilockscreen
    ncdu
    nixfmt
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
    texlive.combined.scheme-full
    tldr
    # virt-manager
    xclip
    # zoom-us
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
    cabal-install
    hoogle
    haskell-language-server
    hlint
    brittany
    nix-tree
    cabal2nix
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

    autojump = {
      enable = true;
      # enableFishIntegration = true;
    };

    # broot = {
    #   enable = true;
    #   # enableFishIntegration = true;
    # };

    obs-studio = {
      enable = true;
      plugins = [ ];
    };

    htop = {
      enable = true;
      settings = {
        sort_direction = "descending";
        sort_key = "PERCENT_CPU";
      };
    };

    fzf = {
      enable = true;
      # enableFishIntegration = true;
    };

    browserpass = {
      enable = true;
      browsers = [ "firefox" "chrome" "chromium" ];
    };
    gpg.enable = true;
  };

  services = {
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