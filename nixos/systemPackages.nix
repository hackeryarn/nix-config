{ pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
    google-chrome
    brave

    git
    wget
    xclip
    ripgrep
    neovim
    fzf
    fd
    sqlite
    gnumake
    stow

    direnv
    nix-direnv

    kwin-tiling
    libsForQt5.spectacle
    ark
    plasma-browser-integration

    obs-studio
    okular
    texlive.combined.scheme-full
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    shellcheck
    editorconfig-core-c
    nixfmt
    sbcl
    racket
    coreutils
    clang
  ];

}
