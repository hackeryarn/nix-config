{ pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
    google-chrome

    git
    wget
    xclip
    ripgrep
    neovim
    fzf
    fd
    sqlite

    direnv
    nix-direnv
    stow

    kwin-tiling
    libsForQt5.spectacle

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
