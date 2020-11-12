{ pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
    git
    wget
    xclip
    ripgrep
    coreutils
    fd
    clang
    neovim
    fzf
    direnv
    nix-direnv
    stow
    kwin-tiling
    kdeApplications.spectacle
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    shellcheck
    editorconfig-core-c
    texlive.combined.scheme-full
    obs-studio
    okular
  ];

}
