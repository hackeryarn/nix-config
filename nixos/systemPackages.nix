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
    (let
      neuronSrc = builtins.fetchTarball
        "https://github.com/srid/neuron/archive/master.tar.gz";
    in import neuronSrc { })
  ];

}
