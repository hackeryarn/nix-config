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
    # (let
    #   neuronRev = "1.0.1.0";
    #   neuronSrc = builtins.fetchTarball {
    #     url = "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz";
    #     sha256 = "1zl6kgy1sgzfkw0gaarm2r1ip6zjkxkkfii2fvgwjdir0g8m7r4q";
    #   };
    # in import neuronSrc { })
  ];

}
