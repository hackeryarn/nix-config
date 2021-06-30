#!/usr/bin/env bash
set -euo pipefail

set +x

# NixOs config
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo cp -r nixos/* /etc/nixos/
sudo nixos-rebuild switch --upgrade

# Polybar setup
mkdir -p $HOME/.config/polybar/logs
touch $HOME/.config/polybar/logs/bottom.log
touch $HOME/.config/polybar/logs/top.log

# Home manager
mkdir -p $HOME/.config/nixpkgs/
stow home
nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install
cp home/nixos.png $HOME/Pictures/
home-manager switch

multilockscreen -u wallpapers/kame-house.jpg
