#!/usr/bin/env bash
set -euo pipefail
set +x

SYSTEM_TO_INSTALL="darter"

# NixOs config
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
sudo cp -r system/* /etc/nixos/
sudo cp "$SYSTEM_TO_INSTALL.nix" /etc/nixos/configuration.nix
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
home-manager switch

multilockscreen -u wallpapers/kame-house.jpg

hms

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

sudo mount -o subvol=/ /dev/mapper/enc /mnt
