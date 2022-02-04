#!/usr/bin/env bash
set -euo pipefail

nix-channel --update
sudo cp -r system/* /etc/nixos/
sudo nixos-rebuild switch --upgrade

hms
