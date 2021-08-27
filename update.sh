#!/usr/bin/env bash
set -euo pipefail

sudo cp -r system/* /etc/nixos/
sudo nixos-rebuild switch --upgrade
