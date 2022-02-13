# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, ... }:

let
  base = pkgs.callPackage ../home.nix { inherit config lib pkgs stdenv; };

  terminal = import ../programs/alacritty/default.nix {
    fontSize = 8;
    inherit pkgs;
  };
in {
  imports = [ ../home.nix terminal ];

  home.packages = base.home.packages;
}
