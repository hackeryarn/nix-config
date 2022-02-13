# Configuration for the HDMI-2 display monitor
{ config, lib, pkgs, stdenv, nur, ... }:

let
  base = pkgs.callPackage ../home.nix { inherit config lib pkgs stdenv; };

  terminal = import ../programs/alacritty/default.nix {
    fontSize = 14;
    inherit pkgs;
  };
in {
  imports = [ ../home.nix terminal ];

  home.packages = base.home.packages;
}
