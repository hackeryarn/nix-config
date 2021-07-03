{ config, lib, pkgs, ... }:

{
  programs.emacs.enable = true;
  xdg.configFile = { "doom" = { source = ./config; }; };
}
