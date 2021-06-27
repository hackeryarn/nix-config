{ config, pkgs, ... }:

let
  hms = pkgs.callPackage ./switcher.nix { inherit config pkgs; };
in
[
  hms         # custom home-manager switcher that considers the current DISPLAY
]
