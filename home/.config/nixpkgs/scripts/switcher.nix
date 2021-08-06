{ config, pkgs, ... }:

let
  fish = "${pkgs.fish}/bin/fish";
  swaymsg = "${pkgs.sway}/bin/swaymsg";
in pkgs.writeShellScriptBin "hms" ''
  monitors=$(${swaymsg} -t get_outputs)
  if [[ $monitors == *"HDMI"* ]]; then
    echo "Switching to default HM config for HDMI display"
    home-manager -f ${config.xdg.configHome}/nixpkgs/display/hdmi.nix switch
  elif [[ $monitors == *"eDP"* ]]; then
    echo "Switching to HM config for eDP or eDP-1 (laptop display)"
    home-manager -f ${config.xdg.configHome}/nixpkgs/display/edp.nix switch
  else
    echo "Could not detect monitor: $monitors"
    exit 1
  fi
  if [[ $1 == "fish" ]]; then
    ${fish} -c fish_update_completions
  fi
''
