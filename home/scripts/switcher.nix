{ config, pkgs, ... }:

let
  home = "${config.xdg.configHome}/nixpkgs";
  swaymsg = "${pkgs.sway}/bin/swaymsg";
in pkgs.writeShellScriptBin "hms" ''
  monitors=$(${swaymsg} -t get_outputs)

  if [[ $monitors == *"HDMI"* ]]; then
    echo "Switching to default HM config for HDMI display"
    cd ${home}
    nix build --impure .#homeConfigurations.hackeryarn-hdmi.activationPackage
    result/activate
    cd -
  elif [[ $monitors == *"eDP"* ]]; then
    echo "Switching to HM config for eDP or eDP-1 (laptop display)"
    cd ${home}
    nix build --impure .#homeConfigurations.hackeryarn-edp.activationPackage
    result/activate
    cd -
    home-manager -f ${config.xdg.configHome}/nixpkgs/display/edp.nix switch
  else
    echo "Could not detect monitor: $monitors"
    exit 1
  fi
''
