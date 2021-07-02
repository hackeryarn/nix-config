{ pkgs, ... }:

let xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
in pkgs.writeShellScriptBin "monitor" ''
  monitors=$(${xrandr} --listmonitors)
  if [[ $monitors == *"HDMI-0"* ]]; then
    echo "HDMI-0"
  elif [[ $monitors == *"HDMI"* ]]; then
    echo "eDP1"
  else
    echo "eDP1"
  fi
''
