{ pkgs, ...}:

let
  xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
in
  pkgs.writeShellScriptBin "monitor" ''
    monitors=$(${xrandr} --listmonitors)
    if [[ $monitors == *"HDMI"* ]]; then
      echo "HDMI"
    else
      echo "eDP"
    fi
  ''
}
