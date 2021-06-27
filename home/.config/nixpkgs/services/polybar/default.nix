{ mainBar, openCalendar, config, pkgs, ...}:

let
  mypolybar = pkgs.polybar.override {
    alsaSupport = true;
    mpdSupport = true;
    pulseSupport = true;
  };

  bars = builtins.readFile ./bars.ini;
  colors = builtins.readFile ./colors.ini;
  mods = builtins.readFile ./modules.ini;
  networkScript   = pkgs.callPackage ./scripts/network.nix {};

in
{
  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./config.ini;
    extraConfig = bars + colors + mods;
    script = ''
      export ETH_INTERFACE=$(${networkScript}/bin/check-network eth)
      export WIFI_INTERFACE=$(${networkScript}/bin/check-network wifi)
      echo "Network interfaces $ETH_INTERFACE & $WIFI_INTERFACE"
      polybar top 2>${config.xdg.configHome}/polybar/logs/top.log & disown
      polybar bottom 2>${config.xdg.configHome}/polybar/logs/bottom.log & disown
    '';
  };
}
