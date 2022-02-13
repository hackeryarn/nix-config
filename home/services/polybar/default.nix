{ mainBar, openCalendar, config, pkgs, ... }:

let
  browser = "${pkgs.firefox}/bin/firefox";

  xdgUtils = pkgs.xdg_utils.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
    postInstall = old.postInstall + "\n" + ''
      wrapProgram $out/bin/xdg-open --suffix PATH : /run/current-system/sw/bin --suffix BROWSER : ${browser}
    '';
  });

  mypolybar = pkgs.polybar.override {
    alsaSupport = true;
    pulseSupport = true;
  };

  bars = builtins.readFile ./bars.ini;
  colors = builtins.readFile ./colors.ini;
  mods1 = builtins.readFile ./modules.ini;
  mods2 = builtins.readFile ./user_modules.ini;

  bluetoothScript = pkgs.callPackage ./scripts/bluetooth.nix { };
  monitorScript = pkgs.callPackage ./scripts/monitor.nix { };
  mprisScript = pkgs.callPackage ./scripts/mpris.nix { };
  networkScript = pkgs.callPackage ./scripts/network.nix { };

  bctl = ''
    [module/bctl]
    type = custom/script
    exec = ${bluetoothScript}/bin/bluetooth-ctl
    tail = true
    click-left = ${bluetoothScript}/bin/bluetooth-ctl --toggle &
  '';

  cal = ''
    [module/clickable-date]
    inherit = module/date
    label = %{A1:${openCalendar}:}%time%%{A}
  '';

  xmonad = ''
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log
    tail = true
  '';

  customMods = mainBar + bctl + cal + xmonad;
in {
  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./config.ini;
    extraConfig = bars + colors + mods1 + mods2 + customMods;
    script = ''
      export MONITOR=$(${monitorScript}/bin/monitor)
      echo "Running polybar on $MONITOR"
      export ETH_INTERFACE=$(${networkScript}/bin/check-network eth)
      export WIFI_INTERFACE=$(${networkScript}/bin/check-network wifi)
      echo "Network interfaces $ETH_INTERFACE & $WIFI_INTERFACE"
      polybar top 2>${config.xdg.configHome}/polybar/logs/top.log & disown
      polybar bottom 2>${config.xdg.configHome}/polybar/logs/bottom.log & disown
    '';
  };
}
