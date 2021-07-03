{ pkgs, ... }:

let
  extra = ''
    ${pkgs.util-linux}/bin/setterm -blank 0 -powersave off -powerdown 0
    ${pkgs.xorg.xset}/bin/xset s off
    ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI2 --mode 3440x1440 --rate 60.00
  '';

  polybarOpts = ''
    ${pkgs.nitrogen}/bin/nitrogen --restore &
    ${pkgs.pasystray}/bin/pasystray &
    ${pkgs.blueman}/bin/blueman-applet &
    ${pkgs.gnome3.networkmanagerapplet}/bin/nm-applet --sm-disable --indicator &
  '';
in {
  xresources.properties = {
    "Xft.dpi" = 96;
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintfull";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";
    "Xcursor.theme" = "Numix-Cursor";
    "Xcursor.size" = 24;
  };
  xsession = {
    enable = true;

    initExtra = extra + polybarOpts;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [ hp.dbus hp.monad-logger ];
      config = ./xmonad-hackeryarn/app/Main.hs;
    };
  };
}
