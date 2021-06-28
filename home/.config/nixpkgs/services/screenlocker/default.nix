{ pkgs, ... }:

{
  services.screen-locker = {
    enable = true;
    inactiveInterval = 20;
    lockCmd = "${pkgs.multilockscreen}/bin/multilockscreen -l dim";
    xautolockExtraOptions = [
      "Xautolock.killer: systemctl suspend"
    ];
  };
}
