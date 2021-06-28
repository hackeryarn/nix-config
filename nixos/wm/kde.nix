{ config, lib, pkgs, ... }:

{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    # Enable the KDE Desktop Environment.
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;

    # Enable touchpad support.
    libinput.enable = true;
    # Set caplock as ctrl
    xkbOptions = "ctrl:nocaps";
  };

}
