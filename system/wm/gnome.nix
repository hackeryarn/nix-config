{ config, lib, pkgs, ... }:

{

  # Enable the X11 windowing system.
  services = {
    xserver = {
      enable = true;
      layout = "us";

      # Enable the KDE Desktop Environment.
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false;
      desktopManager.gnome.enable = true;

      # Enable touchpad support.
      libinput.enable = true;

      # Set caplock as ctrl
      xkbOptions = "ctrl:nocaps";
    };
  };

}
