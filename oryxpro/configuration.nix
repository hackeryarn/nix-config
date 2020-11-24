{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../nixos/shared.nix
  ];

  # Enable nvidia
  services.xserver.videoDriver = "nvidia";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.hackeryarn = {
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    boot.loader.grub.device = "/dev/sda";
    boot.loader.grub.useOSProber = true;
    isNormalUser = true;
    home = "/home/hackeryarn";
    description = "Artem Chernyak";
    extraGroups =
      [ "wheel" "networkmanager" "disk" "audio" "video" "systemd-journal" ];
  };
}
