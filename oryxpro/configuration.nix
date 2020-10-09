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
    isNormalUser = true;
    home = "/home/hackeryarn";
    description = "Artem Chernyak";
    extraGroups =
      [ "wheel" "networkmanager" "disk" "audio" "video" "systemd-journal" ];
  };
}
