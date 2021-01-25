{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../nixos/shared.nix
  ];

  environment.systemPackages = with pkgs; [ lutris ];

  # Enable nvidia
  services.xserver.videoDriver = "nvidia";

  # Open VPN
  services.openvpn.servers = {
    horizonVPN = {
      config = "config /root/nixos/openvpn/horizon.conf";
      autoStart = false;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.hackeryarn = {
    isNormalUser = true;
    home = "/home/hackeryarn";
    description = "hackeryarn";
    extraGroups = [
      "wheel"
      "networkmanager"
      "disk"
      "audio"
      "video"
      "systemd-journal"
      "docker"
    ];
  };

  users.users.artem = {
    isNormalUser = true;
    home = "/home/artem";
    description = "Artem Chernyak";
    extraGroups = [
      "wheel"
      "networkmanager"
      "disk"
      "audio"
      "video"
      "systemd-journal"
      "docker"
    ];
  };

}
