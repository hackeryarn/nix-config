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
  # services.openvpn.servers = {
  #   horizonVPN = {
  #     config = "config /root/nixos/openvpn/horizon.conf";
  #     autoStart = false;
  #   };
  # };

  users.users.artem = {
    isNormalUser = true;
    home = "/home/artem";
    description = "Artem Chernyak";
    shell =  pkgs.fish;
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
