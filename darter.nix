{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../wm/xmonad.nix
    ../shared.nix
  ];

  # Horizon VPN
  services.openvpn.servers = {
    horizonVPN = {
      config = "config /root/nixos/openvpn/horizon.conf";
      autoStart = false;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.artem = {
    isNormalUser = true;
    home = "/home/hackeryarn";
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
