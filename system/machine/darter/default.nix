{ config, pkgs, ... }:

{
  boot = { kernelPackages = pkgs.linuxPackages_latest; };

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Horizon VPN
  services.openvpn.servers = {
    horizonVPN = {
      config = "config /root/nixos/openvpn/horizon.conf";
      autoStart = false;
    };
  };
}
