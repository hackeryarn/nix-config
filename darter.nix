{ config, pkgs, ... }:

let
  customFonts = pkgs.nerdfonts.override { fonts = [ "Iosevka" "FiraCode" ]; };
  myfonts = pkgs.callPackage fonts/default.nix { inherit pkgs; };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./wm/sway.nix
    ./services/btrbk.nix
    ./shared.nix
  ];

  fonts.fonts = with pkgs; [
    customFonts
    font-awesome-ttf
    myfonts.icomoon-feather
  ];

  # Horizon VPN
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
