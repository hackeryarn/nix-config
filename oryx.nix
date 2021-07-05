{ config, pkgs, ... }:

let
  customFonts = pkgs.nerdfonts.override { fonts = [ "Iosevka" "FiraCode" ]; };
  myfonts = pkgs.callPackage nixos/fonts/default.nix { inherit pkgs; };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./shared.nix
    ./wm/xmonad.nix
  ];

  fonts.fonts = with pkgs; [
    customFonts
    font-awesome-ttf
    myfonts.icomoon-feather
  ];

  environment.systemPackages = with pkgs; [ lutris ];

  # Enable nvidia
  services.xserver.videoDriver = "nvidia";

  users.users.hackeryarn = {
    isNormalUser = true;
    home = "/home/hackeryarn";
    description = "Artem Chernyak";
    shell = pkgs.fish;
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
