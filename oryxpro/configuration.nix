{ config, pkgs, ... }:

let
  customFonts = pkgs.nerdfonts.override { fonts = [ "Iosevka" "FiraCode" ]; };
  myfonts = pkgs.callPackage nixos/fonts/default.nix { inherit pkgs; };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./nixos/shared.nix
    ./nixos/wm/xmonad.nix
  ];

  fonts.fonts = with pkgs; [
    customFonts
    font-awesome-ttf
    myfonts.icomoon-feather
  ];

  environment.systemPackages = with pkgs; [ lutris ];

  # Enable nvidia
  services.xserver.videoDriver = "nvidia";

  users.users.artem = {
    isNormalUser = true;
    home = "/home/artem";
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
