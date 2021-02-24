{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [ discord ];

  services = { syncthing.enable = true; };

  programs = {
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    git = {
      enable = true;
      userName = "hackeryarn";
      userEmail = "artemchernyak@gmail.com";
    };
  };

  home.stateVersion = "20.09";
}
