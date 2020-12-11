{ system, neuron }:
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

  systemd.user.services.neuron = let
    notesDir = "/home/artem/zettelkasten";
    neuron-zettel = import neuron { inherit system; };
  in {
    Unit.Description = "Neuron zettelkasten service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${neuron-zettel}/bin/neuron -d ${notesDir} rib -wS";
    };
  };

  home.stateVersion = "20.09";
}
