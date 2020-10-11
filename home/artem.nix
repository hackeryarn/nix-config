{ system }:
{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    google-chrome
    slack
    phantomjs2
    cabal-install
    nodejs
    nixfmt
    zoom-us
    kakoune
    kak-lsp
    parinfer-rust
    nnn
  ];

  services = { syncthing.enable = true; };

  programs = {
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    git = {
      enable = true;
      userName = "artem";
      userEmail = "artem@horizoninvestments.com";
    };
  };

  systemd.user.services.neuron = let
    notesDir = "/home/artem/zettelkasten";
    neuron = (let
      neuronRev = "1.0.1.0";
      neuronSrc = builtins.fetchTarball {
        url = "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz";
        sha256 = "1zl6kgy1sgzfkw0gaarm2r1ip6zjkxkkfii2fvgwjdir0g8m7r4q";
      };
    in import neuronSrc { inherit system; });
  in {
    Unit.Description = "Neuron zettelkasten service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = { ExecStart = "${neuron}/bin/neuron -d ${notesDir} rib -wS"; };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
