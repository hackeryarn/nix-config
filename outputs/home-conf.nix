{ system, nixpkgs, nurpkgs, home-manager, ... }:

let
  username = "hackeryarn";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;
    config.xdg.configHome = configHome;

    # overlays = [ nurpkgs.overlay ];
  };

  nur = import nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };

  mkHome = conf:
    (home-manager.lib.homeManagerConfiguration rec {
      inherit pkgs system username homeDirectory;
      stateVersion = "22.05";
      configuration = conf;
    });

  edpConf = import ../home/display/edp.nix {
    inherit nur pkgs;
    inherit (pkgs) config lib stdenv;
  };

  hdmiConf = import ../home/display/hdmi.nix {
    inherit nur pkgs;
    inherit (pkgs) config lib stdenv;
  };
in {
  hackeryarn-edp = mkHome edpConf;
  hackeryarn-hdmi = mkHome hdmiConf;
}
