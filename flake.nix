{
  description = "NixOs configuration";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "master";
    };

    neuron = {
      type = "github";
      owner = "srid";
      repo = "neuron";
      ref = "master";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, neuron }:
    let
      system = "x86_64-linux";
      pkgs = (import nixpkgs) {
        inherit system;
        config = { allowUnfree = true; };
      };
    in {
      nixosConfigurations.darterpro = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./darterpro/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.artem = import ./home/artem.nix {
              inherit system;
              inherit neuron;
            };
          }
        ];
      };

      nixosConfigurations.oryxpro = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./oryxpro/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.hackeryarn = import ./home/hackeryarn.nix {
              inherit system;
              inherit neuron;
            };
            home-manager.users.artem = import ./home/artem.nix {
              inherit system;
              inherit neuron;
            };
          }
        ];
      };
    };
}
