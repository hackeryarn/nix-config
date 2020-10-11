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
  };

  outputs = { self, nixpkgs, home-manager }:
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
            home-manager.users.artem =
              import ./home/artem.nix { inherit system; };
          }
        ];
      };
      nixosConfigurations.oryxpro = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./oryxpro/configuration.nix ];
      };
    };
}
