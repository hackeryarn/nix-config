{
  description = "Home Manager and System config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    nurpkgs = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nurpkgs, home-manager }:
    let system = "x86_64-linux";
    in {
      homeConfigurations = (import ./outputs/home-conf.nix {
        inherit system nixpkgs nurpkgs home-manager;
      });

      nixosConfigurations = (import ./outputs/nixos-conf.nix {
        inherit (nixpkgs) lib;
        inherit inputs system;
      });
    };
}
