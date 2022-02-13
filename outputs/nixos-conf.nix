{ lib, inputs, system, ... }:

{
  darter = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [ ../system/machine/darter ../system/configuration.nix ];
  };
}
