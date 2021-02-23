update-inputs:
	 nix flake update --update-input nixpkgs
	 nix flake update --update-input home-manager

update-oryx:
	update-inputs
	sudo nixos-rebuild switch --impure --flake '.#oryxpro'

update-darter:
	update-inputs
	sudo nixos-rebuild switch --impure --flake '.#darterpro'
