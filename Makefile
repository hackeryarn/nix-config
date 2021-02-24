update-inputs:
	 nix flake update --update-input nixpkgs
	 nix flake update --update-input home-manager

update-oryx:
	sudo nixos-rebuild switch --impure --flake '.#oryxpro'

update-darter:
	sudo nixos-rebuild switch --impure --flake '.#darterpro'

full-update-darter: update-inputs update-darter
