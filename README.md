# Dotfiles

A repository of my system configurations. The aim is to have a modular and easily reproducible environment.

Each app configuration lives in it's own folder so you can apply only the needed configurations. Each folder layout supports `stow` for easy installation in the home directory. A notible exceptions are the nixos system configurations.

## Setting up files

Pick the application (folder) that you want to configure and run:

``` sh
$ stow <app>
```

This will try to link the file appropriately. If the files already exist on the system, you will need to delete them and re-run the above command.

## Setting up nixos

Nixos uses Nix Flakes for reproducible confiurations.

The setup needs to sync the hardware config before it can fully install. Make sure to select the same system for all the steps

``` sh
# copy the existing hardware config
$ cp /etc/nixos/harware-configuration.nix <system folder>

# apply the appropriate flake
$ sudo nixos-rebuild switch --flake '.#<system name>'
```
