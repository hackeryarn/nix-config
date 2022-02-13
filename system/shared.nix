{ config, lib, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;

    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';

    # Required by cachix to be used as non-root user
    settings = {
      auto-optimise-store = true;
      trusted-users = [ "root" "hackeryarn" ];
    };
  };

  users = {
    extraUsers = let
      hackeryarnUser = {
        hackeryarn = {
          isNormalUser = true;
          home = "/home/hackeryarn";
          description = "Artem Chernyak";
          extraGroups = [
            "wheel"
            "networkmanager"
            "disk"
            "audio"
            "video"
            "systemd-journal"
            "docker"
            "qemu-libvirtd"
            "libvirtd"
            "vboxusers"
          ];
        };
      };
      buildUser = (i: {
        "guixbuilder${i}" = { # guixbuilder$i
          group = "guixbuild"; # -g guixbuild
          extraGroups = [ "guixbuild" ]; # -G guixbuild
          home = "/var/empty"; # -d /var/empty
          shell = pkgs.nologin; # -s `which nologin`
          description = "Guix build user ${i}"; # -c "Guix buid user $i"
          isSystemUser = true; # --system
        };
      });
      # merge all users
    in pkgs.lib.fold (str: acc: acc // buildUser str) hackeryarnUser
    # for i in `seq -w 1 10`
    (map (pkgs.lib.fixedWidthNumber 2) (builtins.genList (n: n + 1) 10));
    extraGroups.guixbuild = { name = "guixbuild"; };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-amd" "kvm-intel" ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    extraModprobeConfig = "options kvm_intel nested=1";
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    # Enable wireless support and openvpn via network manager.
    networkmanager = {
      enable = true;
      packages = [ pkgs.networkmanager_openvpn ];
    };

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ vim git wget stow ];

  # Enable virtualization
  virtualisation = {
    libvirtd.enable = true;
    virtualbox.host.enable = true;

    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
  };

  # Enable nfs with libvirt
  services.nfs.server.enable = true;
  networking.firewall.extraCommands = ''
    ip46tables -I INPUT 1 -i virbr+ -p tcp -m tcp --dport 2049 -j ACCEPT
    ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  '';

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  nixpkgs.config = { allowUnfree = true; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };

  # Ignore lid close when docked
  services.logind = {
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
    extraConfig = "HandleLidSwitch=ignore";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?
}
