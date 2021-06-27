{ config, lib, pkgs, ... }:

{
  nix = {
    autoOptimiseStore = true;

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

    # Required by cachix to be used an non-root user
    trustedUser = ["root" "artem"];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking = {
    hostName = "nixos"; # Define your hostname.
    # Enable wireless support and openvpn via network manager.
    networkmanager = {
      enable   = true;
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
  environment.systemPackages = with pkgs; [
    vim
    wget
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Enable virtualization
  boot.extraModprobeConfig = "options kvm_intel nested=1";

  virtualisation = {
    libvirtd.enable = true;
    docker = {
     enable = true;
     autoPrune = {
       enable = true;
       dates = "weekly";
     };
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  nixpkgs.config = { allowUnfree = true; };

  environment.pathsToLink = [ "/share/nix-direnv" ];

  location.provider = "geoclue2";

  services = {
    # Enable emacs service
    emacs.enable = true;

    # Enable resdshift for better screen color
    redshift.enable = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable graphics.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  # Enable sound.
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    # Enable the KDE Desktop Environment.
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;

    # Enable touchpad support.
    libinput.enable = true;
    # Set caplock as ctrl
    xkbOptions = "ctrl:nocaps";
  };

  programs.fish.enable = true;

  # Ignore lid close when docked
  services.logind.lidSwitchDocked = "ignore";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?
}
