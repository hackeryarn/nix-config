# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Enable virtualization
  boot.extraModprobeConfig = "options kvm_intel nested=1";
  virtualisation.libvirtd.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.enp111s0.useDHCP = true;
  # networking.interfaces.wlp113s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  nixpkgs.config = {
    allowUnfree = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
    git
    wget
    xclip
    ripgrep
    coreutils
    fd
    clang
    neovim
    fzf
    direnv
    nix-direnv
    stow
    kwin-tiling
    kdeApplications.spectacle
    (let neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
     in import neuronSrc {})
  ];

  # Enable derivations to persist garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];
  
  location.provider = "geoclue2";

  services = {
    # Enable emacs service
    emacs.enable = true;

    # Enable resdshift for better screen color
    redshift.enable = true;
  };

  programs = {
    tmux = {
      enable = true;
      extraConfig = ''
        # Set term color
        set-option -g default-terminal "screen-256color"
        set-option -sa terminal-overrides ',screen-256color:RGB'

        # Rebinding prefix to C-a
        set -g prefix C-a
        unbind C-b

        # Ensure that we can send Ctrl-A to other apps
        bind C-a send-prefix

        # Setting the delay between prefix and command
        set -s escape-time 1

        # Set the base index for windows to 1 instead of 0
        set -g base-index 1

        # Set the base index for panes to 1 instead of 0
        setw -g pane-base-index 1

        # Key bindings
        bind r source-file ~/.tmux.conf \; display "Reloaded!"

        # splitting panes with | and -
        bind \\ split-window -h -c "#{pane_current_path}"
        bind - split-window -v -c "#{pane_current_path}"

        # Quick window selection
        bind -r C-h select-window -t :-
        bind -r C-l select-window -t :+

        # Pane resizing panes with Prefix H,J,K,L
        bind -r H resize-pane -L 5
        bind -r J resize-pane -D 5
        bind -r K resize-pane -U 5
        bind -r L resize-pane -R 5

        # mouse support - set to on if you want to use the mouse
        set -g mouse on

        # set colors for the active window
        setw -g window-status-current-style fg=white,bold,bg=yellow

        #Status line left side to show Session:window:pane
        set -g status-left-length 40
        set -g status-left "#[fg=black]Session: #S #[fg=colour243]#I #[fg=black]#P | "

        # Status line right side - 21-Oct 13:37
        set -g status-right "#[fg=black]%d %b %R"

        # Center the window list in the staus line
        set -g status-justify centre

        # enable activity alerts
        setw -g monitor-activity on
        set -g visual-activity on

        # enable vi keys.
        setw -g mode-keys vi

        bind-key -r h select-pane -L
        bind-key -r j select-pane -D
        bind-key -r k select-pane -U
        bind-key -r l select-pane -R

        bind-key -T copy-mode-vi 'v' send-keys -X begin-selection
        bind-key -T copy-mode-vi 'y' send-keys -X copy-selection

        # better yanking
        unbind p
        bind p paste-buffer

        # shortcut for synchronize-panes toggle
        bind C-s set-window-option synchronize-panes

        # copy
        bind-key -n -T copy-mode-vi Enter send-keys -X copy-pipe 'xclip -i -sel p -f | xclip -i -sel c'
        bind-key -n -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe 'xclip -i -sel p -f | xclip -i -sel c' 
      '';
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

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
  sound.enable = true;
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

  # Ignore lid close when docked
  services.logind.lidSwitchDocked = "ignore";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.artem = {
    isNormalUser = true;
    home = "/home/artem";
    description = "Artem Chernyak";
    extraGroups = [
      "wheel" "networkmanager" "disk"
      "audio" "video" "systemd-journal"
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?
}

