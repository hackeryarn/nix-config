{ config, lib, pkgs, ... }:

let lockBg = "~/nix-config/wallpapers/kame-house.jpg";
in {
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    wrapperFeatures.base = true;
    extraPackages = with pkgs; [
      swaylock # lockscreen
      swayidle
      xwayland # for legacy apps
      wl-clipboard # clipboard
      wofi # app launcher
      wf-recorder # screen recorder
      mako # notification daemon
      slurp
      grim
    ];
  };

  environment.systemPackages = with pkgs;
    [
      (pkgs.writeTextFile {
        name = "startsway";
        destination = "/bin/startsway";
        executable = true;
        text = ''
          #! ${pkgs.bash}/bin/bash

          # first import environment variables from the login manager
          systemctl --user import-environment
          # then start the service
          # /home/hackeryarn/.guix-profile/bin/shepherd &
          exec systemctl --wait --user start sway.service
        '';
      })
    ];

  systemd.user.targets.sway-session = {
    description = "Sway compositor session";
    documentation = [ "man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
  };

  systemd.user.services.swayidle = {
    description = "Idle Manager for Wayland";
    documentation = [ "man:swayidle(1)" ];
    wantedBy = [ "sway-session.target" ];
    partOf = [ "graphical-session.target" ];
    path = [ pkgs.bash ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.swayidle}/bin/swayidle -w -d \
               timeout 300 '${pkgs.swaylock}/bin/swaylock -f -i ${lockBg}' \
               timeout 900 'systemctl suspend' \
               timeout 600 '${pkgs.sway}/bin/swaymsg "output * dpms off"' \
               resume '${pkgs.sway}/bin/swaymsg "output * dpms on"' \
               before-sleep '${pkgs.swaylock}/bin/swaylock -f -i ${lockBg}'
      '';
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  systemd.user.services.mako = {
    description = "Mako notifications";
    wantedBy = [ "sway-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.mako}/bin/mako
      '';
      RestartSec = 5;
      Restart = "always";
    };
  };

  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
  };

  programs.light.enable = true;

  environment.loginShellInit = ''
    export GUIX_LOCPATH=/home/hackeryarn/.guix-profile/lib/locale
    export XDG_DATA_DIRS="home/hackeryarn/.local/share/flatpak/exports/share/applications:$XDG_DATA_DIRS"
    if [[ ! -S ''${XDG_RUNTIME_DIR-$HOME/.cache}/shepherd/socket ]]; then
        /home/hackeryarn/.guix-profile/bin/sway
    fi
  '';
}
