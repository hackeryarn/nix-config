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
      waybar
      wl-clipboard # clipboard
      wofi # app launcher
      wf-recorder # screen recorder
      mako # notification daemon
      kanshi # auto screen lock
      # screen capture
      slurp
      grim
    ];
  };

  environment = {
    etc = {
      "sway/config".source = ../dotfiles/sway/config;
      "xdg/waybar/config".source = ../dotfiles/waybar/config;
      "xdg/waybar/style.css".source = ../dotfiles/waybar/style.css;
    };
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

  systemd.user.services.sway = {
    description = "Sway - Wayland window manager";
    documentation = [ "man:sway(5)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
    # We explicitly unset PATH here, as we want it to be set by
    # systemctl --user import-environment in startsway
    environment.PATH = lib.mkForce null;
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
      '';
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
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

  systemd.user.services.waybar = {
    description = "Status bar for sway";
    wantedBy = [ "sway-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.waybar}/bin/waybar
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

  systemd.user.services.kanshi = {
    description = "Kanshi output autoconfig ";
    wantedBy = [ "sway-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      # kanshi doesn't have an option to specifiy config file yet, so it looks
      # at .config/kanshi/config
      ExecStart = ''
        ${pkgs.kanshi}/bin/kanshi
      '';
      RestartSec = 5;
      Restart = "always";
    };
  };

  systemd.user.targets.tray = {
    description = "Sway tray target for udiskie";
    documentation = [ "man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-sesion-pre.target" ];
    after = [ "graphical-session-pre.target" ];
    wantedBy = [ "sway-session.targte" ];
  };

  location.provider = "geoclue2";
  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
  };

  programs.light.enable = true;

  environment.loginShellInit = ''
    [[ "$(tty)" == /dev/tty1 ]] && startsway
  '';
}
