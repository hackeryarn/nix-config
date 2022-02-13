{
  xdg.configFile."kanshi/config".text = ''
    profile undocked {
      output eDP-1 enable
    }
    profile docked {
      output eDP-1 disable
      output HDMI-A-2 enable
    }
  '';
}
