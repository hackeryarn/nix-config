{ config, pkgs, ... }:

let tmuxConf = builtins.readFile ./default.conf;
in {
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    baseIndex = 1;
    extraConfig = tmuxConf;
    escapeTime = 0;
    keyMode = "vi";
    shortcut = "a";
    terminal = "xterm-256color:Tc";
    plugins = with pkgs.tmuxPlugins; [ tmux-colors-solarized ];
  };
}
