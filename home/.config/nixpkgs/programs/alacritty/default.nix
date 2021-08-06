{ fontSize, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      env = { TERM = "xterm-256color"; };
      selection.save_to_clipboard = true;
      shell.program = "${pkgs.fish}/bin/fish";
      colors = {
        primary = {
          background = "#fdf6e3";
          foreground = "#586e75";
        };
        normal = {
          black = "#073642";
          red = "#dc322f";
          green = "#859900";
          yellow = "#b58900";
          blue = "#268bd2";
          magenta = "#d33682";
          cyan = "#2aa198";
          white = "#eee8d5";
        };
        bright = {
          black = "#002b36";
          red = "#cb4b16";
          green = "#586e75";
          yellow = "#657b83";
          blue = "#839496";
          magenta = "#6c71c4";
          cyan = "#93a1a1";
          white = "#fdf6e3";
        };
      };
      font = {
        normal = {
          family = "FiraCode Nerd Font";
          style = "Medium";
        };
        size = fontSize;
      };
    };
  };
}
