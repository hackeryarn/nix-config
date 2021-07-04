{ config, pkgs, ... }:

let
  gitConfig = {
    core = { editor = "nvim"; };
    init.defaultBranch = "main";
    pull.rebase = false;
  };
in {
  programs.git = {
    enable = true;
    aliases = {
      amend = "commit --amend -m";
      br = "branch";
      co = "checkout";
      st = "status";
      cm = "commit -m";
    };
    extraConfig = gitConfig;
    ignores = [ "*.direnv" ];
    userEmail = "artemchernyak@gmail.com";
    userName = "hackeryarn";
  };
}
