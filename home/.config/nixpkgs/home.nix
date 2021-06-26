{ config, pkgs, lib, ... }:

let
  defaultPkgs = with pkgs; [
    nix-doc
    nix-index
    rnix-lsp
  ];

in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  nixpkgs.config = {
    allowUnfree = true;
  };

  imports = (import ./programs);

  xdg.enable = true;

  home = {
    username = "artem";
    homeDirectory = "/home/artem";
    stateVersion = "21.11";

    packages = defaultPkgs;

    sessionVariables = {
      EDITOR = "nvim";
    };
  };

  news.display = "silent";

  programs = {
    emacs.enable = true;
    jq.enable = true;
    bat.enable = true;

    htop = {
      enable = true;
      settings = {
        sort_direction = "descending";
        sort_key = "PERCENT_CPU";
      };
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

  };
}
