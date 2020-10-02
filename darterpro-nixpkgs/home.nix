{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "artem";
  home.homeDirectory = "/home/artem";

  home.packages = [
    pkgs.google-chrome
    pkgs.slack
    pkgs.phantomjs2
    pkgs.cabal-install
    pkgs.nodejs
    pkgs.nixfmt
  ];

  services = { syncthing.enable = true; };

  programs = {
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    git = {
      enable = true;
      userName = "artem";
      userEmail = "artem@horizoninvestments.com";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
