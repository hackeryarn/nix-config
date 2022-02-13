{ pkgs, ... }:

{
  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Light";
      package = pkgs.papirus-icon-theme;
    };
    theme = {
      name = "SolArc";
      package = pkgs.solarc-gtk-theme;
    };
  };
}
