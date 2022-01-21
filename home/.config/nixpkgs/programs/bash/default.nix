{
  programs.bash = {
    enable = true;
    profileExtra = ''
      export GUIX_PROFILE="/home/hackeryarn/.config/guix/current"
      . "$GUIX_PROFILE/etc/profile"
      export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
      export PATH="$HOME/.config/guix/current/bin/:$HOME/.guix-profile/bin/:$PATH"
      export XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share/applications:$XDG_DATA_DIRS"
    '';
    bashrcExtra = ''
      export GUIX_PROFILE="/home/hackeryarn/.config/guix/current"
      . "$GUIX_PROFILE/etc/profile"
      export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
      export PATH="$HOME/.config/guix/current/bin/:$HOME/.guix-profile/bin/:$PATH"
    '';
  };
}
