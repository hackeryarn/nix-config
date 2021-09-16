{
  programs.bash = {
    enable = true;
    profileExtra = ''
      export GUIX_PROFILE="/home/hackeryarn/.config/guix/current"
      . "$GUIX_PROFILE/etc/profile"
      export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
      export PATH="$HOME/.guix-profile/bin/:$PATH"
      if [[ ! -S ''${XDG_RUNTIME_DIR-$HOME/.cache}/shepherd/socket ]]; then
          shepherd
      fi
    '';
    bashrcExtra = ''
      export GUIX_PROFILE="/home/hackeryarn/.config/guix/current"
      . "$GUIX_PROFILE/etc/profile"
      export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
      export PATH="$HOME/.guix-profile/bin/:$PATH"
    '';
  };
}
