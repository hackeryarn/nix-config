{
  programs.bash = {
    enable = true;
    profileExtra = ''
      GUIX_PROFILE="/home/hackeryarn/.config/guix/current"
      . "$GUIX_PROFILE/etc/profile"
    '';
  };
}
