{
  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart =
        "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      Environment = "GUIX_LOCPATH=/root/.guix-profile/lib/locale";
      RemainAfterExit = "yes";
      StandardOutput = "syslog";
      StandardError = "syslog";
      TaskMax = "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
