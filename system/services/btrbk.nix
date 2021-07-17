{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.btrbk ];

  systemd.services.btrbk = {
    description = "btrbk backup";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.btrbk}/bin/btrbk run";
    };
  };

  systemd.timers.btrbk = {
    description = "btrbk backup daily";
    wantedBy = [ "multi-user.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };

  environment.etc."btrbk/btrbk.conf".text = ''
    timestamp_format        long
    snapshot_preserve_min   2d
    snapshot_preserve      14d

    target_preserve_min    no
    target_preserve        20d 10w *m

    snapshot_dir           btrbk_snapshots

    volume /mnt
      target /run/media/hackeryarn/c2ef46b0-7c6d-4d8e-b070-0fc5f420f95e/nixos
      subvolume home
  '';
}
