{ pkgs, ... }:

{
  programs.password-store = {
    enable = true;
    package =
      pkgs.pass.withExtensions (exts: [ exts.pass-otp exts.pass-import ]);
    settings = {
      PASSWORD_STORE_KEY = "A63E3A100E8A5A4D";
      PASSWORD_STORE_CLIP_TIME = "60";
    };
  };
}
