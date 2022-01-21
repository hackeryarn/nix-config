{ config, lib, pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    # stdlib = ''
    #   use_guix() {
    #     local cache_dir="$(direnv_layout_dir)/.guix-profile"
    #     if [[ -e "$cache_dir/etc/profile" ]]; then
    #       # shellcheck disable=SC1091
    #       source "$cache_dir/etc/profile"
    #     else
    #       mkdir -p "$(direnv_layout_dir)"
    #       eval "$(guix environment --root="$cache_dir" "$@" --search-paths)"
    #     fi
    #   }
    # '';
    nix-direnv.enable = true;
  };
}
