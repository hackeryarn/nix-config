{ config, lib, pkgs, ... }:

let

  plugins = pkgs.vimPlugins;

  overriddenPlugins = with pkgs; [ ];

  # Hack to get parinfer-rust working for vim
  kakounePlugins = with pkgs.kakounePlugins; [ parinfer-rust ];

  myVimPlugins = with plugins;
    [
      fzf-vim
      neomake
      rainbow_parentheses-vim
      vim-easy-align
      vim-easymotion
      vim-fugitive
      vim-repeat
      vim-surround
      vim-obsession

      # For parinfer-rust
      vim-plug

      # Theme
      vim-airline
      vim-airline-themes
      NeoSolarized
      vim-devicons

      # Languages
      ale
      deoplete-nvim
      dhall-vim
      haskell-vim
      vim-tmux
      vim-nix
      vim-css-color
    ] ++ overriddenPlugins ++ kakounePlugins;

  baseConfig = builtins.readFile ./config.vim;
  # cocConfig = builtins.readFile ./coc.vim;
  # cocSettings = builtins.toJSON (import ./coc-settings.nix);
  pluginsConfig = builtins.readFile ./plugins.vim;
  vimConfig = baseConfig + pluginsConfig;

  neovim-5 = pkgs.callPackage ./dev/nightly.nix { };
  nvim5-config = builtins.readFile ./dev/metals.vim;
  new-plugins = pkgs.callPackage ./dev/plugins.nix {
    inherit (pkgs.vimUtils) buildVimPlugin;
    inherit (pkgs) fetchFromGitHub;
  };
  nvim5-plugins = with new-plugins; [
    completion-nvim
    diagnostic-nvim
    nvim-lsp
    nvim-metals
  ];
in {
  programs.neovim = {
    enable = true;
    extraConfig = vimConfig;
    package = neovim-5;
    plugins = myVimPlugins;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
  };

  # xdg.configFile = {
  # "nvim/coc-settings.json".text = cocSettings;
  # };
}
