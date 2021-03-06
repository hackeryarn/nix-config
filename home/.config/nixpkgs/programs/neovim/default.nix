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
      nvim-lspconfig
      nvim-compe
      dhall-vim
      haskell-vim
      vim-tmux
      vim-nix
      vim-css-color
    ] ++ overriddenPlugins ++ kakounePlugins;

  baseConfig = builtins.readFile ./config.vim;
  pluginsConfig = builtins.readFile ./plugins.vim;
  vimConfig = baseConfig + pluginsConfig;

  neovim-5 = pkgs.callPackage ./dev/nightly.nix { };
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
}
