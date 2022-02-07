{pkgs, ...}:

{
  programs.neovim = {
    enable = true;

    package = pkgs.neovim-nightly;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;

    extraConfig = ''
      set number relativenumber
      set nobackup
      set clipboard=unnamed
    '';

    plugins = with pkgs.vimPlugins; [
      nord-vim
      lightline-vim
      vim-nix
      telescope-nvim
      nvim-lspconfig
      completion-nvim
      nvim-tree-lua
    ];
  };
}
