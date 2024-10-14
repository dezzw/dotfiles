{ pkgs, ... }:

{
  programs.nixvim = {
    enable = true;

    globals.mapleader = " ";

    opts = {
      number = true; # Show line numbers
      relativenumber = true; # Show relative line numbers

      shiftwidth = 2; # Tab width should be 2
    };

    colorschemes.onedark.enable = true;

    plugins = {
      airline.enable = true;

      cmp.enable = true;
      cmp-buffer.enable = true;
      cmp-dictionary.enable = true;
      cmp-nvim-lsp.enable = true;
      cmp-path.enable = true;
      cmp-tabnine.enable = true;

      lsp = {
        enable = true;

        servers = {
          clangd.enable = true;

          nixd.enable = true;

          pyright.enable = true;

          ts_ls.enable = true;

          lua_ls = {
            enable = true;
            settings.telemetry.enable = false;
          };
        };
      };

      comment.enable = true;

    };

    extraPlugins = with pkgs.vimPlugins; [];
  };
}
