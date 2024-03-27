{ pkgs, ... }:

{
  programs.nixvim = {
    enable = true;

    globals.mapleader = " ";

    options = {
      number = true; # Show line numbers
      relativenumber = true; # Show relative line numbers

      shiftwidth = 2; # Tab width should be 2
    };

    colorschemes.onedark.enable = true;

    # plugins = {
    #   airline.enable = true;

    #   nvim-cmp = {
    #     enable = true;
    #     autoEnableSources = true;
    #     sources = [
    #       # { name = "nvim_lsp"; }
    #       { name = "path"; }
    #       { name = "buffer"; }
    #       # { name = "luasnip"; }
    #     ];

    #     mapping = {
    #       "<CR>" = "cmp.mapping.confirm({ select = true })";
    #       "<Tab>" = {
    #         action = ''
    #           function(fallback)
    #             if cmp.visible() then
    #               cmp.select_next_item()
    #             elseif luasnip.expandable() then
    #               luasnip.expand()
    #             elseif luasnip.expand_or_jumpable() then
    #               luasnip.expand_or_jump()
    #             elseif check_backspace() then
    #               fallback()
    #             else
    #               fallback()
    #             end
    #           end
    #         '';
    #         modes = [ "i" "s" ];
    #       };
    #     };
    #   };

      # lsp = {
      #   enable = true;

      #   servers = {
      #     clangd.enable = true;

      #     nixd.enable = true;

      #     pyright.enable = true;

      #     tsserver.enable = true;

      #     lua-ls = {
      #       enable = true;
      #       settings.telemetry.enable = false;
      #     };
      #   };
      # };

    # };

    extraPlugins = with pkgs.vimPlugins; [{
      plugin = comment-nvim;
      config = ''lua require("Comment").setup()'';
    }];
  };
}
