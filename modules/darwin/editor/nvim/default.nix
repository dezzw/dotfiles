{  lib, config, pkgs, ... }:
with lib;

let
  cfg = config.modules.nvim;
in {
  options.modules.nvim = { enable = mkEnableOption "nvim"; };
  config = mkIf cfg.enable {

    home.file.".config/nvim/settings.lua".source = ./init.lua;
    
    home.packages = with pkgs; [
      stylua # Lua
    ];

    programs.neovim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [ 
        vim-nix
        plenary-nvim
        {
          plugin = impatient-nvim;
          config = "lua require('impatient')";
        }
        {
          plugin = lualine-nvim;
          config = "lua require('lualine').setup()";
        }
        {
          plugin = telescope-nvim;
          config = "lua require('telescope').setup()";
        }
        {
          plugin = indent-blankline-nvim;
          config = "lua require('indent_blankline').setup()";
        }
        {
          plugin = nvim-lspconfig;
          config = ''
                        lua << EOF
                        require('lspconfig').rust_analyzer.setup{}
                        require('lspconfig').rnix.setup{}
                        EOF
                    '';
        }
        {
          plugin = nvim-treesitter;
          config = ''
                    lua << EOF
                    require('nvim-treesitter.configs').setup {
                        highlight = {
                            enable = true,
                            additional_vim_regex_highlighting = false,
                        },
                    }
                    EOF
                    '';
        }
      ];

      extraConfig = ''
                luafile ~/.config/nvim/settings.lua
            '';
    };
  };
}
