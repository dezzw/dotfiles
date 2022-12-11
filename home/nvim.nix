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
    withRuby = true;

    extraConfig = ''
      set number relativenumber
      set nobackup
      set clipboard=unnamed

      let mapleader=" "
    '';

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
                        require('lspconfig').sumneko_lua.setup{}
                        require('lspconfig').rnix.setup{}
                        require('lspconfig').zk.setup{}
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

  };
  
}
