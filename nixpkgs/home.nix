{ config, pkgs, ... }:

let
  comma = import ( pkgs.fetchFromGitHub {
      owner = "Shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) {};

in

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  home.packages = with pkgs; [

    nixUnstable

    # c/c++
    ccls
    cmake

    # nodejs
    nodejs
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.bash-language-server
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin

    # nix
    rnix-lsp

    #cach
    cachix

    # direnv
    direnv

    # tools
    comma
    jump
    exa
    stow
    ripgrep

    # tex
    texlive.combined.scheme-full
  ];

  programs.git = {
    enable = true;
    userName  = "UncleAlone";
    userEmail = "desmond.pc.w@gmail.com";
    delta = {
      enable = true;
      options = {
        features = "decorations";

        interactive = {
          keep-plus-minus-markers = false;
        };

        decorations = {
          commit-decoration-style = "blue ol";
          commit-style = "raw";
          file-style = "omit";
          hunk-header-decoration-style = "blue box";
          hunk-header-file-style = "red";
          hunk-header-line-number-style = "#067a00";
          hunk-header-style = "file line-number syntax";
        };
      };
    };
  };

  programs.bat.enable = true;
  programs.bat.config = {
    theme = "ansi";
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      ls = "exa";
      la = "exa -la";
      lt = "exa -laT";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "zsh-users/zsh-autosuggestions"; } # Simple plugin installation
	      { name = "zsh-users/zsh-syntax-highlighting"; }
        { name = "spwhitt/nix-zsh-completions"; }
	      { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
      ];
    };
    initExtra = ''
     . $HOME/.p10k.zsh
     . $HOME/.dotfiles/Shells/emacs-cmds.sh
     . $HOME/.dotfiles/Shells/doom-emacs-cmds.sh
     eval "$(jump shell)"
     eval "$(direnv hook zsh)"
    '';
  };


  programs.emacs.enable = true;

  programs.emacs.package =
    (
      pkgs.emacsWithPackagesFromUsePackage {
        alwaysEnsure = true;
        alwaysTangle = true;

        # Custom overlay derived from 'emacs' flake input
        package = pkgs.emacs;
        config = ../Emacs/emacs-configs/demacs/init.el;

      }
    );

  home.file.".emacs-profiles.el".text = ''
      (("default" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/demacs")))
       ("doom" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/doom-core")
	      ;;(env . (("DOOMDIR" . "~/.dotfiles/Emacs/emacs-configs/doom")))))
	    ))
       ("beta" . ((user-emacs-directory . "~/.dotfiles/Emacs/emacs-configs/beta_emacs")))
      )
  '';

  # programs.neovim = {
  #   enable = true;
  #   extraConfig = ''
  #       colorscheme gruvbox
  #       let g:context_nvim_no_redraw = 1
  #       set mouse=a
  #       set number
  #       set termguicolors
  #     '';
  #   plugins = with pkgs.vimPlugins; [
  #       editorconfig-vim
  #       gruvbox-community
  #       vim-airline
  #       vim-elixir
  #       vim-nix
  #     ]; # Only loaded if programs.neovim.extraConfig is set
  #   viAlias = true;
  #   vimAlias = true;
  #   vimdiffAlias = true;
  # };


}
