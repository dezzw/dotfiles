{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [

    nixUnstable

    # c/c++
    cmake

    # nodejs
    nodejs
    nodePackages.typescript

    rustc
    ghc
    sbcl
    
    # tools
    jump
    exa
    stow
    ripgrep
    aria

    # tex
    texlive.combined.scheme-full

    browserpass
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
      pip = "pip3";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "zsh-users/zsh-autosuggestions"; } # Simple plugin installation
	      { name = "zsh-users/zsh-syntax-highlighting"; }
        # { name = "spwhitt/nix-zsh-completions"; }
	      { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
              { name = "marlonrichert/zsh-autocomplete";}
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

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  programs.gpg = {
    enable = true;
  };

  programs.password-store = {
    enable = true;
  };
}
