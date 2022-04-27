{ config, pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./nvim.nix
    ./tmux.nix
    ./clisp.nix
  ];
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [

    # c/c++
    cmake
    cdecl
    astyle

    # nodejs
    nodejs
    nodePackages.typescript
    
    # rust
    rustc

    # haskell
    ghc

    # tex
    texlive.combined.scheme-full

    # tools
    jump
    exa
    ripgrep
    aria
    ranger

    comma

    openvpn
  ];

  programs.git = {
    enable = true;
    userName = "dezzw";
    userEmail = "dw@dezzw.com";
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
      ls = "exa -la";
      lt = "exa -laT";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "zsh-users/zsh-syntax-highlighting"; }
        { name = "jeffreytse/zsh-vi-mode"; }
        # { name = "spwhitt/nix-zsh-completions"; }
        { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
        # { name = "marlonrichert/zsh-autocomplete"; }
      ];
    };
    initExtra = ''
    
     . $HOME/.p10k.zsh
     . $HOME/.dotfiles/Shells/emacs-cmds.sh
     . $HOME/.dotfiles/Shells/doom-emacs-cmds.sh
     eval "$(jump shell)"
     eval "$(direnv hook zsh)"

     if [[ "$TERM" == "dumb" ]]
     then
         unsetopt zle
         unsetopt prompt_cr
         unsetopt prompt_subst
         if whence -w precmd >/dev/null; then
             unfunction precmd
         fi
         if whence -w preexec >/dev/null; then
             unfunction preexec
         fi
         PS1='$ '
    fi
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
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp exts.pass-update exts.pass-import ]);
  };

  programs.browserpass = {
    enable = true;
  };
}
