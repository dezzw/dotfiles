{ config, pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./nvim.nix
    ./tmux.nix
    ./clisp.nix
    # ./alacritty.nix
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
    nodePackages.npm
    nodePackages.coffee-script
    nodePackages.typescript
    yarn
    node2nix
    
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
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    shellAliases = {
      ls = "exa -la";
      lt = "exa -laT";
    };
    # zplug = {
    #   enable = true;
    #   plugins = [
    #     # { name = "zsh-users/zsh-autosuggestions"; }
    #     # { name = "zsh-users/zsh-syntax-highlighting"; }
    #     { name = "z-shell/F-Sy-H"; }
    #     { name = "jeffreytse/zsh-vi-mode"; }
    #     # { name = "spwhitt/nix-zsh-completions"; }
    #     { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
    #     # { name = "marlonrichert/zsh-autocomplete"; }
    #     # { name = "hlissner/zsh-autopair"; }
    #   ];
    # };

    # oh-my-zsh.enable = true;
    
    plugins = [
      {
        name = "powerlevel10k";
        file = "powerlevel10k.zsh-theme";
        src = pkgs.fetchFromGitHub {
          owner = "romkatv";
          repo = "powerlevel10k";
          rev = "v1.16.1";
          sha256 = "sha256-DLiKH12oqaaVChRqY0Q5oxVjziZdW/PfnRW1fCSCbjo=";
        };
      }
      {
          name = "autopair";
          file = "autopair.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "hlissner";
            repo = "zsh-autopair";
            rev = "4039bf142ac6d264decc1eb7937a11b292e65e24";
            sha256 = "02pf87aiyglwwg7asm8mnbf9b2bcm82pyi1cj50yj74z4kwil6d1";
          };
      }
      {
        name = "fast-syntax-highlighting";
        file = "fast-syntax-highlighting.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma";
          repo = "fast-syntax-highlighting";
          rev = "v1.28";
          sha256 = "106s7k9n7ssmgybh0kvdb8359f3rz60gfvxjxnxb4fg5gf1fs088";
        };
      }
    ];

    initExtra = ''
     . $HOME/.p10k.zsh
     . $HOME/.dotfiles/Shells/emacs-cmds.sh
     eval "$(jump shell)"

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

    vterm_printf(){
              if [ -n "$TMUX" ]; then
                  # Tell tmux to pass the escape sequences through
                  # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                  printf "\ePtmux;\e\e]%s\007\e\\" "$1"
              elif [ "''${TERM%%-*}" = "screen" ]; then
                  # GNU screen (screen, screen-256color, screen-256color-bce)
                  printf "\eP\e]%s\007\e\\" "$1"
              else
                  printf "\e]%s\e\\" "$1"
              fi
        }
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

  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
