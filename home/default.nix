{ config, pkgs, ... }:

{  
  imports = [
    ./emacs
    ./nvim.nix
    ./tmux.nix
    ./clisp.nix
    ./alacritty.nix
  ];
  
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home =
    let NODE_GLOBAL = "${config.home.homeDirectory}/.node-packages";
    in
      {
        # This value determines the Home Manager release that your
        # configuration is compatible with. This helps avoid breakage
        # when a new Home Manager release introduces backwards
        # incompatible changes.
        #
        # You can update Home Manager without changing this value. See
        # the Home Manager release notes for a list of state version
        # changes in each release.
        stateVersion = "22.05";
        sessionVariables = {
          # GPG_TTY = "/dev/ttys000";
          EDITOR = "nvim";
          VISUAL = "nvim";
          # CLICOLOR = 1;
          LSCOLORS = "ExFxBxDxCxegedabagacad";
          NODE_PATH = "${NODE_GLOBAL}/lib";
          # HOMEBREW_NO_AUTO_UPDATE = 1;
          ASPELL_CONF = "conf ${config.xdg.configHome}/aspell/config;";
        };
        sessionPath = [ "${NODE_GLOBAL}/bin" ];

        packages = with pkgs; [

          coreutils

          # c/c++
          cmake
          cdecl
          astyle

          # nodejs
          nodejs
          nodePackages.npm
          nodePackages.coffee-script
          nodePackages.typescript
          flow
          yarn

          jq

          deno
          
          # rust
          rustup
          
          # racket
          # racket-minimal

          # tex
          texlive.combined.scheme-medium

          # tools
          exa
          zoxide
          ripgrep
          aria
          ranger
          fd
          
          comma

          openvpn
        ];
      };
  
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
      cd = "z";
    };
    
    # zplug = {
    #   enable = true;
    #   plugins = [
    #     # { name = "zsh-users/zsh-autosuggestions"; }
    #     { name = "zdharma/fast-syntax-highlighting"; tags = [ defer:2 ]; }
    #     # { name = "spwhitt/nix-zsh-completions"; }
    #     { name = "romkatv/powerlevel10k"; tags = [ as:theme depth:1 ]; }
    #     { name = "hlissner/zsh-autopair"; tags = [ defer:2 ]; }
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
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.5.0";
          sha256 = "0za4aiwwrlawnia4f29msk822rj9bgcygw6a8a6iikiwzjjz0g91";
        };
      }
    ];

    initExtra = ''
     . $HOME/.p10k.zsh
     . $HOME/.dotfiles/Shells/emacs-cmds.sh

     eval "$(zoxide init zsh)"

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
      if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
          # Tell tmux to pass the escape sequences through
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
  
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
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
    package = pkgs.pass.withExtensions (exts: with exts; [
      pass-otp
      pass-update
    ]);
    settings = {
      PASSWORD_STORE_DIR = "$HOME/.password-store";
    };
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
