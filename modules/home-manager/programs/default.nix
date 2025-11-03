{
  pkgs,
  lib,
  config,
  ...
}:
{
  programs.bat = {
    enable = true;
    config = {
      italic-text = "always";
      style = "plain";
    };
  };

  programs.nix-index.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;
    defaultCommand = "fd --type f --hidden --exclude .git";
    fileWidgetCommand = "fd --type f";
  };

  programs.codex.enable = true;
  programs.claude-code.enable = true;

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*" = {
      forwardAgent = false;
      addKeysToAgent = "no";
      compression = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
    };
    includes =
      [ "*.conf" ]
      ++ lib.optionals pkgs.stdenv.isDarwin [ "~/.orbstack/ssh/config" ];
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    history = {
      expireDuplicatesFirst = true;
      ignoreSpace = true;
      save = 10000;
    };

    defaultKeymap = "emacs";
    envExtra = ''
      # don't use global env as it will slow us down
      skip_global_compinit=1
    '';
    completionInit = ''
      autoload -Uz compinit
      for dump in ~/.zcompdump(N.mh+24); do
        compinit
      done
      compinit -C
    '';
    initContent = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]] \
        && [[ -n ''${EMACS_VTERM_PATH} ]] \
        && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
        source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
      fi

      vterm_printf() {
        if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ]); then
           printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "''${TERM%%-*}" = "screen" ]; then
           printf "\eP\e]%s\007\e\\" "$1"
        else
           printf "\e]%s\e\\" "$1"
        fi
      }

      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
        alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi

      vterm_cmd() {
        local vterm_elisp
        vterm_elisp=""
        while [ $# -gt 0 ]; do
          vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
          shift
        done
        vterm_printf "51;E$vterm_elisp"
      }

      find_file() {
        vterm_cmd find-file "$(realpath "''${@:-.}")"
      }

      [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
      source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    '';
    sessionVariables = { };
    plugins = [
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
    shellAliases = {
      cd = "z";
      cat = "bat";
    };
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      # source ${config.home.homeDirectory}/.dotfiles/modules/home-manager/dotfiles/emacs.fish
    '';
    shellAbbrs = {
      cd = "z";
      cat = "bat";
    };
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  programs.eza = {
    enable = true;
    git = true;
  };

  programs.git = {
    enable = true;
    settings = {
      user.name = "dezzw";
      user.email = "dw@dezzw.com";
    };
  };

  programs.delta = {
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

  programs.gpg.enable = true;

  programs.zellij.enable = true;

  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    settings = {
      manager = {
        show_hidden = true;
        sort_by = "modified";
        sort_dir_first = true;
      };
    };
  };

  programs.lazygit.enable = true;
}
