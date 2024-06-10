{ inputs, config, pkgs, username, lib, ... }:
let
  defaultPkgs = with pkgs;
    [
      # filesystem
      fd
      ripgrep
      curl
      tree
      
      # compression
      unzip
      gzip
      xz
      zip

      jq

      # c/c++
      cmake
      astyle

      # nodejs
      nodejs
      deno
      typescript

      lua

      # php

      # java
      zulu

      # clj
      clojure
      leiningen

      # tex
      texlive.combined.scheme-full

      aria # cli downloader

      comma

      # misc
      neofetch # display key software/version info in term
    ] ++ lib.optionals pkgs.stdenv.isDarwin [ yabai ];

  guiPkgs = with pkgs;
    [ ] ++ lib.optionals pkgs.stdenv.isDarwin
    [ utm ]; # utm is a qemu wrapper for mac only
in {
  programs.home-manager.enable = true;
  home.enableNixpkgsReleaseCheck = false;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";
  home.packages = defaultPkgs ++ guiPkgs;

  imports = [ ../emacs
              ../neovim
              ../helix
            ];

  home.sessionVariables = {
    #TERM = "xterm-256color";
    
    # EDITOR = "nvim";
    # VISUAL = "nvim";
    # GIT_EDITOR = "nvim";
    # Add colors to man pages
    MANPAGER = "less -R --use-color -Dd+r -Du+b +Gg -M -s";
    SYSTEMD_COLORS = "true";
    COLORTERM = "truecolor";
    FZF_CTRL_R_OPTS = "--sort --exact";
    ENCHANT_CONFIG_DIR = "$HOME/.config/enchant";
  };

  home.file.".direnvrc".text = ''
    source ~/.config/direnv/direnvrc
  '';
  
  home.file.".p10k.zsh".source = ./dotfiles/p10k.zsh;

  programs.bat = {
    enable = true;
    #extraPackages = with pkgs.bat-extras; [ batman batgrep ];
    config = {
      italic-text = "always";
      style = "plain"; # no line numbers, git status, etc... more like cat with colors
    };
  };

  programs.nix-index.enable = true;
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;
    defaultCommand = "fd --type f --hidden --exclude .git";
    fileWidgetCommand = "fd --type f"; # for when ctrl-t is pressed
  };
  programs.ssh = {
    enable = true;
    compression = true;
    controlMaster = "auto";
    includes = [ "*.conf" "~/.orbstack/ssh/config" ];
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
      save = 10000; # save 10,000 lines of history
    };

    defaultKeymap = "emacs";
    # things to add to .zshenv
    envExtra = ''
      # don't use global env as it will slow us down
      skip_global_compinit=1
    '';
    #initExtraBeforeCompInit = "";
    completionInit = ''
      # only update compinit once each day
      # better solution would be to pre-build zcompdump with compinit call then link it in
      # and never recalculate
      autoload -Uz compinit
      for dump in ~/.zcompdump(N.mh+24); do
        compinit
      done
      compinit -C
    '';
    initExtraFirst = ''
      source ${./dotfiles/p10k.zsh}
    '';
    initExtra = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]] \
        && [[ -n ''${EMACS_VTERM_PATH} ]] \
        && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
        source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
      fi

      vterm_printf() {
        if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ]); then
           # Tell tmux to pass the escape sequences through
           printf "\ePtmux;\e\e]%s\007\e\\" "$1"
        elif [ "''${TERM%%-*}" = "screen" ]; then
           # GNU screen (screen, screen-256color, screen-256color-bce)
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

  programs.eza = {
    enable = true;
    git = true;
  };

  programs.git = {
    enable = true;
    userName = "dezzw";
    userEmail = "dw@dezzw.com";
    delta = {
      enable = true;
      options = {
        features = "decorations";

        interactive = { keep-plus-minus-markers = false; };

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

  programs.gpg = { enable = true; };

  programs.password-store = {
    enable = true;
    package =
      pkgs.pass.withExtensions (exts: with exts; [ pass-otp pass-update ]);
    settings = { PASSWORD_STORE_DIR = "$HOME/.password-store"; };
  };

  programs.browserpass = { enable = true; };

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    shell = "${pkgs.zsh}/bin/zsh";
    historyLimit = 10000;
    escapeTime = 0;
    extraConfig = builtins.readFile ./dotfiles/tmux.conf;
    sensibleOnTop = true;
    plugins = with pkgs; [
      tmuxPlugins.sensible
      tmuxPlugins.open
      {
        plugin = tmuxPlugins.fzf-tmux-url;
        # default key bind is ctrl-b, u
        extraConfig = ''
          set -g @fzf-url-history-limit '2000'
          set -g @open-S 'https://www.duckduckgo.com/'
        '';
      }
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = ''
          set -g @resurrect-strategy-nvim 'session'
          set -g @resurrect-processes ': all:'
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
    ];
  };

  programs.zellij = {
    enable = true;
    enableZshIntegration = false;
    settings = {
      # simplified_ui = true;
      pane_frames = true; # pane 之间的边框
      theme = "nord";
    };
  };
    
  programs.alacritty = {
    enable = true;
    package =
      pkgs.alacritty; # switching to unstable so i get 0.11 with undercurl support
    settings = {
      window = {
        decorations = "Buttonless";
        dynamic_title = true;
        opacity = 0.7;
        blur = true;
	option_as_alt = "Both";
      };
      scrolling.history = 3000;
      font = {
	normal = {
	  family = "FiraCode Nerd Font Mono";
	  style = "Regular";
	};
	bold.style = "Bold";
	italic.style = "Italic";
	bold_italic.style = "Bold Italic";
	size = if pkgs.stdenvNoCC.isDarwin then 14 else 9;
      };
      shell.program = "${pkgs.zsh}/bin/zsh";
      live_config_reload = true;
      cursor.vi_mode_style = "Underline";
      colors.draw_bold_text_with_bright_colors = true;
      keyboard.bindings = [
      ];
    };
  };
}
