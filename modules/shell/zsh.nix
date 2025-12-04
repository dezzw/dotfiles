# Zsh shell configuration

{ config, pkgs, ... }:

{
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

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
}