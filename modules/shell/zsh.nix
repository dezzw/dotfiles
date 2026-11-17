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
      if [[ "$INSIDE_EMACS" = 'ghostel' ]]; then
          # Open a file in Emacs from the terminal
          e()   { ghostel_cmd find-file-other-window "$@"; }

          # Open dired in another window
          dow() { ghostel_cmd dired-other-window "$@"; }

          # Open magit for the current directory
          gst() { ghostel_cmd magit-status-setup-buffer "$(pwd)"; }

	  cdc() { ghostel_cmd cd-workspace && clear }
      fi
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
      python = "python3";
    };
  };
}
