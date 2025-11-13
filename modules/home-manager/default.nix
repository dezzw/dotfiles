{
  inputs,
  config,
  pkgs,
  username,
  lib,
  homeDirectory ? if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}",
  ...
}:
let
  fontPackages = import ../common/fonts.nix pkgs;

  commonPkgs = with pkgs; [
    # filesystem
    fd
    ripgrep
    curl
    tree
    htop
    git
    fzf

    # compression
    unzip
    gzip
    xz
    zip

    jq

    # nodejs
    nodejs

    # lua
    lua5_4_compat

    # python
    pipx

    # java
    zulu

    # clj
    clojure
    leiningen
    babashka

    # tex
    # texliveFull
    # texliveMedium

    # AI clients
    # aider-chat # outdated using brew for now
    claude-code
    codex
    claude-code-acp
    codex-acp
    cursor-agent

    comma

    cachix

    # misc
    tig
    serpl
    lazysql
    slumber

    just
  ];

  darwinPkgs = with pkgs; [ ];

  linuxPkgs = with pkgs; fontPackages;

  allPkgs =
    commonPkgs
    ++ (lib.optionals pkgs.stdenv.isDarwin darwinPkgs)
    ++ (lib.optionals pkgs.stdenv.isLinux linuxPkgs);
in
{
  programs.home-manager.enable = true;
  home.enableNixpkgsReleaseCheck = lib.mkDefault false;

  home = {
    inherit username homeDirectory;
    stateVersion = "25.11";
    packages = allPkgs;

    sessionVariables = {
      TERM = "xterm-256color";
      COLORTERM = "truecolor";
      SYSTEMD_COLORS = "true";
      FZF_CTRL_R_OPTS = "--sort --exact";

      # Editor settings
      EDITOR = "hx";
      VISUAL = "hx";
      GIT_EDITOR = "hx";

      # Add colors to man pages
      MANPAGER = "less -R --use-color -Dd+r -Du+b +Gg -M -s";
      ENCHANT_CONFIG_DIR = "$HOME/.config/enchant";
    }
    // lib.optionalAttrs pkgs.stdenv.isLinux {
      # Linux-specific variables
      XDG_CURRENT_DESKTOP = "WSLG";
    };

    sessionPath = [
      "$HOME/.cargo/bin"
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      "/Applications/Xcode.app/Contents/Developer/usr/bin/"
      "/Applications/Emacs.app/Contents/MacOS/bin/"
    ];

    file = lib.optionalAttrs pkgs.stdenv.isDarwin {
      ".config/wezterm/" = {
        source = ./dotfiles/wezterm;
        recursive = true;
      };
    };
  };

  imports = [
    ./programs
    ../emacs
    ../neovim
    # ../helix
    # ../alacritty
    # ../ghostty
    ../zellij
  ];

  # Linux-specific: Enable font configuration
  fonts.fontconfig.enable = lib.mkIf pkgs.stdenv.isLinux true;

  # Linux-specific: Bash configuration with work environment
  programs.bash = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    shellAliases = {
      ls = "eza";
      ll = "eza -l";
      la = "eza -a";
      cd = "z";
      python = "python3";
    };

    bashrcExtra = ''
      # Work environment variables
      export XTENSA_SYSTEM=/opt/xtensa/registry
      export LD_LIBRARY_PATH=/usr/local/lib
      export LM_LICENSE_FILE=27001@10.0.10.168
      export XTENSA_CORE=quadra_cpu_prod
      export PATH="$PATH:/opt/xtensa/XtDevTools/install/tools/RI-2019.1-linux/XtensaTools/bin/"
      export PATH="$PATH:$HOME/.cargo/bin/"
      export USER="desmond.wang"
      export LOGNAME="desmond.wang"
    '';
  };
}
