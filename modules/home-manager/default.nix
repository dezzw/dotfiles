{
  inputs,
  config,
  pkgs,
  username,
  lib,
  ...
}:
let
  defaultPkgs =
    with pkgs;
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

      # AI client
      # aider-chat # outdated using brew for now

      comma

      cachix

      # misc

      # helix
      tig
      serpl
      lazysql
      slumber

      just
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [ ];

  guiPkgs = with pkgs; [ ] ++ lib.optionals pkgs.stdenv.isDarwin [ ]; # utm is a qemu wrapper for mac only
in
{
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

  imports = [
    ../emacs
    ../neovim
    ./programs
    # ../helix
    # ../alacritty
    # ../ghostty
  ];

  home.sessionVariables = {
    TERM = "xterm-256color";

    EDITOR = "nvim";
    VISUAL = "nvim";
    GIT_EDITOR = "nvim";
    # Add colors to man pages
    MANPAGER = "less -R --use-color -Dd+r -Du+b +Gg -M -s";
    SYSTEMD_COLORS = "true";
    COLORTERM = "truecolor";
    FZF_CTRL_R_OPTS = "--sort --exact";
    ENCHANT_CONFIG_DIR = "$HOME/.config/enchant";
  };

  home.sessionPath = [
    "$HOME/.cargo/bin"
    "/Applications/Xcode.app/Contents/Developer/usr/bin/"
    "/Applications/Emacs.app/Contents/MacOS/bin/"
  ];

  home.file = {
    ".config/wezterm/" = {
      source = ./dotfiles/wezterm;
      recursive = true;
    };
  };
}
