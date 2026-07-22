# Session environment variables

{ pkgs, lib, ... }:

{
  home.sessionVariables = {
    TERM = "xterm-256color";
    COLORTERM = "truecolor";
    SYSTEMD_COLORS = "true";

    # Editor settings
    EDITOR = "nvim";
    VISUAL = "nvim";
    GIT_EDITOR = "nvim";

    # Add colors to man pages
    MANPAGER = "less -R --use-color -Dd+r -Du+b +Gg -M -s";
    ENCHANT_CONFIG_DIR = "$HOME/.config/enchant";
  }
  // lib.optionalAttrs pkgs.stdenv.isLinux {
    # Linux-specific variables
    XDG_CURRENT_DESKTOP = "WSLG";
  };
}