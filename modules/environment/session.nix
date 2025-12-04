# Session environment variables

{ pkgs, lib, ... }:

{
  home.sessionVariables = {
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
}