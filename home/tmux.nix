{pkgs, ...}:

{
  programs.tmux = {
    enable = true;
    escapeTime = 10;
    terminal = "screen-256color";
    extraConfig = ''
      set-option -sa terminal-overrides ',XXX:RGB'
    '';
  };
}
