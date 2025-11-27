# FZF configuration

{ config, ... }:

{
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;
    defaultCommand = "fd --type f --hidden --exclude .git";
    fileWidgetCommand = "fd --type f";
  };
}