# FZF configuration

{ ... }:

{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;

    defaultCommand = "fd --type f --hidden --exclude .git";

    fileWidget = {
      command = "fd --type f --hidden --exclude .git";
    };

    changeDirWidget = {
      command = "fd --type d --hidden --exclude .git";
    };

    historyWidget = {
      options = [ "--sort --exact" ];
    };
  };
}
