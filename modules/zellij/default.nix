{ pkgs, ... }:

{
  programs.zellij = {
    enable = true;
    enableBashIntegration = true;

    settings = {
      theme = "onedark";
      show_startup_tips = false;
      default_mode = "locked";
    };
  };
}
