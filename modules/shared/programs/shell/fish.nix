# Fish shell configuration

{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellInit = ''
      # source ${config.home.homeDirectory}/.dotfiles/modules/home-manager/dotfiles/emacs.fish
    '';
    shellAbbrs = {
      cd = "z";
      cat = "bat";
    };
  };

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };
}