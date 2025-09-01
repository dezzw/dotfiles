{ pkgs, ... }:

{
  programs.helix = {
    enable = false;
  };

  home.packages = with pkgs; [
    steel
  ];
}
