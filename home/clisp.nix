{ pkgs, ... }:

{
  home.packages = with pkgs; [
    sbcl
    roswell
    ncurses
  ];
}
