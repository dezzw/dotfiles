{ pkgs, ... }:

{
  programs.neovim = {
    enable = false;
    package = pkgs.neovim-nightly;
    coc = {
      enable = true;
    };
  };
}
